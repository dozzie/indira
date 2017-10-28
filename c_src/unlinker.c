//----------------------------------------------------------------------------
//
// Program to clean up stale unix sockets in the case of Erlang VM crash.
//
// Sockets are identified by their device and inode from stat(2), provided by
// port owner process.
//
//----------------------------------------------------------------------------

#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <limits.h>
#include <errno.h>

#include <signal.h>

//----------------------------------------------------------------------------
// signal handling {{{

static
void set_signal_handler(int signum)
{
  struct sigaction handler;
  memset(&handler, 0, sizeof(handler));
  handler.sa_handler = SIG_IGN;
  sigaction(signum, &handler, NULL);
}

// }}}
//----------------------------------------------------------------------------
// read_exact() {{{

static
ssize_t read_exact(int fd, unsigned char *buffer, size_t len)
{
  ssize_t result;
  size_t left_to_read = len;

  while (left_to_read > 0) {
    if ((result = read(fd, buffer, left_to_read)) > 0) {
      buffer += result;
      left_to_read -= result;
    } else if (result == 0 && left_to_read == len) {
      // EOF at the beginning
      return 0;
    } else if (result == 0 || errno != EINTR) {
      // unexpected EOF or fatal read error
      return -1;
    }
  }

  return len;
}

// }}}
//----------------------------------------------------------------------------
// int unpacking {{{

static
uint16_t unpack16(const unsigned char *buf)
{
  return (uint32_t)buf[0] << (8 * 1)
       | (uint32_t)buf[1] << (8 * 0);
}

static
uint32_t unpack32(const unsigned char *buf)
{
  return (uint32_t)buf[0] << (8 * 3)
       | (uint32_t)buf[1] << (8 * 2)
       | (uint32_t)buf[2] << (8 * 1)
       | (uint32_t)buf[3] << (8 * 0);
}

static
uint64_t unpack64(const unsigned char *buf)
{
  return (uint64_t)buf[0] << (8 * 7)
       | (uint64_t)buf[1] << (8 * 6)
       | (uint64_t)buf[2] << (8 * 5)
       | (uint64_t)buf[3] << (8 * 4)
       | (uint64_t)buf[4] << (8 * 3)
       | (uint64_t)buf[5] << (8 * 2)
       | (uint64_t)buf[6] << (8 * 1)
       | (uint64_t)buf[7] << (8 * 0);
}

// }}}
//----------------------------------------------------------------------------

typedef enum {
  eof, path_add, path_forget, command_error, read_error
} command_t;

struct socket_path {
  dev_t device;
  ino_t inode;
  char *path;
};

//----------------------------------------------------------------------------
// read_command() {{{

static
command_t read_command(int fd, struct socket_path *element)
{
  unsigned char buffer[2 * sizeof(uint64_t) + 2 * PATH_MAX];
  ssize_t result;
  size_t msg_len;

  // read message length
  if ((result = read_exact(fd, buffer, 2)) == 0)
    return eof;
  else if (result < 0)
    return read_error;
  if ((msg_len = unpack16(buffer)) < sizeof(char) + 2 * sizeof(uint64_t))
    return command_error;

  // read command (add or forget a path)
  unsigned char command;
  if (read_exact(fd, &command, sizeof(command)) <= 0)
    return read_error;
  --msg_len;

  if (command == '+') { // add path
    // this command carries device+inode and a non-empty path (it's composed
    // of getcwd() and AF_UNIX address, so two paths joined)
    if (msg_len > 2 * sizeof(uint64_t) + 2 * PATH_MAX ||
        msg_len < 2 * sizeof(uint64_t) + 1)
      return command_error;

    if (read_exact(fd, buffer, msg_len) <= 0)
      return read_error;

    element->device = unpack64(buffer);
    element->inode = unpack64(buffer + sizeof(uint64_t));

    msg_len -= 2 * sizeof(uint64_t);
    element->path = malloc(msg_len + 1);
    memcpy(element->path, buffer + 2 * sizeof(uint64_t), msg_len);
    element->path[msg_len] = 0;

    return path_add;
  } else if (command == '-') { // forget path
    // this command carries just device+inode
    if (msg_len != 2 * sizeof(uint64_t))
      return command_error;

    if (read_exact(fd, buffer, msg_len) <= 0)
      return read_error;

    element->device = unpack64(buffer);
    element->inode = unpack64(buffer + sizeof(uint64_t));
    element->path = NULL;

    return path_forget;
  }

  return command_error;
}

// }}}
//----------------------------------------------------------------------------

int main(void)
{
  // This is a list of paths of listening AF_UNIX sockets. As such, it's
  // expected to be (a) short and (b) rarely changing.
  //
  // (Usually a system will have many client sockets for every listening
  // socket, and a typical ulimit for number of file descriptors is around
  // 1024 or 4096, so just a fraction of that number will be for listening
  // sockets. Erlang system will have even less of those, because it's usually
  // used for network communication.)
  //
  // Given this, a naive linear search should be fast enough for removing
  // properly closed sockets.

  struct socket_path *elements;
  size_t nelems = 0;
  size_t max_elems;

  unsigned char buffer[4];
  if (read_exact(0, buffer, 2) <= 0)
    return 1;
  if (unpack16(buffer) != 4)
    return 1;
  if (read_exact(0, buffer, 4) <= 0)
    return 1;
  max_elems = unpack32(buffer);
  if (max_elems == 0)
    return 1;
  elements = calloc(max_elems, sizeof(struct socket_path));

  set_signal_handler(SIGHUP);
  set_signal_handler(SIGINT);
  set_signal_handler(SIGTERM);

  struct socket_path sock;
  command_t command;

  while (1) {
    switch ((command = read_command(0, &sock))) {
      case eof:
      case command_error:
      case read_error:
        goto CLEANUP;
      break;

      case path_add:
        // XXX: this should never happen, since the maximum number of
        // registered sockets is supposed to come from Erlang's maximum number
        // of ports
        if (nelems == max_elems)
          abort();

        elements[nelems].device = sock.device;
        elements[nelems].inode = sock.inode;
        elements[nelems].path = sock.path;
        ++nelems;
      break;

      case path_forget:
        for (size_t i = 0; i < nelems; ++i) {
          if (elements[i].device == sock.device &&
              elements[i].inode == sock.inode) {
            --nelems;
            free(elements[i].path);
            if (i < nelems) {
              elements[i].device = elements[nelems].device;
              elements[i].inode = elements[nelems].inode;
              elements[i].path = elements[nelems].path;
            }
            elements[nelems].device = 0;
            elements[nelems].inode = 0;
            elements[nelems].path = NULL;

            break;
          }
        }
      break;
    }
  }

CLEANUP:
  if (command == command_error)
    // crash, but don't unlink the sockets (Erlang VM is still running)
    return 1;

  struct stat file_info;
  for (size_t i = 0; i < nelems; ++i) {
    if (stat(elements[i].path, &file_info) == 0 &&
        (file_info.st_mode & S_IFSOCK) == S_IFSOCK &&
        file_info.st_dev == elements[i].device &&
        file_info.st_ino == elements[i].inode)
      unlink(elements[i].path);

    free(elements[i].path);
  }

  free(elements);

  return (command == eof) ? 0 : 1; // eof or read_error
}

//----------------------------------------------------------------------------
// vim:ft=c:foldmethod=marker
