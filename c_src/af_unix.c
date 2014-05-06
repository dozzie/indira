//----------------------------------------------------------------------------
// preamble

//----------------------------------------------------------
// sockets and poll {{{

#include <string.h>
#include <stdint.h>
#include <unistd.h>
#include <poll.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#include <limits.h>
#include <stdlib.h> // calloc(), free()

// }}}
//----------------------------------------------------------
// Erlang port driver {{{

#include <erl_driver.h>

// }}}
//----------------------------------------------------------
// definitions {{{

#define MAX_POLL_SOCKS 1024
#define MAX_BUFFER (64 * 1024)

#define PORT_DRIVER_NAME      "af_unix_drv"
#define PORT_DRIVER_NAME_SYM   af_unix_drv

#define PORT_DRIVER_NAME_LEN (sizeof(PORT_DRIVER_NAME) - 1)

#if ERL_DRV_EXTENDED_MAJOR_VERSION == 2
// >=E15B
typedef ErlDrvSizeT erl_size_t;
#else
// E14A
typedef int erl_size_t;
#endif

// }}}
//----------------------------------------------------------

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// basic socket operations {{{

void unix_srv_close(char *address, int lsock);

int unix_srv_listen(char *address, int type, uid_t uid, gid_t gid, mode_t mode)
{
  int lsock = socket(AF_UNIX, type, 0);
  struct sockaddr_un bind_addr;
  bind_addr.sun_family = AF_UNIX;
  strncpy(bind_addr.sun_path, address, sizeof(bind_addr.sun_path));

  int result = bind(lsock, (struct sockaddr *)&bind_addr, sizeof(bind_addr));
  if (result != 0)
    return -1;

  chmod(address, mode);
  // TODO: chown(address, uid, gid);

  result = listen(lsock, 1);
  if (result != 0) {
    int old_errno = errno;
    unix_srv_close(address, lsock);
    errno = old_errno;

    return -1;
  }

  return lsock;
}

void unix_srv_close(char *address, int lsock)
{
  close(lsock);
  unlink(address);
}

// }}}
//----------------------------------------------------------------------------
// poll sockets {{{

struct poll_set {
  struct pollfd socks[MAX_POLL_SOCKS];  // poll objects
  size_t count;                         // number of sockets registered
};

int poll_add(struct poll_set *set, int sock);

void poll_init(struct poll_set *set, int listen_sock)
{
  memset(set, 0, sizeof(*set));
  poll_add(set, listen_sock);
}

int poll_wait(struct poll_set *set, int timeout)
{
  int result = poll(set->socks, set->count, timeout);
  if (result < 0 && errno == EINTR)
    return 0; // it's not an error
  return result;
}

int poll_ready(struct poll_set *set, size_t position)
{
  return (set->socks[position].revents != 0);
}

int poll_fd(struct poll_set *set, size_t position)
{
  return set->socks[position].fd;
}

size_t poll_count(struct poll_set *set)
{
  return set->count;
}

ssize_t poll_find_fd(struct poll_set *set, int fd)
{
  size_t i;
  for (i = 0; i < set->count; ++i)
    if (set->socks[i].fd == fd)
      return i;
  return -1;
}

int poll_add(struct poll_set *set, int sock)
{
  if (set->count < MAX_POLL_SOCKS) {
    set->socks[set->count].fd = sock;
    set->socks[set->count].events = POLLIN;
    set->count++;
    return 0;
  } else {
    return -1;
  }
}

void poll_remove(struct poll_set *set, size_t position)
{
  if (position < set->count - 1) {
    // if not last FD, put the last one here
    set->socks[position].fd = set->socks[set->count - 1].fd;
    set->socks[position].events = set->socks[set->count - 1].events;
  }
  set->count--;
}

void poll_remove_fd(struct poll_set *set, int fd)
{
  ssize_t position = poll_find_fd(set, fd);
  if (position != -1)
    poll_remove(set, position);
}

// }}}
//----------------------------------------------------------------------------
// Erlang port driver API {{{

struct unix_sock_context {
  int initialized;
  struct poll_set sockets;
  char buffer[MAX_BUFFER]; // small optimization: don't allocate 64k every time
  char address[PATH_MAX];
  ErlDrvPort erl_port;
};

//----------------------------------------------------------
// entry point definition {{{

ErlDrvData unix_sock_driver_start(ErlDrvPort port, char *cmd);
void       unix_sock_driver_output(ErlDrvData drv_data, char *buf, erl_size_t len);
void       unix_sock_driver_stop(ErlDrvData drv_data);

ErlDrvEntry unix_sock_driver_entry = {
  NULL,       // int        init(void)
  unix_sock_driver_start,  // ErlDrvData start(ErlDrvPort port, char *cmd)
  unix_sock_driver_stop,   // void       stop(ErlDrvData drv_data)
  unix_sock_driver_output, // void       output(ErlDrvData drv_data, char *buf, int len) // Erlang has data for driver
  NULL,       // void       ready_input(ErlDrvData, ErlDrvEvent)
  NULL,       // void       ready_output(ErlDrvData, ErlDrvEvent)
  PORT_DRIVER_NAME, // <driver name>
  NULL,       // void       finish(void)
  NULL,       // <reserved>
  NULL,       // int        control(...) // port_control/3 handler
  NULL,       // void       timeout(ErlDrvData drv_data)
  NULL,       // void       outputv(ErlDrvData drv_data, ErlIOVec *ev) // Erlang has data for driver
  NULL,       // void       ready_async(ErlDrvData drv_data, ErlDrvThreadData thread_data)
  NULL,       // void       flush(ErlDrvData drv_data)
  NULL,       // int        call(ErlDrvData drv_data, unsigned int command, char *buf, int len, char **rbuf, int rlen, unsigned int *flags) // like control(), a synchronous call
  NULL,       // void       event(ErlDrvData drv_data, ErlDrvEvent event, ErlDrvEventData event_data)
  ERL_DRV_EXTENDED_MARKER,
  ERL_DRV_EXTENDED_MAJOR_VERSION,
  ERL_DRV_EXTENDED_MINOR_VERSION,
  ERL_DRV_FLAG_USE_PORT_LOCKING,  // XXX: driver flags
  NULL,       // <reserved>
  NULL,       // called when process monitor dies
  NULL        // called to close an event object
};

// the same as <driver name> in structure above, but as identifer instead of
// string
DRIVER_INIT(PORT_DRIVER_NAME_SYM)
{
  return &unix_sock_driver_entry;
}

// }}}
//----------------------------------------------------------
// Erlang port start {{{

ErlDrvData unix_sock_driver_start(ErlDrvPort port, char *cmd)
{
  struct unix_sock_context *context =
    calloc(1, sizeof(struct unix_sock_context));

  context->erl_port = port;

  return (ErlDrvData)context;
}

// }}}
//----------------------------------------------------------
// Erlang port stop {{{

void unix_sock_driver_stop(ErlDrvData drv_data)
{
  struct unix_sock_context *context = (struct unix_sock_context *)drv_data;
  if (!context->initialized)
    return;

  unix_srv_close(context->address, poll_fd(&context->sockets, 0));

  // NOTE: socket at position 0 is the listening one; it's already closed
  size_t i;
  for (i = 1; i < poll_count(&context->sockets); ++i)
    close(poll_fd(&context->sockets, i));

  free(context);
}

// }}}
//----------------------------------------------------------
// Erlang port operation {{{

int setup_socket(struct unix_sock_context *context, char *buf, int len)
{
  size_t addr_len = sizeof(context->address);
  memmove(context->address, buf, (addr_len <= len) ? (addr_len - 1) : len);
  context->address[addr_len - 1] = 0; // terminate with NIL byte

  int type = SOCK_STREAM;
  uid_t uid = 0;      // TODO: read from command buffer
  gid_t gid = 0;      // TODO: read from command buffer
  mode_t mode = 0660; // TODO: read from command buffer

  int lsock = unix_srv_listen(context->address, type, uid, gid, mode);
  if (lsock < 0)
    return -1;

  poll_init(&context->sockets, lsock);
  context->initialized = 1;

  return 0;
}

void unix_sock_driver_output(ErlDrvData drv_data, char *buf, erl_size_t len)
{
  struct unix_sock_context *context = (struct unix_sock_context *)drv_data;

  //   E               C
  // 'o' OPEN(f)   -----> '+' OK() | '!' ERROR(m)
  //
  // 'p' POLL(t)   -----> (replies: DATA, NEW, CLOSE; list ended with END)
  //               <----- 'n' NEW(n)
  //               <----- 'd' DATA(n,d)
  //               <----- 'c' CLOSE(n)
  // 'd' DATA(n,d) ----->
  // 'c' CLOSE(n)  ----->

  int timeout;
  int ready_fds;
  size_t i;
  int fd;

  switch (buf[0]) {
    case 'o': // OPEN(f)     <<"o", File/binary>>
      if (setup_socket(context, buf + 1, len - 1) == 0) {
        context->buffer[0] = '+';
        driver_output(context->erl_port, context->buffer, 1);
      } else {
        context->buffer[0] = '!';
        strerror_r(errno, context->buffer + 1, sizeof(context->buffer) - 1);
        driver_output(context->erl_port,
                      context->buffer, strlen(context->buffer));
      }
    break;

    case 'p': // POLL(t)     <<"p", Timeout:32>> (in ms, as for poll())
      timeout = (buf[1] << 24) + (buf[2] << 16) + (buf[3] <<  8) + buf[4];
      ready_fds = poll_wait(&context->sockets, timeout);

      if (poll_ready(&context->sockets, 0)) {
        ready_fds--;

        fd = accept(poll_fd(&context->sockets, 0), NULL, NULL);
        if (poll_add(&context->sockets, fd) == 0) {
          context->buffer[0] = 'n';
          context->buffer[1] = (fd & 0x0000ff00) >> 8;
          context->buffer[2] = (fd & 0x000000ff);
          driver_output(context->erl_port, context->buffer, 3);
        } else {
          close(fd);
        }
      }

      for (i = 1; ready_fds > 0; ++i) {
        if (!poll_ready(&context->sockets, i)) // not the FD we're looking for
          continue;
        ready_fds--;

        fd = poll_fd(&context->sockets, i);

        // reserve one byte for opcode ('d' or 'c') and two for FD
        ssize_t bufread = read(fd, context->buffer + 3,
                               sizeof(context->buffer) - 3);
        if (bufread > 0) {
          // some data to read; send it to port controller and continue
          context->buffer[0] = 'd';
          context->buffer[1] = (fd & 0x0000ff00) >> 8;
          context->buffer[2] = (fd & 0x000000ff);
          driver_output(context->erl_port, context->buffer, bufread + 3);
        } else {
          // EOF; close, remove from poll and continue
          close(fd);
          poll_remove(&context->sockets, i);
          context->buffer[0] = 'c';
          context->buffer[1] = (fd & 0x0000ff00) >> 8;
          context->buffer[2] = (fd & 0x000000ff);
          driver_output(context->erl_port, context->buffer, 3);
        }
      }
      context->buffer[0] = 'e';
      driver_output(context->erl_port, context->buffer, 1);
    break;

    case 'd': // DATA(n,d)   <<OpCode:8, FD:16, Payload/binary>>
      fd = (buf[1] << 8) + buf[2]; // big endian
      write(fd, buf + 3, len - 3);
    break;

    case 'c': // CLOSE(n)    <<OpCode:8, FD:16>>
      fd = (buf[1] << 8) + buf[2]; // big endian
      close(fd);
      poll_remove_fd(&context->sockets, fd);
    break;

    default:
      // TODO: raise error?
    break;
  }
}

// }}}
//----------------------------------------------------------

// }}}
//----------------------------------------------------------------------------
// vim:ft=c:foldmethod=marker:nowrap
