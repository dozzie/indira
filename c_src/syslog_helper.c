#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <signal.h>
#include <unistd.h>

int try_stream(char *path);
int try_dgram(char *path);

// usage:
//   syslog_helper ident facility
//   syslog_helper ident
//   syslog_helper
int main(int argc, char **argv)
{
  if (argc == 1 || !strcmp(argv[1], "-h") || !strcmp(argv[1], "--help")) {
    printf("Usage: %s /dev/log\n", argv[0]);
    return 0;
  }

  char *path = argv[1];

  signal(SIGPIPE, SIG_IGN); // ignore SIGPIPE, it will be handled manually

  int syslog = -1;
  char buffer[16 * 1024]; // way, way too big for syslog

  while (fgets(buffer, sizeof(buffer), stdin) != NULL) {
    if (syslog == -1) // try reconnect as stream socket
      syslog = try_stream(path);
    if (syslog == -1) // try reconnect as datagram socket
      syslog = try_dgram(path);
    if (syslog == -1) // skip the line
      continue;

    char *pos = strchr(buffer, '\n');
    if (pos != NULL)
      *pos = '\0';

    if (write(syslog, buffer, strlen(buffer)) == -1) {
      close(syslog);
      syslog = -1;
    }
  }

  return 0;
}

int try_stream(char *path)
{
  int syslog = socket(AF_UNIX, SOCK_STREAM, 0);

  struct sockaddr_un addr;
  addr.sun_family = AF_UNIX;
  strncpy(addr.sun_path, path, sizeof(addr.sun_path));

  if (connect(syslog, (struct sockaddr *)&addr, sizeof(addr)) == 0)
    return syslog;

  close(syslog);
  return -1;
}

int try_dgram(char *path)
{
  int syslog = socket(AF_UNIX, SOCK_DGRAM, 0);

  struct sockaddr_un addr;
  addr.sun_family = AF_UNIX;
  strncpy(addr.sun_path, path, sizeof(addr.sun_path));

  if (connect(syslog, (struct sockaddr *)&addr, sizeof(addr)) == 0)
    return syslog;

  close(syslog);
  return -1;
}
