#include <stdio.h>

static char buf[1024];

char *readline() {
  setvbuf(stdin, NULL, _IONBF, 0);
  return fgets(buf, 1023, stdin);
}
