#include <stdarg.h>
#include <stdio.h>
#include <string.h>

void to_valist(va_list *args, void *dummy, ...) {
  va_start(*args, dummy);
}

void myprintf(const char *format, ...) {
  va_list args;
  va_start(args, format);
  vprintf(format, args);
  va_end(args);
}

int main(int argc, char *argv[]) {
  /*  va_list args;
  to_valist(&args, NULL, 1, 4, "Test");
  vprintf("%d - %d - %s\n", args);
  va_end(args);*/

  //  myprintf("%d - %d - %s\n", 1, 4, "Test");

  const void *a[10];

  a[0] = "a";
  a[1] = (const void *)4;
  vprintf("%s - %d\n", a);

  printf("%s - %d\n", a[0], a[1]);

  return 0;
}

