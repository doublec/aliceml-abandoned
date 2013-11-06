#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

void to_valist(va_list *args, void *dummy, ...) {
  va_start(*args, dummy);
}

void myprintf(const char *format, ...) {
  va_list args;
  va_start(args, format);
  vprintf(format, args);
  va_end(args);
}

typedef struct {
  int type;
  union {
    char* arg_string;
    int arg_int;
    double arg_double;
  };
} arg;

#define _GET_VARG(args, num) \
  (args[num].type == 2 ? args[num].arg_double : args[num].arg_int)

int main(int argc, char *argv[]) {
  /*  va_list args;
  to_valist(&args, NULL, 1, 4, "Test");
  vprintf("%d - %d - %s\n", args);
  va_end(args);*/

  //  myprintf("%d - %d - %s\n", 1, 4, "Test");

  char a[40];
  memset(a, 0, sizeof(a));

  char sa[10] = "xyz";
  char *s = sa;
  int i = 4;
  double f = 3.5;

  char* pos = (char *)a;
  memcpy(pos, &s, sizeof(char*));
  pos += (sizeof(char*));
  memcpy(pos, &i, sizeof(int));
  pos += (sizeof(int));
  memcpy(pos, &f, sizeof(double));
  vprintf("%s - %d - %f\n", a);

  //  printf("%s - %d - %f\n", a[0], a[1]);
  //  printf("%s - %d - %f\n", sa, i, f);//a[0], a[4], a[8]);
  double b[40];
  //  b[0] = (int)sa;
  //  b[1] = (int)i;
  //  b[2] = (int)f;
  memcpy(b, &i, sizeof(int));

  /*
  pos = (char*)b;
  memcpy(pos, &s, sizeof(char*));
  pos += (sizeof(double));
  memcpy(pos, &i, sizeof(int));
  pos += (sizeof(double));
  memcpy(pos, &f, sizeof(double));
  */
  /*
  arg b[10];
  b[0].arg_string = s; b[0].type = 0;
  b[1].arg_int = i;    b[1].type = 1;
  b[2].arg_double = f; b[2].type = 2;
  */
  //  printf("%s - %f\n", b[0].arg_int, (b[2].type == 2 ? (int)(b[2].arg_double) : b[2].arg_int));
  //  printf("%f\n", (false ? (int)(s) : *((char*)(&f))));

  //  printf("%s - %d - %f\n", b[0], b[1], b[2]);
  printf("%d\n", b[0]);
  //  printf("%s - %d - %f\n", *a);
  return 0;
}

