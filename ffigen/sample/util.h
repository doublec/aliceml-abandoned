
#include <stdio.h>
#include <gtk/gtk.h>

void printhello();

char add_char(char a, char b);
short add_short(short a, short b);
long add_long(long a, long b);
long long add_longlong(long long a, long long b);
int add_int(int a, int b);

float add_float(float a, float b);
double add_double(double a, double b);
long double add_longdouble(long double a, long double b);

int add_array(char a[4]);

typedef enum { const_a,const_b,const_c,const_d } myenum;


struct vec {
  int x; int y;
  myenum a;
};

struct vec* add_vec(struct vec* x, struct vec* y);

myenum dummy(myenum p);

void mymemcpy(void* dst, const void* src, int length);
void mymemset(void* dst, int length, char chr);

void inc(int* i,...);

int list_add(GList* lst);

typedef void (*voidtovoid)(void);
voidtovoid get_printhello();
void call_fun(voidtovoid f);
