
#include <stdio.h>
#include <glib.h>

#include "util.h" 

// Einfaches Printhello
void printhello()
{
  printf("Hello World!\n");
}


// Addition auf Integern
char add_char(char a, char b) { return a + b; }
short add_short(short a, short b) { return a + b; }
long add_long(long a, long b) { return a + b; }
long long add_longlong(long long a, long long b) { return a + b; }
int add_int(int a, int b) { return a + b; }

// Addition auf Flieﬂkommazahlen
float add_float(float a, float b) { return a + b; }
double add_double(double a, double b) { return a + b; }
long double add_longdouble(long double a, long double b) { return a + b; }

// Addiert die Werte eines Arrays
int add_array(char a[4])
{
  int r = 0;
  for (int i=0; i<4; i++)
    r+=a[i];
  return r;
}

// Addiert zwei Vektoren
vec* add_vec(vec* a, vec* b)
{
  vec* v = new vec;
  v->x = a->x + b->x;
  v->y = a->y + b->y;
  return v;
}

// mymemcpy
void mymemcpy(void* dst, const void* src, int length)
{
  char* cdst = (char*) dst;
  char* csrc = (char*) src;

  while (length--)
    {
      cdst[0] = csrc[0];
      cdst++; csrc++;
    }
}

// mymemset
void mymemset(void* dst, int length, char chr)
{
  char* cdst = (char*) dst;

  while (length--)
    {
      cdst[0] = chr;
      cdst++; 
    }
}

// Inc

void inc(int* p)
{
  p[0]++;
}

// GList

int list_add(GList* lst)
{
  int s = 0;
  
  GList* frst = g_list_first(lst);
 
  for (int i=0; i<g_list_length(frst); i++) {
    int* data = (int*)g_list_nth_data(frst,i);
    s += *data;
  }

  return s;
}

// dummy
myenum dummy(myenum p)
{
  printf("Value: %i\n",p);
  return (myenum)((int)p+1);
}

void call_fun(void (*f)(void))
{
  f();
}

voidtovoid get_printhello()
{
  return printhello;
}


