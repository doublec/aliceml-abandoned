

#include "c.h"
#include "malloc.h"


///////////////////////////////////////////////////////////
// Basic

void* my_new(int bytes) {
  return malloc(bytes);
}

void my_delete(void* ptr) {
  free(ptr);
}

void** pointer(void* ptr) {
  void** pptr = (void**)malloc(sizeof(void*));
  *pptr = ptr;
  return pptr;
}

void* unref(void** ptr) {
  return *ptr;
}

void* cast(void* ptr) {
  return ptr;
}


///////////////////////////////////////////////////////////
// Basic Types


/* Char */

int sizeof_char() {
  return sizeof(char);
}

char* cast_char(void* ptr) {
  return (char*)ptr;
}

char* pointer_char(char c) {
  char* pchr = (char*)malloc(sizeof(char));
  *pchr = c;
  return pchr;
}

char unref_char(char* pchr) {
  return *pchr;
}

/* Short */

int sizeof_short() {
  return sizeof(short);
}

short* cast_short(void* ptr) {
  return (short*)ptr;
}

short* pointer_short(short c) {
  short* pchr = (short*)malloc(sizeof(short));
  *pchr = c;
  return pchr;
}

short unref_short(short* pchr) {
  return *pchr;
}

/* Int */

int sizeof_int() {
  return sizeof(int);
}

int* cast_int(void* ptr) {
  return (int*)ptr;
}

int* pointer_int(int c) {
  int* pchr = (int*)malloc(sizeof(int));
  *pchr = c;
  return pchr;
}

int unref_int(int* pchr) {
  return *pchr;
}

/* Long */

int sizeof_long() {
  return sizeof(long);
}

long* cast_long(void* ptr) {
  return (long*)ptr;
}

long* pointer_long(long c) {
  long* pchr = (long*)malloc(sizeof(long));
  *pchr = c;
  return pchr;
}

long unref_long(long* pchr) {
  return *pchr;
}


/* Float */

int sizeof_float() {
  return sizeof(float);
}

float* cast_float(void* ptr) {
  return (float*)ptr;
}

float* pointer_float(float c) {
  float* pchr = (float*)malloc(sizeof(float));
  *pchr = c;
  return pchr;
}

float unref_float(float* pchr) {
  return *pchr;
}


/* Double */

int sizeof_double() {
  return sizeof(double);
}

double* cast_double(void* ptr) {
  return (double*)ptr;
}

double* pointer_double(double c) {
  double* pchr = (double*)malloc(sizeof(double));
  *pchr = c;
  return pchr;
}

double unref_double(double* pchr) {
  return *pchr;
}
