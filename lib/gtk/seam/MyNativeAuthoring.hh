//
// Author:
//   Robert Grabowski <grabow@ps.uni-sb.de>
//
// Copyright:
//   Robert Grabowski, 2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

/*
  This header file is included in every generated native source file.
  It contains general conversion macros that should be incorporated
  into the general Alice.hh authoring header file one day.
*/

#ifndef _MY_NATIVE_AUTHORING_HH_
#define _MY_NATIVE_AUTHORING_HH_

#include "Alice.hh"

// extending the existing DEFINE0..DEFINE5
#define DEFINE6(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::nArgs == 5);			\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];		\
    word x2 = Scheduler::currentArgs[2];		\
    word x3 = Scheduler::currentArgs[3];                \
    word x4 = Scheduler::currentArgs[4];                \
    word x5 = Scheduler::currentArgs[5];
#define DEFINE7(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::nArgs == 5);			\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];		\
    word x2 = Scheduler::currentArgs[2];		\
    word x3 = Scheduler::currentArgs[3];                \
    word x4 = Scheduler::currentArgs[4];                \
    word x5 = Scheduler::currentArgs[5];                \
    word x6 = Scheduler::currentArgs[6];                
#define DEFINE8(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::nArgs == 5);			\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];		\
    word x2 = Scheduler::currentArgs[2];		\
    word x3 = Scheduler::currentArgs[3];                \
    word x4 = Scheduler::currentArgs[4];                \
    word x5 = Scheduler::currentArgs[5];                \
    word x6 = Scheduler::currentArgs[6];                \
    word x7 = Scheduler::currentArgs[7];                
#define DEFINE9(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::nArgs == 5);			\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];		\
    word x2 = Scheduler::currentArgs[2];		\
    word x3 = Scheduler::currentArgs[3];                \
    word x4 = Scheduler::currentArgs[4];                \
    word x5 = Scheduler::currentArgs[5];                \
    word x6 = Scheduler::currentArgs[6];                \
    word x7 = Scheduler::currentArgs[7];                \
    word x8 = Scheduler::currentArgs[8];                
#define DEFINE10(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::nArgs == 5);			\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];		\
    word x2 = Scheduler::currentArgs[2];		\
    word x3 = Scheduler::currentArgs[3];                \
    word x4 = Scheduler::currentArgs[4];                \
    word x5 = Scheduler::currentArgs[5];                \
    word x6 = Scheduler::currentArgs[6];                \
    word x7 = Scheduler::currentArgs[7];                \
    word x8 = Scheduler::currentArgs[8];                \
    word x9 = Scheduler::currentArgs[9];                
#define DEFINE11(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::nArgs == 5);			\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];		\
    word x2 = Scheduler::currentArgs[2];		\
    word x3 = Scheduler::currentArgs[3];                \
    word x4 = Scheduler::currentArgs[4];                \
    word x5 = Scheduler::currentArgs[5];                \
    word x6 = Scheduler::currentArgs[6];                \
    word x7 = Scheduler::currentArgs[7];                \
    word x8 = Scheduler::currentArgs[8];                \
    word x9 = Scheduler::currentArgs[9];                \
    word x10= Scheduler::currentArgs[10];               
#define DEFINE12(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::nArgs == 5);			\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];		\
    word x2 = Scheduler::currentArgs[2];		\
    word x3 = Scheduler::currentArgs[3];                \
    word x4 = Scheduler::currentArgs[4];                \
    word x5 = Scheduler::currentArgs[5];                \
    word x6 = Scheduler::currentArgs[6];                \
    word x7 = Scheduler::currentArgs[7];                \
    word x8 = Scheduler::currentArgs[8];                \
    word x9 = Scheduler::currentArgs[9];                \
    word x10= Scheduler::currentArgs[10];               \
    word x11= Scheduler::currentArgs[11];               
#define DEFINE13(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::nArgs == 5);			\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];		\
    word x2 = Scheduler::currentArgs[2];		\
    word x3 = Scheduler::currentArgs[3];                \
    word x4 = Scheduler::currentArgs[4];                \
    word x5 = Scheduler::currentArgs[5];                \
    word x6 = Scheduler::currentArgs[6];                \
    word x7 = Scheduler::currentArgs[7];                \
    word x8 = Scheduler::currentArgs[8];                \
    word x9 = Scheduler::currentArgs[9];                \
    word x10= Scheduler::currentArgs[10];               \
    word x11= Scheduler::currentArgs[11];               \
    word x12= Scheduler::currentArgs[12];               
#define DEFINE14(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::nArgs == 5);			\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];		\
    word x2 = Scheduler::currentArgs[2];		\
    word x3 = Scheduler::currentArgs[3];                \
    word x4 = Scheduler::currentArgs[4];                \
    word x5 = Scheduler::currentArgs[5];                \
    word x6 = Scheduler::currentArgs[6];                \
    word x7 = Scheduler::currentArgs[7];                \
    word x8 = Scheduler::currentArgs[8];                \
    word x9 = Scheduler::currentArgs[9];                \
    word x10= Scheduler::currentArgs[10];               \
    word x11= Scheduler::currentArgs[11];               \
    word x12= Scheduler::currentArgs[12];               \
    word x13= Scheduler::currentArgs[13];               
#define DEFINE15(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::nArgs == 5);			\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];		\
    word x2 = Scheduler::currentArgs[2];		\
    word x3 = Scheduler::currentArgs[3];                \
    word x4 = Scheduler::currentArgs[4];                \
    word x5 = Scheduler::currentArgs[5];                \
    word x6 = Scheduler::currentArgs[6];                \
    word x7 = Scheduler::currentArgs[7];                \
    word x8 = Scheduler::currentArgs[8];                \
    word x9 = Scheduler::currentArgs[9];                \
    word x10= Scheduler::currentArgs[10];               \
    word x11= Scheduler::currentArgs[11];               \
    word x12= Scheduler::currentArgs[12];               \
    word x13= Scheduler::currentArgs[13];               \
    word x14= Scheduler::currentArgs[14];               
#define DEFINE16(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::nArgs == 5);			\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];		\
    word x2 = Scheduler::currentArgs[2];		\
    word x3 = Scheduler::currentArgs[3];                \
    word x4 = Scheduler::currentArgs[4];                \
    word x5 = Scheduler::currentArgs[5];                \
    word x6 = Scheduler::currentArgs[6];                \
    word x7 = Scheduler::currentArgs[7];                \
    word x8 = Scheduler::currentArgs[8];                \
    word x9 = Scheduler::currentArgs[9];                \
    word x10= Scheduler::currentArgs[10];               \
    word x11= Scheduler::currentArgs[11];               \
    word x12= Scheduler::currentArgs[12];               \
    word x13= Scheduler::currentArgs[13];               \
    word x14= Scheduler::currentArgs[14];               \
    word x15= Scheduler::currentArgs[15];               



// macro for "normal" unmanaged C pointers (that are simply used
// as words in Alice)
#define DECLARE_UNMANAGED_POINTER(pointer, x)                       \
  void *pointer = NULL;                                             \
  if (Store::WordToTransient(x) != INVALID_POINTER) { REQUEST(x); } \
  else { pointer = Store::WordToUnmanagedPointer(x); }     

// DECLARE_STRING, DECLARE_REAL and DECLARE_ARRAY only return
// the Alice classes String, Real and Array. The following macros
// convert them into the C types.
#define DECLARE_CSTRING(str,x)                           \
  DECLARE_STRING(str##__temp,x);                         \
  char *str = str##__temp->ExportC();
#define DECLARE_CDOUBLE(dbl,x)                           \
  DECLARE_REAL(dbl##__temp,x);                           \
  double dbl = dbl##__temp->GetValue();

// type = array member C type; F = conversion macro (like DECLARE_INT,etc.)
#define DECLARE_CARRAY(a,x,type,F)                       \
  DECLARE_VECTOR(a##__temp,x);                           \
  type a [a##__temp->GetLength()];                       \
  for (u_int a##__iter = 0; a##__iter < a##__temp->GetLength(); a##__iter++) {\
     F(a##__value,a##__temp->Sub(a##__iter));            \
     a[a##__iter] = a##__value;                          \
  }

// extending the existing RETURN0..RETURN3
#define RETURN4(w1, w2, w3, w4) {		\
  Scheduler::nArgs = 4;				\
  Scheduler::currentArgs[0] = w1;		\
  Scheduler::currentArgs[1] = w2;		\
  Scheduler::currentArgs[2] = w3;		\
  Scheduler::currentArgs[3] = w4;		\
  return Worker::CONTINUE;			\
}

#define RETURN5(w1, w2, w3, w4, w5) {		\
  Scheduler::nArgs = 5;				\
  Scheduler::currentArgs[0] = w1;		\
  Scheduler::currentArgs[1] = w2;		\
  Scheduler::currentArgs[2] = w3;		\
  Scheduler::currentArgs[3] = w4;		\
  Scheduler::currentArgs[4] = w5;		\
  return Worker::CONTINUE;			\
}

#define RETURN6(w1, w2, w3, w4, w5, w6) {	\
  Scheduler::nArgs = 6;				\
  Scheduler::currentArgs[0] = w1;		\
  Scheduler::currentArgs[1] = w2;		\
  Scheduler::currentArgs[2] = w3;		\
  Scheduler::currentArgs[3] = w4;		\
  Scheduler::currentArgs[4] = w5;		\
  Scheduler::currentArgs[5] = w6;		\
  return Worker::CONTINUE;			\
}

// General macros that wrap the heterogenous "ToWord" conversion
#define INT_TO_WORD(i) Store::IntToWord(i)
#define REAL_TO_WORD(r) Real::New(r)->ToWord()
#define STRING_TO_WORD(s) String::New( \
                            reinterpret_cast<const char *>(s))->ToWord()
#define UNMANAGED_POINTER_TO_WORD(p) Store::UnmanagedPointerToWord(p)
#endif
