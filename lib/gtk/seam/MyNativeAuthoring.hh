#ifndef _MY_NATIVE_AUTHORING_HH_
#define _MY_NATIVE_AUTHORING_HH_

#include "Alice.hh"

#ifndef DEFINE5
#define DEFINE5(name)					\
  static Interpreter::Result name() {			\
    Assert(Scheduler::nArgs == 5);			\
    word prim_self = Scheduler::GetAndPopFrame();	\
    prim_self = prim_self;				\
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];		\
    word x2 = Scheduler::currentArgs[2];		\
    word x3 = Scheduler::currentArgs[3];                \
    word x4 = Scheduler::currentArgs[4];                
#endif
#define DEFINE6(name)					\
  static Interpreter::Result name() {			\
    Assert(Scheduler::nArgs == 6);			\
    word prim_self = Scheduler::GetAndPopFrame();	\
    prim_self = prim_self;				\
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];		\
    word x2 = Scheduler::currentArgs[2];		\
    word x3 = Scheduler::currentArgs[3];                \
    word x4 = Scheduler::currentArgs[4];                \
    word x5 = Scheduler::currentArgs[5];
#define DEFINE7(name)					\
  static Interpreter::Result name() {			\
    Assert(Scheduler::nArgs == 7);			\
    word prim_self = Scheduler::GetAndPopFrame();	\
    prim_self = prim_self;				\
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];		\
    word x2 = Scheduler::currentArgs[2];		\
    word x3 = Scheduler::currentArgs[3];                \
    word x4 = Scheduler::currentArgs[4];                \
    word x5 = Scheduler::currentArgs[5];                \
    word x6 = Scheduler::currentArgs[6];                
#define DEFINE8(name)					\
  static Interpreter::Result name() {			\
    Assert(Scheduler::nArgs == 8);			\
    word prim_self = Scheduler::GetAndPopFrame();	\
    prim_self = prim_self;				\
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];		\
    word x2 = Scheduler::currentArgs[2];		\
    word x3 = Scheduler::currentArgs[3];                \
    word x4 = Scheduler::currentArgs[4];                \
    word x5 = Scheduler::currentArgs[5];                \
    word x6 = Scheduler::currentArgs[6];                \
    word x7 = Scheduler::currentArgs[7];                
#define DEFINE9(name)					\
  static Interpreter::Result name() {			\
    Assert(Scheduler::nArgs == 9);			\
    word prim_self = Scheduler::GetAndPopFrame();	\
    prim_self = prim_self;				\
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
  static Interpreter::Result name() {			\
    Assert(Scheduler::nArgs == 10);			\
    word prim_self = Scheduler::GetAndPopFrame();	\
    prim_self = prim_self;				\
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
  static Interpreter::Result name() {			\
    Assert(Scheduler::nArgs == 11);			\
    word prim_self = Scheduler::GetAndPopFrame();	\
    prim_self = prim_self;				\
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
  static Interpreter::Result name() {			\
    Assert(Scheduler::nArgs == 12);			\
    word prim_self = Scheduler::GetAndPopFrame();	\
    prim_self = prim_self;				\
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
  static Interpreter::Result name() {			\
    Assert(Scheduler::nArgs == 13);			\
    word prim_self = Scheduler::GetAndPopFrame();	\
    prim_self = prim_self;				\
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
  static Interpreter::Result name() {			\
    Assert(Scheduler::nArgs == 14);			\
    word prim_self = Scheduler::GetAndPopFrame();	\
    prim_self = prim_self;				\
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
  static Interpreter::Result name() {			\
    Assert(Scheduler::nArgs == 15);			\
    word prim_self = Scheduler::GetAndPopFrame();	\
    prim_self = prim_self;				\
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
  static Interpreter::Result name() {			\
    Assert(Scheduler::nArgs == 16);			\
    word prim_self = Scheduler::GetAndPopFrame();	\
    prim_self = prim_self;				\
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
#define DEFINE17(name)					\
  static Interpreter::Result name() {			\
    Assert(Scheduler::nArgs == 17);			\
    word prim_self = Scheduler::GetAndPopFrame();	\
    prim_self = prim_self;				\
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
    word x15= Scheduler::currentArgs[15];               \
    word x16= Scheduler::currentArgs[16];               

// macro for "normal" unmanaged C pointers (that are simply used
// as words in Alice)
//#define DECLARE_UNMANAGED_POINTER(pointer, x)                       \
//  void *pointer = NULL;                                             \
//  if (Store::WordToTransient(x) != INVALID_POINTER) { REQUEST(x); } \
//  else { pointer = Store::WordToUnmanagedPointer(x); }     

#define DECLARE_ENUM DECLARE_INT
#define DECLARE_CSTRING(str,x)                           \
  DECLARE_STRING(str##__temp,x);                         \
  char *str = str##__temp->ExportC();
#define DECLARE_CDOUBLE(dbl,x)                           \
  DECLARE_REAL(dbl##__temp,x);                           \
  double dbl = dbl##__temp->GetValue();
#define DECLARE_CARRAY(a,x,type,F)                       \
  DECLARE_ARRAY(a##__temp,x);                            \
  type a [a##__temp->GetLength()];                       \
  for (u_int a##__iter = 0; a##__iter < a##__temp->GetLength(); a##__iter++) {\
     F(a##__value,a##__temp->Sub(a##__iter));            \
     a[a##__iter] = a##__value;                          \
  }

#define RETURN_TUPLE0() RETURN_UNIT
#define RETURN_TUPLE1 RETURN

#define RETURN_TUPLE2(y0,y1)                             \
  Tuple *result = Tuple::New(2);                         \
  result->Init(0,y0);                                    \
  result->Init(1,y1);                                    \
  RETURN(result->ToWord());

#define RETURN_TUPLE3(y0,y1,y2)                          \
  Tuple *result = Tuple::New(3);                         \
  result->Init(0,y0);                                    \
  result->Init(1,y1);                                    \
  result->Init(2,y2);                                    \
  RETURN(result->ToWord());

#define RETURN_TUPLE4(y0,y1,y2,y3)                       \
  Tuple *result = Tuple::New(3);                         \
  result->Init(0,y0);                                    \
  result->Init(1,y1);                                    \
  result->Init(2,y2);                                    \
  result->Init(3,y3);                                    \
  RETURN(result->ToWord());

#define RETURN_TUPLE5(y0,y1,y2,y3,y4)                    \
  Tuple *result = Tuple::New(3);                         \
  result->Init(0,y0);                                    \
  result->Init(1,y1);                                    \
  result->Init(2,y2);                                    \
  result->Init(3,y3);                                    \
  result->Init(4,y4);                                    \
  RETURN(result->ToWord());

#define RETURN_TUPLE6(y0,y1,y2,y3,y4,y5)                 \
  Tuple *result = Tuple::New(4);                         \
  result->Init(0,y0);                                    \
  result->Init(1,y1);                                    \
  result->Init(2,y2);                                    \
  result->Init(3,y3);                                    \
  result->Init(4,y4);                                    \
  result->Init(5,y5);                                    \
  RETURN(result->ToWord());

#define INT_TO_WORD(i) Store::IntToWord(i)
#define REAL_TO_WORD(r) Real::New(r)->ToWord()
#define STRING_TO_WORD(s) String::New( \
                            reinterpret_cast<const char *>(s))->ToWord()
#define ENUM_TO_WORD INT_TO_WORD

inline word PointerToObject(void *p, int type) {
  Tuple *t = Tuple::New(2);
  t->Init(0,Store::UnmanagedPointerToWord(p));
  t->Init(1,Store::IntToWord(type));
  return t->ToWord();
}

#endif
