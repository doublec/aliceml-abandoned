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
#define DEFINE6(name)				\
  static Worker::Result name() {		\
    Assert(Scheduler::GetNArgs() == 6);		\
    POP_PRIM_SELF();				\
    word x0 = Scheduler::GetCurrentArg(0);	\
    word x1 = Scheduler::GetCurrentArg(1);	\
    word x2 = Scheduler::GetCurrentArg(2);	\
    word x3 = Scheduler::GetCurrentArg(3);	\
    word x4 = Scheduler::GetCurrentArg(4);	\
    word x5 = Scheduler::GetCurrentArg(5);
#define DEFINE7(name)						\
  static Worker::Result name() {				\
    Assert(Scheduler::GetNArgs() == 7);				\
    POP_PRIM_SELF();						\
    word x0 = Scheduler::GetCurrentArg(0);			\
    word x1 = Scheduler::GetCurrentArg(1);			\
    word x2 = Scheduler::GetCurrentArg(2);			\
    word x3 = Scheduler::GetCurrentArg(3);			\
    word x4 = Scheduler::GetCurrentArg(4);			\
    word x5 = Scheduler::GetCurrentArg(5);			\
    word x6 = Scheduler::GetCurrentArg(6);                
#define DEFINE8(name)						\
  static Worker::Result name() {				\
    Assert(Scheduler::GetNArgs() == 8);				\
    POP_PRIM_SELF();						\
    word x0 = Scheduler::GetCurrentArg(0);			\
    word x1 = Scheduler::GetCurrentArg(1);			\
    word x2 = Scheduler::GetCurrentArg(2);			\
    word x3 = Scheduler::GetCurrentArg(3);			\
    word x4 = Scheduler::GetCurrentArg(4);			\
    word x5 = Scheduler::GetCurrentArg(5);			\
    word x6 = Scheduler::GetCurrentArg(6);			\
    word x7 = Scheduler::GetCurrentArg(7);                
#define DEFINE9(name)						\
  static Worker::Result name() {				\
    Assert(Scheduler::GetNArgs() == 9);				\
    POP_PRIM_SELF();						\
    word x0 = Scheduler::GetCurrentArg(0);			\
    word x1 = Scheduler::GetCurrentArg(1);			\
    word x2 = Scheduler::GetCurrentArg(2);			\
    word x3 = Scheduler::GetCurrentArg(3);			\
    word x4 = Scheduler::GetCurrentArg(4);			\
    word x5 = Scheduler::GetCurrentArg(5);			\
    word x6 = Scheduler::GetCurrentArg(6);			\
    word x7 = Scheduler::GetCurrentArg(7);			\
    word x8 = Scheduler::GetCurrentArg(8);                
#define DEFINE10(name)						\
  static Worker::Result name() {				\
    Assert(Scheduler::GetNArgs() == 10);			\
    POP_PRIM_SELF();						\
    word x0 = Scheduler::GetCurrentArg(0);			\
    word x1 = Scheduler::GetCurrentArg(1);			\
    word x2 = Scheduler::GetCurrentArg(2);			\
    word x3 = Scheduler::GetCurrentArg(3);			\
    word x4 = Scheduler::GetCurrentArg(4);			\
    word x5 = Scheduler::GetCurrentArg(5);			\
    word x6 = Scheduler::GetCurrentArg(6);			\
    word x7 = Scheduler::GetCurrentArg(7);			\
    word x8 = Scheduler::GetCurrentArg(8);			\
    word x9 = Scheduler::GetCurrentArg(9);                
#define DEFINE11(name)						\
  static Worker::Result name() {				\
    Assert(Scheduler::GetNArgs() == 11);			\
    POP_PRIM_SELF();						\
    word x0 = Scheduler::GetCurrentArg(0);			\
    word x1 = Scheduler::GetCurrentArg(1);			\
    word x2 = Scheduler::GetCurrentArg(2);			\
    word x3 = Scheduler::GetCurrentArg(3);			\
    word x4 = Scheduler::GetCurrentArg(4);			\
    word x5 = Scheduler::GetCurrentArg(5);			\
    word x6 = Scheduler::GetCurrentArg(6);			\
    word x7 = Scheduler::GetCurrentArg(7);			\
    word x8 = Scheduler::GetCurrentArg(8);			\
    word x9 = Scheduler::GetCurrentArg(9);			\
    word x10= Scheduler::GetCurrentArg(10);               
#define DEFINE12(name)						\
  static Worker::Result name() {				\
    Assert(Scheduler::GetNArgs() == 12);			\
    POP_PRIM_SELF();						\
    word x0 = Scheduler::GetCurrentArg(0);			\
    word x1 = Scheduler::GetCurrentArg(1);			\
    word x2 = Scheduler::GetCurrentArg(2);			\
    word x3 = Scheduler::GetCurrentArg(3);			\
    word x4 = Scheduler::GetCurrentArg(4);			\
    word x5 = Scheduler::GetCurrentArg(5);			\
    word x6 = Scheduler::GetCurrentArg(6);			\
    word x7 = Scheduler::GetCurrentArg(7);			\
    word x8 = Scheduler::GetCurrentArg(8);			\
    word x9 = Scheduler::GetCurrentArg(9);			\
    word x10= Scheduler::GetCurrentArg(10);			\
    word x11= Scheduler::GetCurrentArg(11);               
#define DEFINE13(name)						\
  static Worker::Result name() {				\
    Assert(Scheduler::GetNArgs() == 13);			\
    POP_PRIM_SELF();						\
    word x0 = Scheduler::GetCurrentArg(0);			\
    word x1 = Scheduler::GetCurrentArg(1);			\
    word x2 = Scheduler::GetCurrentArg(2);			\
    word x3 = Scheduler::GetCurrentArg(3);			\
    word x4 = Scheduler::GetCurrentArg(4);			\
    word x5 = Scheduler::GetCurrentArg(5);			\
    word x6 = Scheduler::GetCurrentArg(6);			\
    word x7 = Scheduler::GetCurrentArg(7);			\
    word x8 = Scheduler::GetCurrentArg(8);			\
    word x9 = Scheduler::GetCurrentArg(9);			\
    word x10= Scheduler::GetCurrentArg(10);			\
    word x11= Scheduler::GetCurrentArg(11);			\
    word x12= Scheduler::GetCurrentArg(12);               
#define DEFINE14(name)						\
  static Worker::Result name() {				\
    Assert(Scheduler::GetNArgs() == 14);			\
    POP_PRIM_SELF();						\
    word x0 = Scheduler::GetCurrentArg(0);			\
    word x1 = Scheduler::GetCurrentArg(1);			\
    word x2 = Scheduler::GetCurrentArg(2);			\
    word x3 = Scheduler::GetCurrentArg(3);			\
    word x4 = Scheduler::GetCurrentArg(4);			\
    word x5 = Scheduler::GetCurrentArg(5);			\
    word x6 = Scheduler::GetCurrentArg(6);			\
    word x7 = Scheduler::GetCurrentArg(7);			\
    word x8 = Scheduler::GetCurrentArg(8);			\
    word x9 = Scheduler::GetCurrentArg(9);			\
    word x10= Scheduler::GetCurrentArg(10);			\
    word x11= Scheduler::GetCurrentArg(11);			\
    word x12= Scheduler::GetCurrentArg(12);			\
    word x13= Scheduler::GetCurrentArg(13);               
#define DEFINE15(name)						\
  static Worker::Result name() {				\
    Assert(Scheduler::GetNArgs() == 15);			\
    POP_PRIM_SELF();						\
    word x0 = Scheduler::GetCurrentArg(0);			\
    word x1 = Scheduler::GetCurrentArg(1);			\
    word x2 = Scheduler::GetCurrentArg(2);			\
    word x3 = Scheduler::GetCurrentArg(3);			\
    word x4 = Scheduler::GetCurrentArg(4);			\
    word x5 = Scheduler::GetCurrentArg(5);			\
    word x6 = Scheduler::GetCurrentArg(6);			\
    word x7 = Scheduler::GetCurrentArg(7);			\
    word x8 = Scheduler::GetCurrentArg(8);			\
    word x9 = Scheduler::GetCurrentArg(9);			\
    word x10= Scheduler::GetCurrentArg(10);			\
    word x11= Scheduler::GetCurrentArg(11);			\
    word x12= Scheduler::GetCurrentArg(12);			\
    word x13= Scheduler::GetCurrentArg(13);			\
    word x14= Scheduler::GetCurrentArg(14);               
#define DEFINE16(name)						\
  static Worker::Result name() {				\
    Assert(Scheduler::GetNArgs() == 16);			\
    POP_PRIM_SELF();						\
    word x0 = Scheduler::GetCurrentArg(0);			\
    word x1 = Scheduler::GetCurrentArg(1);			\
    word x2 = Scheduler::GetCurrentArg(2);			\
    word x3 = Scheduler::GetCurrentArg(3);			\
    word x4 = Scheduler::GetCurrentArg(4);			\
    word x5 = Scheduler::GetCurrentArg(5);			\
    word x6 = Scheduler::GetCurrentArg(6);			\
    word x7 = Scheduler::GetCurrentArg(7);			\
    word x8 = Scheduler::GetCurrentArg(8);			\
    word x9 = Scheduler::GetCurrentArg(9);			\
    word x10= Scheduler::GetCurrentArg(10);			\
    word x11= Scheduler::GetCurrentArg(11);			\
    word x12= Scheduler::GetCurrentArg(12);			\
    word x13= Scheduler::GetCurrentArg(13);			\
    word x14= Scheduler::GetCurrentArg(14);			\
    word x15= Scheduler::GetCurrentArg(15);               
#define DEFINE17(name)						\
  static Worker::Result name() {				\
    Assert(Scheduler::GetNArgs() == 16);			\
    POP_PRIM_SELF();						\
    word x0 = Scheduler::GetCurrentArg(0);			\
    word x1 = Scheduler::GetCurrentArg(1);			\
    word x2 = Scheduler::GetCurrentArg(2);			\
    word x3 = Scheduler::GetCurrentArg(3);			\
    word x4 = Scheduler::GetCurrentArg(4);			\
    word x5 = Scheduler::GetCurrentArg(5);			\
    word x6 = Scheduler::GetCurrentArg(6);			\
    word x7 = Scheduler::GetCurrentArg(7);			\
    word x8 = Scheduler::GetCurrentArg(8);			\
    word x9 = Scheduler::GetCurrentArg(9);			\
    word x10= Scheduler::GetCurrentArg(10);			\
    word x11= Scheduler::GetCurrentArg(11);			\
    word x12= Scheduler::GetCurrentArg(12);			\
    word x13= Scheduler::GetCurrentArg(13);			\
    word x14= Scheduler::GetCurrentArg(14);			\
    word x15= Scheduler::GetCurrentArg(15);                     \
    word x16= Scheduler::GetCurrentArg(16);               





// macro for "normal" unmanaged C pointers (that are simply used
// as words in Alice)
#define DECLARE_UNMANAGED_POINTER(pointer, x)                       \
  void *pointer = NULL;                                             \
  if (Store::WordToTransient(x) != INVALID_POINTER) { REQUEST(x); } \
  else { pointer = Store::WordToUnmanagedPointer(x); }     

#define DECLARE_INT_AS(t, i, x)                 \
    t i;                                        \
      { s_int tmp_ = Store::WordToInt (x);      \
    if (tmp_ == INVALID_INT) { REQUEST(x); }       \
    else {}                                     \
    i = tmp_; }

// DECLARE_STRING, DECLARE_REAL and DECLARE_ARRAY only return
// the Alice classes String, Real and Array. The following macros
// convert them into the C types.
#define DECLARE_CSTRING(str,x)                           \
  DECLARE_STRING(str##__temp,x);                         \
  char *str = str##__temp->ExportC();
#define DECLARE_CDOUBLE(dbl,x)                           \
  DECLARE_REAL(dbl##__temp,x);                           \
  double dbl = dbl##__temp->GetValue();
      
#define DECLARE_CFLOAT(dbl,x)                           \
  DECLARE_REAL(dbl##__temp,x);                           \
  float dbl = dbl##__temp->GetValue();

#define DECLARE_DOUBLE_AS(t, dbl,x)                      \
  DECLARE_REAL(dbl##__temp,x);                           \
  t dbl = dbl##__temp->GetValue();


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
  Scheduler::SetNArgs(4);			\
  Scheduler::SetCurrentArg(0, w1);		\
  Scheduler::SetCurrentArg(1, w2);		\
  Scheduler::SetCurrentArg(2, w3);		\
  Scheduler::SetCurrentArg(3, w4);		\
  return Worker::CONTINUE;			\
}

#define RETURN5(w1, w2, w3, w4, w5) {		\
  Scheduler::SetNArgs(5);			\
  Scheduler::SetCurrentArg(0, w1);		\
  Scheduler::SetCurrentArg(1, w2);		\
  Scheduler::SetCurrentArg(2, w3);		\
  Scheduler::SetCurrentArg(3, w4);		\
  Scheduler::SetCurrentArg(4, w5);		\
  return Worker::CONTINUE;			\
}

#define RETURN6(w1, w2, w3, w4, w5, w6) {	\
  Scheduler::SetNArgs(6);			\
  Scheduler::SetCurrentArg(0, w1);		\
  Scheduler::SetCurrentArg(1, w2);		\
  Scheduler::SetCurrentArg(2, w3);		\
  Scheduler::SetCurrentArg(3, w4);		\
  Scheduler::SetCurrentArg(4, w5);		\
  Scheduler::SetCurrentArg(5, w6);		\
  return Worker::CONTINUE;			\
}

#define RETURN7(w1, w2, w3, w4, w5, w6, w7) {	\
  Scheduler::SetNArgs(7);			\
  Scheduler::SetCurrentArg(0, w1);		\
  Scheduler::SetCurrentArg(1, w2);		\
  Scheduler::SetCurrentArg(2, w3);		\
  Scheduler::SetCurrentArg(3, w4);		\
  Scheduler::SetCurrentArg(4, w5);		\
  Scheduler::SetCurrentArg(5, w6);		\
  Scheduler::SetCurrentArg(6, w7);		\
  return Worker::CONTINUE;			\
}

#define RETURN8(w1, w2, w3, w4, w5, w6, w7, w8) {	\
  Scheduler::SetNArgs(8);				\
  Scheduler::SetCurrentArg(0, w1);			\
  Scheduler::SetCurrentArg(1, w2);			\
  Scheduler::SetCurrentArg(2, w3);			\
  Scheduler::SetCurrentArg(3, w4);			\
  Scheduler::SetCurrentArg(4, w5);			\
  Scheduler::SetCurrentArg(5, w6);			\
  Scheduler::SetCurrentArg(6, w7);			\
  Scheduler::SetCurrentArg(7, w8);			\
  return Worker::CONTINUE;				\
}

// General macros that wrap the heterogenous "ToWord" conversion
#define INT_TO_WORD(i) Store::IntToWord(i)
#define REAL_TO_WORD(r) Real::New(r)->ToWord()
#define STRING_TO_WORD(s)					\
   ((s != NULL) ?						\
    String::New(reinterpret_cast<const char *>(s))->ToWord() :	\
    String::New(static_cast<u_int>(0))->ToWord())
#define UNMANAGED_POINTER_TO_WORD(p) Store::UnmanagedPointerToWord(p)

extern word GtkCoreErrorConstructor;

#define RAISE_CORE_ERROR(msg) \
 do { ConVal *conVal = ConVal::New(Store::DirectWordToBlock(GtkCoreErrorConstructor), 1); \
      conVal->Init (0, String::New(msg)->ToWord()); \
      word exn = conVal->ToWord (); \
      RAISE(exn); \
    } while (0);

/*
GSList *alice_list_to_gslist (word l);
word gslist_to_alice_list (GSList *l);

GList *alice_list_to_glist (word l);
word glist_to_alice_list (GList *l);
*/

word alice_cons (word h, word t);

// FIXME
#define gslist_to_alice_list(l) Store::IntToWord (Types::nil)
#define glist_to_alice_list(l)  Store::IntToWord (Types::nil)
#define alice_list_to_gslist(l) ((GSList*)0)
#define alice_list_to_glist(l) ((GList*)0)

#endif
