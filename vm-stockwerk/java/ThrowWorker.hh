//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __JAVA__THROW_WORKER_HH__
#define __JAVA__THROW_WORKER_HH__

#if defined(INTERFACE)
#pragma interface "java/ThrowWorker.hh"
#endif

#include "generic/Worker.hh"
#include "java/Data.hh"

class DllExport ThrowWorker: public Worker {
private:
  ThrowWorker() {}
public:
  struct Throwable {
    word wClass, wMethodRef;
  };

  static ThrowWorker *self;

  static Throwable ArithmeticException;
  static Throwable ArrayIndexOutOfBoundsException;
  static Throwable ArrayStoreException;
  static Throwable ClassCastException;
  static Throwable ClassCircularityError;
  static Throwable ClassFormatError;
  static Throwable IncompatibleClassChangeError;
  static Throwable IndexOutOfBoundsException;
  static Throwable InstantiationError;
  static Throwable NegativeArraySizeException;
  static Throwable NoClassDefFoundError;
  static Throwable NoSuchFieldError;
  static Throwable NoSuchMethodError;
  static Throwable NullPointerException;
  static Throwable VerifyError;

  static void Init();

  static void PushFrame(Throwable &throwable, JavaString *message);

  virtual u_int GetFrameSize(StackFrame *sFrame);
  virtual Result Run(StackFrame *sFrame);
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

#endif
