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

#if defined(INTERFACE)
#pragma implementation "java/ThrowWorker.hh"
#endif

#include <cstdio>

#include "generic/RootSet.hh"
#include "generic/Backtrace.hh"
#include "java/ThrowWorker.hh"
#include "java/ClassInfo.hh"
#include "java/ClassLoader.hh"

//
// ThrowFrame
//
class ThrowFrame: private StackFrame {
protected:
  enum { CLASS_POS, METHOD_REF_POS, MESSAGE_POS, SIZE };
public:
  using StackFrame::Clone;

  static ThrowFrame *New(word wClass, word wMethodRef, JavaString *message) {
    NEW_STACK_FRAME(frame, ThrowWorker::self, SIZE);
    frame->InitArg(CLASS_POS, wClass);
    frame->InitArg(METHOD_REF_POS, wMethodRef);
    frame->InitArg(MESSAGE_POS, message->ToWord());
    return static_cast<ThrowFrame *>(frame);
  }

  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
  word GetClass() {
    return GetArg(CLASS_POS);
  }
  word GetMethodRef() {
    return GetArg(METHOD_REF_POS);
  }
  JavaString *GetMessage() {
    return JavaString::FromWordDirect(GetArg(MESSAGE_POS));
  }
};

//
// ThrowWorker Implementation
//
ThrowWorker *ThrowWorker::self;

ThrowWorker::Throwable ThrowWorker::ArithmeticException;
ThrowWorker::Throwable ThrowWorker::ArrayIndexOutOfBoundsException;
ThrowWorker::Throwable ThrowWorker::ArrayStoreException;
ThrowWorker::Throwable ThrowWorker::ClassCastException;
ThrowWorker::Throwable ThrowWorker::ClassCircularityError;
ThrowWorker::Throwable ThrowWorker::ClassFormatError;
ThrowWorker::Throwable ThrowWorker::IncompatibleClassChangeError;
ThrowWorker::Throwable ThrowWorker::IndexOutOfBoundsException;
ThrowWorker::Throwable ThrowWorker::InstantiationError;
ThrowWorker::Throwable ThrowWorker::NegativeArraySizeException;
ThrowWorker::Throwable ThrowWorker::NoClassDefFoundError;
ThrowWorker::Throwable ThrowWorker::NoSuchFieldError;
ThrowWorker::Throwable ThrowWorker::NoSuchMethodError;
ThrowWorker::Throwable ThrowWorker::NullPointerException;
ThrowWorker::Throwable ThrowWorker::VerifyError;

static void Init(const char *className, ThrowWorker::Throwable &throwable) {
  ClassLoader *classLoader = ClassLoader::GetBootstrapClassLoader();
  throwable.wClass = classLoader->ResolveClass(JavaString::New(className));
  JavaString *name = JavaString::New("<init>");
  JavaString *descriptor = JavaString::New("(Ljava/lang/String;)V");
  throwable.wMethodRef =
    classLoader->ResolveMethodRef(throwable.wClass, name, descriptor);
  RootSet::Add(throwable.wClass);
  RootSet::Add(throwable.wMethodRef);
}

void ThrowWorker::Init() {
  self = new ThrowWorker();
  ::Init("java/lang/ArithmeticException", ArithmeticException);
  ::Init("java/lang/ArrayIndexOutOfBoundsException",
	 ArrayIndexOutOfBoundsException);
  ::Init("java/lang/ArrayStoreException", ArrayStoreException);
  ::Init("java/lang/ClassCastException", ClassCastException);
  ::Init("java/lang/ClassCircularityError", ClassCircularityError);
  ::Init("java/lang/ClassFormatError", ClassFormatError);
  ::Init("java/lang/IncompatibleClassChangeError",
	 IncompatibleClassChangeError);
  ::Init("java/lang/IndexOutOfBoundsException", IndexOutOfBoundsException);
  ::Init("java/lang/InstantiationError", InstantiationError);
  ::Init("java/lang/NegativeArraySizeException", NegativeArraySizeException);
  ::Init("java/lang/NoClassDefFoundError", NoClassDefFoundError);
  ::Init("java/lang/NoSuchMethodError", NoSuchMethodError);
  ::Init("java/lang/NoSuchFieldError", NoSuchFieldError);
  ::Init("java/lang/NoSuchMethodError", NoSuchMethodError);
  ::Init("java/lang/NullPointerException", NullPointerException);
  ::Init("java/lang/VerifyError", VerifyError);
}

void ThrowWorker::PushFrame(Throwable &throwable, JavaString *message) {
  ThrowFrame::New(throwable.wClass, throwable.wMethodRef, message);
}

u_int ThrowWorker::GetFrameSize(StackFrame *sFrame) {
  ThrowFrame *frame = static_cast<ThrowFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result ThrowWorker::Run(StackFrame *sFrame) {
  ThrowFrame *frame = static_cast<ThrowFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  word wMethodRef = frame->GetMethodRef();
  MethodRef *methodRef = MethodRef::FromWord(wMethodRef);
  if (methodRef == INVALID_POINTER) {
    Scheduler::currentData = wMethodRef;
    return Worker::REQUEST;
  }
  Class *theClass = Class::FromWord(frame->GetClass());
  Assert(theClass != INVALID_POINTER);
  Object *object = Object::New(theClass);
  //--** invoke constructor method
  word wFrame = frame->Clone();
  Scheduler::PopFrame(frame->GetSize());
  Scheduler::currentData = object->ToWord();
  Scheduler::currentBacktrace = Backtrace::New(wFrame);
  return RAISE;
}

const char *ThrowWorker::Identify() {
  return "ThrowWorker";
}

void ThrowWorker::DumpFrame(StackFrame *sFrame) {
  ThrowFrame *frame = static_cast<ThrowFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  Class *theClass = Class::FromWord(frame->GetClass());
  std::fprintf(stderr, "Throw %s\n",
	       theClass == INVALID_POINTER? "(unknown class)":
	       theClass->GetClassInfo()->GetName()->ExportC());
}
