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

#include "generic/RootSet.hh"
#include "java/ClassLoader.hh"
#include "java/ByteCodeInterpreter.hh"
#include "java/Authoring.hh"

static word wStackTraceElementClass;

DEFINE1(fillInStackTrace) {
  DECLARE_OBJECT(_this, x0);
  Assert(_this != INVALID_POINTER);
  Backtrace *backtrace = Backtrace::New();
  TaskStack *taskStack = Scheduler::currentTaskStack;
  for (u_int i = Scheduler::nFrames; i--; ) {
    StackFrame *frame = StackFrame::FromWordDirect(taskStack->GetArg(i));
    if (frame->GetWorker() == ByteCodeInterpreter::self)
      backtrace->Enqueue(frame->ToWord());
  }
  _this->PutInstanceField(0, backtrace->ToWord());
  RETURN(_this->ToWord());
} END

DEFINE1(getStackTraceDepth) {
  DECLARE_OBJECT(_this, x0);
  Assert(_this != INVALID_POINTER);
  word wBacktrace = _this->GetInstanceField(0);
  if (wBacktrace == null) RETURN_INT(0);
  RETURN_INT(Backtrace::FromWordDirect(wBacktrace)->GetNumberOfElements());
} END

DEFINE2(getStackTraceElement) {
  DECLARE_OBJECT(_this, x0);
  Assert(_this != INVALID_POINTER);
  DECLARE_INT(i, x1);
  word wBacktrace = _this->GetInstanceField(0);
  if (wBacktrace == null)
    THROW(NullPointerException, "backtrace");
  Class *stackTraceElementClass = Class::FromWord(wStackTraceElementClass);
  if (stackTraceElementClass == INVALID_POINTER)
    REQUEST(wStackTraceElementClass);
  Assert(stackTraceElementClass->GetNumberOfInstanceFields() ==
	 StackTraceElement::SIZE);
  Backtrace *backtrace = Backtrace::FromWordDirect(wBacktrace);
  Object *stackTraceElement = Object::New(stackTraceElementClass);
  if (i < 0 || static_cast<u_int>(i) >= backtrace->GetNumberOfElements())
    THROW(ArrayIndexOutOfBoundsException, "backtrace");
  ByteCodeInterpreter::FillStackTraceElement(backtrace->GetNthElement(i),
					     stackTraceElement);
  RETURN(stackTraceElement->ToWord());
} END

void NativeMethodTable::java_lang_Throwable(JavaString *className) {
  ClassLoader *classLoader = ClassLoader::GetBootstrapClassLoader();
  wStackTraceElementClass =
    classLoader->ResolveClass(JavaString::New("java/lang/StackTraceElement"));
  RootSet::Add(wStackTraceElementClass);
  Register(className, "fillInStackTrace", "()Ljava/lang/Throwable;",
	   fillInStackTrace, 1, true);
  Register(className, "getStackTraceDepth", "()I", 
	   getStackTraceDepth, 1, true);
  Register(className, "getStackTraceElement",
	   "(I)Ljava/lang/StackTraceElement;", getStackTraceElement, 2, true);
}
