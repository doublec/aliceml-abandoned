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

#include "generic/RootSet.hh"
#include "generic/Backtrace.hh"
#include "java/ThrowWorker.hh"
#include "java/StackFrame.hh"
#include "java/ClassLoader.hh"

//
// ThrowFrame
//

class ThrowFrame: private StackFrame {
protected:
  enum { EXCEPTION_POS, DETAIL_MESSAGE_POS, SIZE };
public:
  using Block::ToWord;

  static ThrowFrame *New(word exception, JavaString *detailMessage) {
    StackFrame *frame =
      StackFrame::New(THROW_FRAME, ThrowWorker::self, SIZE);
    frame->InitArg(EXCEPTION_POS, exception);
    frame->InitArg(DETAIL_MESSAGE_POS, detailMessage->ToWord());
    return static_cast<ThrowFrame *>(frame);
  }
};

//
// ThrowWorker Method Implementation
//

ThrowWorker *ThrowWorker::self;

word ThrowWorker::NoSuchMethodError;

void ThrowWorker::Init() {
  self = new ThrowWorker();
  ClassLoader *classLoader = ClassLoader::GetBootstrapClassLoader();
  NoSuchMethodError =
    classLoader->ResolveClass(JavaString::New("java/lang/NoSuchMethodError"));
  RootSet::Add(NoSuchMethodError);
}

void ThrowWorker::PushFrame(word exception, JavaString *detailMessage) {
  Scheduler::PushFrame(ThrowFrame::New(exception, detailMessage)->ToWord());
}

Worker::Result ThrowWorker::Run() {
  Scheduler::currentData = Store::IntToWord(0); //--**
  Scheduler::currentBacktrace = Backtrace::New(Scheduler::GetAndPopFrame());
  return Worker::RAISE; //--**
}

const char *ThrowWorker::Identify() {
  return "ThrowWorker";
}

void ThrowWorker::DumpFrame(word) {
  //--**
}
