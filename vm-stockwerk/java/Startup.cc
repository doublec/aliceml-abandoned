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
#pragma implementation "java/Startup.hh"
#endif

#include "generic/String.hh"
#include "generic/Worker.hh"
#include "generic/Properties.hh"
#include "generic/Scheduler.hh"
#include "java/StackFrame.hh"
#include "java/ClassLoader.hh"
#include "java/Startup.hh"

class RunMainWorker: public Worker {
private:
  RunMainWorker() {}
public:
  static RunMainWorker *self;

  static void Init() {
    self = new RunMainWorker();
  }

  static void PushFrame(Thread *thread, word wMethodRef);

  virtual Result Run();
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

class RunMainFrame: private StackFrame {
protected:
  enum { METHOD_REF_POS, SIZE };
public:
  using Block::ToWord;

  static RunMainFrame *New(word wMethodRef) {
    StackFrame *frame =
      StackFrame::New(RUN_MAIN_FRAME, RunMainWorker::self, SIZE);
    frame->InitArg(METHOD_REF_POS, wMethodRef);
    return static_cast<RunMainFrame *>(frame);
  }
  static RunMainFrame *FromWordDirect(word x) {
    StackFrame *frame = StackFrame::FromWordDirect(x);
    Assert(frame->GetLabel() == RUN_MAIN_FRAME);
    return static_cast<RunMainFrame *>(frame);
  }

  word GetMethodRef() {
    return GetArg(METHOD_REF_POS);
  }
};

RunMainWorker *RunMainWorker::self;

void RunMainWorker::PushFrame(Thread *thread, word wMethodRef) {
  thread->PushFrame(RunMainFrame::New(wMethodRef)->ToWord());
}

Worker::Result RunMainWorker::Run() {
  RunMainFrame *frame = RunMainFrame::FromWordDirect(Scheduler::GetFrame());
  word wMethodRef = frame->GetMethodRef();
  //--** the following may fail an assertion, if the method is not static:
  StaticMethodRef *methodRef = StaticMethodRef::FromWord(wMethodRef);
  if (methodRef == INVALID_POINTER) {
    Scheduler::currentData = wMethodRef;
    return Worker::REQUEST;
  }
  Scheduler::PopFrame();
  Closure *closure =
    methodRef->GetClass()->GetStaticMethod(methodRef->GetIndex());
  return Scheduler::PushCall(closure->ToWord());
}

const char *RunMainWorker::Identify() {
  return "RunMainWorker";
}

void RunMainWorker::DumpFrame(word) {
  std::fprintf(stderr, "Run `void main(String[] args)'\n");
}

void Startup() {
  RunMainWorker::Init();
  ClassLoader *classLoader = ClassLoader::New();
  char buf[512];
  std::sprintf(buf, "%s",
	       String::FromWordDirect(Properties::rootUrl)->ExportC());
  for (u_int i = std::strlen(buf); i--; )
    if (buf[i] == '.') buf[i] = '/';
  word theClass = classLoader->ResolveClass(JavaString::New(buf));
  JavaString *name = JavaString::New("main");
  JavaString *descriptor = JavaString::New("([Ljava/lang/String;)V");
  word methodRef = classLoader->ResolveMethodRef(theClass, name, descriptor);
  Thread *thread = Scheduler::NewThread(0, Store::IntToWord(0));
  RunMainWorker::PushFrame(thread, methodRef);
}
