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

#include <cstdio>

#include "java/ThrowWorker.hh"
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

  virtual u_int GetFrameSize(StackFrame *sFrame);
  virtual Result Run(StackFrame *sFrame);
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

class RunMainFrame: private StackFrame {
protected:
  enum { METHOD_REF_POS, SIZE };
public:
  static RunMainFrame *New(Thread *thread, word wMethodRef) {
    NEW_THREAD_STACK_FRAME(frame, thread, RunMainWorker::self, SIZE);
    frame->InitArg(METHOD_REF_POS, wMethodRef);
    return static_cast<RunMainFrame *>(frame);
  }

  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
  word GetMethodRef() {
    return GetArg(METHOD_REF_POS);
  }
};

RunMainWorker *RunMainWorker::self;

void RunMainWorker::PushFrame(Thread *thread, word wMethodRef) {
  RunMainFrame::New(thread, wMethodRef);
}

u_int RunMainWorker::GetFrameSize(StackFrame *sFrame) {
  RunMainFrame *frame = static_cast<RunMainFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result RunMainWorker::Run(StackFrame *sFrame) {
  RunMainFrame *frame = static_cast<RunMainFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  word wMethodRef = frame->GetMethodRef();
  MethodRef *methodRef = MethodRef::FromWord(wMethodRef);
  if (methodRef == INVALID_POINTER) {
    Scheduler::currentData = wMethodRef;
    return Worker::REQUEST;
  }
  if (methodRef->GetLabel() != JavaLabel::StaticMethodRef) {
    ThrowWorker::PushFrame(ThrowWorker::NoSuchMethodError,
			   JavaString::New("main"));
    Scheduler::nArgs = 0;
    return CONTINUE;
  }
  StaticMethodRef *staticMethodRef =
    static_cast<StaticMethodRef *>(methodRef);
  Scheduler::PopFrame(frame->GetSize());
  Closure *closure =
    staticMethodRef->GetClass()->GetStaticMethod(staticMethodRef->GetIndex());
  //--** pass string array as argument
  return Scheduler::PushCall(closure->ToWord());
}

const char *RunMainWorker::Identify() {
  return "RunMainWorker";
}

void RunMainWorker::DumpFrame(StackFrame *) {
  std::fprintf(stderr, "Run `void main(String[] args)'\n");
}

void Startup(int argc, char *argv[]) {
  RunMainWorker::Init();
  ClassLoader *classLoader = ClassLoader::GetBootstrapClassLoader();
  Thread *thread = Scheduler::NewThread(0, Store::IntToWord(0));
  {
    char buf[512];
    std::sprintf(buf, "%s", argv[1]);
    for (u_int i = std::strlen(buf); i--; )
      if (buf[i] == '.') buf[i] = '/';
    word theClass = classLoader->ResolveClass(JavaString::New(buf));
    JavaString *name = JavaString::New("main");
    JavaString *descriptor = JavaString::New("([Ljava/lang/String;)V");
    word methodRef = classLoader->ResolveMethodRef(theClass, name, descriptor);
    RunMainWorker::PushFrame(thread, methodRef);
  }
#if defined(JAVA_INITIALIZE_SYSTEM_CLASS)
  {
    JavaString *className = JavaString::New("java/lang/System");
    word theClass = classLoader->ResolveClass(className);
    JavaString *name = JavaString::New("initializeSystemClass");
    JavaString *descriptor = JavaString::New("()V");
    word methodRef = classLoader->ResolveMethodRef(theClass, name, descriptor);
    RunMainWorker::PushFrame(thread, methodRef);
  }
#endif
  ClassLoader::PushPreloadFrame(thread);
}
