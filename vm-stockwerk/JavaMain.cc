//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cstdio>
#include <cstdlib>
#include "Seam.hh"
#include "java/ClassLoader.hh"
#include "java/ClassInfo.hh"
#include "java/JavaLanguageLayer.hh"
#include "java/Startup.hh"

//
// ClassInitializerWorker
//
class ClassInitializerWorker: public Worker {
private:
  ClassInitializerWorker() {}
public:
  static ClassInitializerWorker *self;

  static void Init();

  static void PushFrame(word wClass);

  virtual u_int GetFrameSize(StackFrame *sFrame);
  virtual Result Run(StackFrame *sFrame);
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

class ClassInitializerFrame: private StackFrame {
protected:
  enum { CLASS_POS, SIZE };
public:
  using StackFrame::Clone;

  static ClassInitializerFrame *New(word wClass) {
    NEW_STACK_FRAME(frame, ClassInitializerWorker::self, SIZE);
    frame->InitArg(CLASS_POS, wClass);
    return static_cast<ClassInitializerFrame *>(frame);
  }

  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
  word GetClass() {
    return GetArg(CLASS_POS);
  }
};

ClassInitializerWorker *ClassInitializerWorker::self;

void ClassInitializerWorker::Init() {
  self = new ClassInitializerWorker();
}

void ClassInitializerWorker::PushFrame(word wClass) {
  ClassInitializerFrame::New(wClass);
}

u_int ClassInitializerWorker::GetFrameSize(StackFrame *sFrame) {
  ClassInitializerFrame *frame = static_cast<ClassInitializerFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result ClassInitializerWorker::Run(StackFrame *sFrame) {
  ClassInitializerFrame *frame = static_cast<ClassInitializerFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  word wClass = frame->GetClass();
  Class *theClass = Class::FromWord(wClass);
  if (theClass == INVALID_POINTER) {
    Scheduler::currentData = wClass;
    return Worker::REQUEST;
  }
  Scheduler::PopFrame(frame->GetSize());
  if (theClass->IsInitialized()) {
    Scheduler::nArgs = 0;
    return Worker::CONTINUE;
  }
  return theClass->RunInitializer();
}

const char *ClassInitializerWorker::Identify() {
  return "ClassInitializerWorker";
}

void ClassInitializerWorker::DumpFrame(StackFrame *sFrame) {
  ClassInitializerFrame *frame = static_cast<ClassInitializerFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  Class *theClass = Class::FromWord(frame->GetClass());
  std::fprintf(stderr, "Initialize %s\n",
	       theClass == INVALID_POINTER? "(unknown class)":
	       theClass->GetClassInfo()->GetName()->ExportC());
}

//
// Main Language Layer DLL Functions
//
static void InitJava() {
  static bool initialized = false;
  if (!initialized) {
    JavaLanguageLayer::Init();
    ClassInitializerWorker::Init();
    initialized = true;
  }
}

void Start(int argc, const char *argv[]) {
  if (argc < 2) {
    std::fprintf(stderr, "usage: %s <classfile> <args...>\n", argv[0]);
    std::exit(2);
  }
  InitJava();
  Startup(argc, argv);
}

Worker::Result Load(String *name) {
  InitJava();
  if (name == NULL)
    return Worker::CONTINUE;
  ClassLoader *classLoader = ClassLoader::GetBootstrapClassLoader();
  word wClass = classLoader->ResolveClass(JavaString::New(name));
  ClassInitializerWorker::PushFrame(wClass);
  Scheduler::nArgs = 0;
  return Worker::CONTINUE;
}
