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

#include <cstdio>

#include "generic/RootSet.hh"
#include "java/StackFrame.hh"
#include "java/ClassInfo.hh"
#include "java/ClassLoader.hh"
#include "java/Authoring.hh"

static word wConstructorClassFieldRef, wConstructorSlotFieldRef;

//
// ReturnInstanceWorker
//
class ReturnInstanceWorker: public Worker {
public:
  static ReturnInstanceWorker *self;
private:
  ReturnInstanceWorker() {}
public:
  static void Init() {
    self = new ReturnInstanceWorker();
  }

  static void PushFrame(Object *instance);

  virtual Result Run();
  virtual const char *Identify();
  virtual void DumpFrame(word wFrame);
};

class ReturnInstanceFrame: private StackFrame {
protected:
  enum { INSTANCE_POS, SIZE };
public:
  using Block::ToWord;

  static ReturnInstanceFrame *New(Object *instance) {
    StackFrame *frame =
      StackFrame::New(RETURN_INSTANCE_FRAME, ReturnInstanceWorker::self, SIZE);
    frame->InitArg(INSTANCE_POS, instance->ToWord());
    return static_cast<ReturnInstanceFrame *>(frame);
  }
  static ReturnInstanceFrame *FromWordDirect(word x) {
    StackFrame *frame = StackFrame::FromWordDirect(x);
    Assert(frame->GetLabel() == RETURN_INSTANCE_FRAME);
    return static_cast<ReturnInstanceFrame *>(frame);
  }

  Object *GetInstance() {
    return Object::FromWordDirect(GetArg(INSTANCE_POS));
  }
};

ReturnInstanceWorker *ReturnInstanceWorker::self;

void ReturnInstanceWorker::PushFrame(Object *instance) {
  Scheduler::PushFrame(ReturnInstanceFrame::New(instance)->ToWord());
}

Worker::Result ReturnInstanceWorker::Run() {
  ReturnInstanceFrame *frame =
    ReturnInstanceFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  Scheduler::nArgs = Scheduler::ONE_ARG;
  Scheduler::currentArgs[0] = frame->GetInstance()->ToWord();
  return CONTINUE;
}

const char *ReturnInstanceWorker::Identify() {
  return "ReturnInstanceWorker";
}

void ReturnInstanceWorker::DumpFrame(word wFrame) {
  ReturnInstanceFrame *frame = ReturnInstanceFrame::FromWordDirect(wFrame);
  Class *theClass = frame->GetInstance()->GetClass();
  std::fprintf(stderr, "Return instance of class %s\n",
	       theClass->GetClassInfo()->GetName()->ExportC());
}

//
// Native Method Implementations
//
DEFINE2(newInstance0) {
  DECLARE_OBJECT(object, x0);
  DECLARE_AARRAY(array, x1);
  InstanceFieldRef *classFieldRef =
    InstanceFieldRef::FromWord(wConstructorClassFieldRef);
  if (classFieldRef == INVALID_POINTER) REQUEST(wConstructorClassFieldRef);
  InstanceFieldRef *slotFieldRef =
    InstanceFieldRef::FromWord(wConstructorSlotFieldRef);
  if (slotFieldRef == INVALID_POINTER) REQUEST(wConstructorSlotFieldRef);
  ClassObject *classObject = static_cast<ClassObject *>
    (Object::FromWord(object->GetInstanceField(classFieldRef->GetIndex())));
  Class *theClass = static_cast<Class *>(classObject->GetRepresentedType());
  u_int slot =
    JavaInt::FromWord(object->GetInstanceField(slotFieldRef->GetIndex()));
  Object *instance = Object::New(theClass);
  ReturnInstanceWorker::PushFrame(instance);
  u_int nArgs = 1;
  Scheduler::currentArgs[0] = instance->ToWord();
  //--** fill currentArgs with `array' elements, increment nArgs
  Scheduler::nArgs = nArgs == 1? Scheduler::ONE_ARG: nArgs;
  Closure *closure = theClass->GetVirtualMethod(slot);
  return Scheduler::PushCall(closure->ToWord());
} END

void NativeMethodTable::
sun_reflect_NativeConstructorAccessorImpl(JavaString *className) {
  ReturnInstanceWorker::Init();
  ClassLoader *classLoader = ClassLoader::GetBootstrapClassLoader();
  wConstructorClassFieldRef = classLoader->ResolveFieldRef
    (classLoader->ResolveClass
       (JavaString::New("java/lang/reflect/Constructor")),
     JavaString::New("clazz"), JavaString::New("Ljava/lang/Class;"));
  RootSet::Add(wConstructorClassFieldRef);
  wConstructorSlotFieldRef = classLoader->ResolveFieldRef
    (classLoader->ResolveClass
       (JavaString::New("java/lang/reflect/Constructor")),
     JavaString::New("slot"), JavaString::New("I"));
  RootSet::Add(wConstructorSlotFieldRef);
  Register(className, "newInstance0",
	   "(Ljava/lang/reflect/Constructor;[Ljava/lang/Object;)"
	   "Ljava/lang/Object;", newInstance0, 2, false);
}
