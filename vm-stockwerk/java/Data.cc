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
#pragma implementation "java/Data.hh"
#endif

#include "generic/RootSet.hh"
#include "generic/String.hh"
#include "generic/Backtrace.hh"
#include "java/ClassInfo.hh"
#include "java/StackFrame.hh"
#include "java/NativeMethodTable.hh"

//
// InitializeClassWorker
//
class InitializeClassWorker: public Worker {
public:
  static InitializeClassWorker *self;
private:
  InitializeClassWorker() {}
public:
  static void Init() {
    self = new InitializeClassWorker();
  }

  static void PushFrame(Class *theClass, u_int nArgs, word args);

  virtual Result Run();
  virtual Result Handle();
  virtual const char *Identify();
  virtual void DumpFrame(word wFrame);
};

class InitializeClassFrame: private StackFrame {
protected:
  enum { CLASS_POS, NARGS_POS, ARGS_POS, SIZE };
public:
  using Block::ToWord;

  static InitializeClassFrame *New(Class *theClass, u_int nArgs, word args) {
    StackFrame *frame = StackFrame::New(INITIALIZE_CLASS_FRAME,
					InitializeClassWorker::self, SIZE);
    frame->InitArg(CLASS_POS, theClass->ToWord());
    frame->InitArg(NARGS_POS, nArgs);
    frame->InitArg(ARGS_POS, args);
    return static_cast<InitializeClassFrame *>(frame);
  }
  static InitializeClassFrame *FromWordDirect(word x) {
    StackFrame *frame = StackFrame::FromWordDirect(x);
    Assert(frame->GetLabel() == INITIALIZE_CLASS_FRAME);
    return static_cast<InitializeClassFrame *>(frame);
  }

  Class *GetClass() {
    return Class::FromWordDirect(GetArg(CLASS_POS));
  }
  void RestoreArgs() {
    Scheduler::nArgs = Store::DirectWordToInt(GetArg(NARGS_POS));
    word args = GetArg(ARGS_POS);
    switch (Scheduler::nArgs) {
    case 0:
      break;
    case Scheduler::ONE_ARG:
      Scheduler::currentArgs[0] = args;
      break;
    default:
      Block *b = Store::DirectWordToBlock(args);
      Assert(b->GetLabel() == ARGS_LABEL);
      for (u_int i = Scheduler::nArgs; i--; )
	Scheduler::currentArgs[i] = b->GetArg(i);
      break;
    }
  }
};

InitializeClassWorker *InitializeClassWorker::self;

void InitializeClassWorker::PushFrame(Class *theClass, u_int nArgs,
				      word args) {
  InitializeClassFrame *frame =
    InitializeClassFrame::New(theClass, nArgs, args);
  Scheduler::PushFrame(frame->ToWord());
}

Worker::Result InitializeClassWorker::Run() {
  InitializeClassFrame *frame =
    InitializeClassFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  frame->GetClass()->GetLock()->Release();
  frame->RestoreArgs();
  return CONTINUE;
}

Worker::Result InitializeClassWorker::Handle() {
  InitializeClassFrame *frame =
    InitializeClassFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  Class *theClass = frame->GetClass();
  theClass->GetLock()->Release();
  Scheduler::currentBacktrace->Dump();
  Error("static initializer raised an exception");
  //--** mark theClass as unusable (initialization failed)
  Scheduler::currentBacktrace->Enqueue(frame->ToWord());
  return RAISE;
}

const char *InitializeClassWorker::Identify() {
  return "InitializeClassWorker";
}

void InitializeClassWorker::DumpFrame(word wFrame) {
  InitializeClassFrame *frame = InitializeClassFrame::FromWordDirect(wFrame);
  Class *theClass = frame->GetClass();
  std::fprintf(stderr, "Initialize class %s\n",
	       theClass->GetClassInfo()->GetName()->ExportC());
}

//
// Class Implementation
//
void Class::Init() {
  InitializeClassWorker::Init();
}

Class *Class::New(ClassInfo *classInfo) {
  // Precondition: parent class has already been created
  word wSuper = classInfo->GetSuper();
  Class *super = wSuper == Store::IntToWord(0)?
    INVALID_POINTER: Class::FromWord(wSuper);
  // Count number of static and instance fields:
  Table *fields = classInfo->GetFields();
  u_int nSuperInstanceFields = super == INVALID_POINTER? 0:
    super->GetNumberOfInstanceFields();
  u_int i, nStaticFields = 0, nInstanceFields = nSuperInstanceFields;
  u_int nFields = fields->GetCount();
  for (i = nFields; i--; ) {
    FieldInfo *fieldInfo = FieldInfo::FromWordDirect(fields->Get(i));
    if (fieldInfo->IsStatic())
      nStaticFields++;
    else
      nInstanceFields++;
  }
  // Count number of static and virtual methods:
  Table *methods = classInfo->GetMethods();
  u_int nSuperVirtualMethods = super == INVALID_POINTER? 0:
    super->GetNumberOfVirtualMethods();
  u_int nStaticMethods = 0, nVirtualMethods = nSuperVirtualMethods;
  u_int nMethods = methods->GetCount();
  for (i = nMethods; i--; ) {
    MethodInfo *methodInfo = MethodInfo::FromWordDirect(methods->Get(i));
    if (methodInfo->IsStatic())
      nStaticMethods++;
    else
      nVirtualMethods++;
  }
  // Construct virtual table:
  Block *virtualTable =
    Store::AllocBlock(JavaLabel::VirtualTable, nVirtualMethods);
  Block *superVirtualTable = super == INVALID_POINTER? INVALID_POINTER:
    super->GetVirtualTable();
  for (i = nSuperVirtualMethods; i--; )
    virtualTable->InitArg(i, superVirtualTable->GetArg(i));
  // Allocate class proper:
  Block *b = Store::AllocBlock(JavaLabel::Class,
			       BASE_SIZE + nStaticFields + nStaticMethods);
  b->InitArg(CLASS_INFO_POS, classInfo->ToWord());
  b->InitArg(NUMBER_OF_VIRTUAL_METHODS_POS, nVirtualMethods);
  b->InitArg(NUMBER_OF_INSTANCE_FIELDS_POS, nInstanceFields);
  b->InitArg(VIRTUAL_TABLE_POS, virtualTable->ToWord());
  b->InitArg(LOCK_POS, Lock::New()->ToWord());
  // Initialize static fields:
  i = 0, nStaticFields = 0;
  while (i < nFields) {
    FieldInfo *fieldInfo = FieldInfo::FromWordDirect(fields->Get(i));
    if (fieldInfo->IsStatic()) {
      word initialValue;
      if (fieldInfo->HasConstantValue())
	initialValue = fieldInfo->GetConstantValue();
      else {
	JavaString *descriptor = fieldInfo->GetDescriptor();
	switch (descriptor->CharAt(0)) {
	case '[': case 'L':
	  initialValue = null;
	  break;
	case 'B': case 'C': case 'I': case 'S': case 'Z':
	  initialValue = Store::IntToWord(0);
	  break;
	case 'J':
	  initialValue = JavaLong::New(0, 0)->ToWord();
	  break;
	case 'F':
	  initialValue = Float::New(0.0)->ToWord();
	  break;
	case 'D':
	  initialValue = Double::New(0.0L)->ToWord();
	  break;
	default:
	  Error("invalid field descriptor");
	}
	b->InitArg(BASE_SIZE + nStaticFields, initialValue);
      }
      nStaticFields++;
    }
    i++;
  }
  // Create method closures:
  RuntimeConstantPool *runtimeConstantPool =
    classInfo->GetRuntimeConstantPool();
  i = 0, nStaticMethods = 0, nVirtualMethods = 0;
  Closure *classInitializer = INVALID_POINTER;
  JavaString *classInitializerName = JavaString::New("<clinit>");
  JavaString *classInitializerDescriptor = JavaString::New("()V");
  while (i < nMethods) {
    MethodInfo *methodInfo = MethodInfo::FromWordDirect(methods->Get(i));
    JavaByteCode *byteCode = methodInfo->GetByteCode();
    if (byteCode != INVALID_POINTER) {
      Closure *closure = Closure::New(byteCode->ToWord(), 1);
      closure->Init(0, runtimeConstantPool->ToWord());
      if (methodInfo->IsStatic()) {
	b->InitArg(BASE_SIZE + nStaticFields + nStaticMethods,
		   closure->ToWord());
	nStaticMethods++;
	if (methodInfo->IsTheMethod(classInitializerName,
				    classInitializerDescriptor)) {
	  Assert(classInitializer == INVALID_POINTER);
	  classInitializer = closure;
	}
      } else {
	virtualTable->InitArg(nSuperVirtualMethods + nVirtualMethods,
			      closure->ToWord());
	nVirtualMethods++;
      }
    } else if (methodInfo->IsNative()) {
      Closure *closure =
	NativeMethodTable::Lookup(classInfo->GetName(), methodInfo->GetName(),
				  methodInfo->GetDescriptor());
      if (closure != INVALID_POINTER) {
	if (methodInfo->IsStatic()) {
	  b->InitArg(BASE_SIZE + nStaticFields + nStaticMethods,
		     closure->ToWord());
	  nStaticMethods++;
	  if (methodInfo->IsTheMethod(classInitializerName,
				      classInitializerDescriptor)) {
	    Assert(classInitializer == INVALID_POINTER);
	    classInitializer = closure;
	  }
	} else {
	  virtualTable->InitArg(nSuperVirtualMethods + nVirtualMethods,
				closure->ToWord());
	  nVirtualMethods++;
	}
      } else {
	word foo = String::New(classInfo->GetName()->Concat("#")->Concat(methodInfo->GetName())->Concat(methodInfo->GetDescriptor())->ExportC())->ToWord();
	; //--** Error("LinkageError"); //--** throw
	if (methodInfo->IsStatic()) {
	  b->InitArg(BASE_SIZE + nStaticFields + nStaticMethods, foo);
	  nStaticMethods++;
	} else {
	  virtualTable->InitArg(nSuperVirtualMethods + nVirtualMethods, foo);
	  nVirtualMethods++;
	}
      }
    } else { // is abstract
      Assert(!methodInfo->IsStatic());
      nVirtualMethods++;
    }
    i++;
  }
  b->InitArg(CLASS_INITIALIZER_POS,
	     classInitializer == INVALID_POINTER?
	     null: classInitializer->ToWord());
  return static_cast<Class *>(b);
}

//--** these should be defined in Data.hh
ClassInfo *Class::GetClassInfo() {
  return ClassInfo::FromWordDirect(GetArg(CLASS_INFO_POS));
}

bool Class::IsInterface() {
  return GetClassInfo()->IsInterface();
}

Class *Class::GetSuperClass() {
  word wSuper = GetClassInfo()->GetSuper();
  if (wSuper == Store::IntToWord(0)) return INVALID_POINTER;
  Class *super = Class::FromWord(wSuper);
  Assert(super != INVALID_POINTER);
  return super;
}

Worker::Result Class::RunInitializer() {
  word wClassInitializer = GetArg(CLASS_INITIALIZER_POS);
  Assert(wClassInitializer != null);
  GetLock()->AssertAcquired();
  ReplaceArg(CLASS_INITIALIZER_POS, 0);
  word args;
  switch (Scheduler::nArgs) {
  case 0:
    args = Store::IntToWord(0);
    break;
  case Scheduler::ONE_ARG:
    args = Scheduler::currentArgs[0];
    break;
  default:
    Block *b = Store::AllocBlock(ARGS_LABEL, Scheduler::nArgs);
    for (u_int i = Scheduler::nArgs; i--; )
      b->InitArg(i, Scheduler::currentArgs[i]);
    args = b->ToWord();
    break;
  }
  InitializeClassWorker::PushFrame(this, Scheduler::nArgs, args);
  return Scheduler::PushCall(wClassInitializer);
}

//
// JavaString Implementation
//
word JavaString::wClass = Store::IntToWord(0);

static word wInternTable;

static const u_int initialInternTableSize = 19; //--** to be determined

void JavaString::Init() {
  //--** not nice: this creates a JavaString before wClass is initialized
  ClassLoader *classLoader = ClassLoader::GetBootstrapClassLoader();
  wClass = classLoader->ResolveClass(JavaString::New("java/lang/String"));
  RootSet::Add(wClass);
  wInternTable =
    HashTable::New(HashTable::BLOCK_KEY, initialInternTableSize)->ToWord();
  RootSet::Add(wInternTable);
}

JavaString *JavaString::Intern() {
  HashTable *internTable = HashTable::FromWordDirect(wInternTable);
  BaseArray *array = ToArray();
  word key = array->ToWord();
  if (internTable->IsMember(key))
    return JavaString::FromWordDirect(internTable->GetItem(key));
  JavaString *result = JavaString::New(array, 0, array->GetLength());
  internTable->InsertItem(key, result->ToWord());
  return result;
}
