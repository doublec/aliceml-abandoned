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

#include <cstdio>

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
  virtual Result Handle(word data);
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
      ::Block *b = Store::DirectWordToBlock(args);
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
  Scheduler::PushHandler(Store::IntToWord(0));
}

Worker::Result InitializeClassWorker::Run() {
  InitializeClassFrame *frame =
    InitializeClassFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  Scheduler::PopHandler();
  frame->GetClass()->GetLock()->Release();
  frame->RestoreArgs();
  return CONTINUE;
}

Worker::Result InitializeClassWorker::Handle(word) {
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
// Type Implementation
//
ClassObject *Type::GetClassObject() {
  switch (GetLabel()) {
  case JavaLabel::Class:
    return static_cast<Class *>(this)->GetClassObject();
  case JavaLabel::PrimitiveType:
    PrimitiveType::GetClassObject
      (static_cast<PrimitiveType *>(this)->GetType());
  case JavaLabel::ArrayType:
    return static_cast<ArrayType *>(this)->GetClassObject();
  default:
    Error("invalid type");
  }
}

//
// PrimitiveType Implementation
//
static word primitiveClasses[PrimitiveType::Void + 1];

void PrimitiveType::Init() {
  for (u_int i = Void + 1; i--; ) {
    primitiveClasses[i] = null;
    RootSet::Add(primitiveClasses[i]);
  }
}

ClassObject *PrimitiveType::GetClassObject(type t) {
  if (primitiveClasses[t] == null) {
    ClassObject *classObject = ClassObject::New(New(t));
    primitiveClasses[t] = classObject->ToWord();
    return classObject;
  }
  return static_cast<ClassObject *>
    (ClassObject::FromWord(primitiveClasses[t]));
}

//
// Class Implementation
//
void Class::Init() {
  InitializeClassWorker::Init();
}

word Class::MakeMethodKey(JavaString *name, JavaString *descriptor) {
  return name->Concat(descriptor)->ToArray()->ToWord();
}

void Class::FillMethodHashTable(ChunkMap *methodHashTable) {
  ClassInfo *classInfo = GetClassInfo();
  word wSuper = classInfo->GetSuper();
  if (wSuper != null)
    Class::FromWord(wSuper)->FillMethodHashTable(methodHashTable);
  Table *methods = classInfo->GetMethods();
  ChunkMap *myMethodHashTable = GetMethodHashTable();
  for (u_int i = methods->GetCount(); i--; ) {
    MethodInfo *methodInfo = MethodInfo::FromWordDirect(methods->Get(i));
    if (!methodInfo->IsStatic()) {
      word key =
	MakeMethodKey(methodInfo->GetName(), methodInfo->GetDescriptor());
      methodHashTable->Put(key, myMethodHashTable->Get(key));
    }
  }
}

static u_int CountInterfaces(Class *theClass) {
  Assert(theClass != INVALID_POINTER);
  Table *interfaces = theClass->GetClassInfo()->GetInterfaces();
  u_int n = interfaces->GetCount();
  for (u_int i = n; i--; )
    n += CountInterfaces(Class::FromWord(interfaces->Get(i)));
  Class *superClass = theClass->GetSuperClass();
  if (superClass != INVALID_POINTER)
    n += CountInterfaces(superClass);
  return n;
}

static void FillInterfaceTable(Class *aClass, u_int &index,
			       Table *interfaceTable,
			       ChunkMap *methodHashTable,
			       Table *virtualTable) {
  Table *interfaces = aClass->GetClassInfo()->GetInterfaces();
  for (u_int i = interfaces->GetCount(); i--; ) {
    Class *interface = Class::FromWord(interfaces->Get(i));
    Assert(interface != INVALID_POINTER);
    Table *methods = interface->GetClassInfo()->GetMethods();
    u_int nMethods = methods->GetCount();
    Table *interfaceVirtualTable = Table::New(nMethods + 1);
    interfaceVirtualTable->Init(0, interface->ToWord());
    for (u_int j = nMethods; j--; ) {
      MethodInfo *methodInfo = MethodInfo::FromWordDirect(methods->Get(j));
      word wKey =
	Class::MakeMethodKey(methodInfo->GetName(),
			     methodInfo->GetDescriptor());
      if (!methodHashTable->IsMember(wKey))
	Error("NoSuchMethodError"); //--** throw
      MethodRef *methodRef =
	MethodRef::FromWordDirect(methodHashTable->Get(wKey));
      if (methodRef->GetLabel() != JavaLabel::VirtualMethodRef)
	Error("IncompatibleClassChangeError"); //--** throw
      VirtualMethodRef *virtualMethodRef =
	static_cast<VirtualMethodRef *>(methodRef);
      word wClosure = virtualTable->Get(virtualMethodRef->GetIndex());
      interfaceVirtualTable->Init(j + 1, wClosure);
    }
    interfaceTable->Init(index++, interfaceVirtualTable->ToWord());
    FillInterfaceTable(interface, index, interfaceTable,
		       methodHashTable, virtualTable);
  }
  Class *superClass = aClass->GetSuperClass();
  if (superClass != INVALID_POINTER)
    FillInterfaceTable(superClass, index, interfaceTable,
		       methodHashTable, virtualTable);
}

Class *Class::New(ClassInfo *classInfo) {
  // Precondition: parent class has already been created
  word wSuper = classInfo->GetSuper();
  Class *super = wSuper == null? INVALID_POINTER: Class::FromWord(wSuper);
  // Count number of static and instance fields:
  Table *fields = classInfo->GetFields();
  Chunk *superInstanceFieldTypes = super == INVALID_POINTER?
    INVALID_POINTER: super->GetInstanceFieldTypes();
  u_int nSuperInstanceFields = superInstanceFieldTypes == INVALID_POINTER?
    0: superInstanceFieldTypes->GetSize();
  u_int i, nStaticFields = 0, nInstanceFields = nSuperInstanceFields;
  u_int nFields = fields->GetCount();
  Chunk *instanceFieldTypes0 =
    Store::AllocChunk(nFields); //--** pessimistic assumption
  char *p = instanceFieldTypes0->GetBase(), *q = p;
  for (i = 0; i < nFields; i++) {
    FieldInfo *fieldInfo = FieldInfo::FromWordDirect(fields->Get(i));
    if (fieldInfo->IsStatic())
      nStaticFields++;
    else {
      JavaString *descriptor = fieldInfo->GetDescriptor();
      switch (descriptor->CharAt(0)) {
      case 'B': case 'C': case 'S': case 'Z': case 'I':
	*q++ = Class::t_int;
	break;
      case 'J':
	*q++ = Class::t_long;
	break;
      case 'F':
	*q++ = Class::t_float;
	break;
      case 'D':
	*q++ = Class::t_double;
	break;
      case 'L': case '[':
	*q++ = Class::t_object;
	break;
      default:
	Error("invalid descriptor");
      }
      nInstanceFields++;
    }
  }
  Chunk *instanceFieldTypes = Store::AllocChunk(nInstanceFields);
  char *r = instanceFieldTypes->GetBase();
  if (superInstanceFieldTypes != INVALID_POINTER)
    std::memcpy(r, superInstanceFieldTypes->GetBase(), nSuperInstanceFields);
  std::memcpy(r + nSuperInstanceFields, p, q - p);
  // Count number of static methods:
  Table *methods = classInfo->GetMethods();
  u_int nMethods = methods->GetCount(), nStaticMethods = 0;
  for (i = nMethods; i--; ) {
    MethodInfo *methodInfo = MethodInfo::FromWordDirect(methods->Get(i));
    if (methodInfo->IsStatic())
      nStaticMethods++;
  }
  // Allocate class:
  Block *b = Store::AllocBlock(JavaLabel::Class,
			       BASE_SIZE + nStaticFields + nStaticMethods);
  b->InitArg(CLASS_INFO_POS, classInfo->ToWord());
  Class *theClass = static_cast<Class *>(b);
  // Build method hash table:
  ChunkMap *methodHashTable = ChunkMap::New(16); //--** to be determined
  if (super != INVALID_POINTER)
    super->FillMethodHashTable(methodHashTable);
  u_int nSuperVirtualMethods = methodHashTable->GetSize();
  u_int nVirtualMethods = nSuperVirtualMethods, iIndex = 0;
  for (nStaticMethods = 0, i = 0; i < nMethods; i++) {
    MethodInfo *methodInfo = MethodInfo::FromWordDirect(methods->Get(i));
    u_int nArgs = methodInfo->GetNumberOfArguments();
    word wKey =
      MakeMethodKey(methodInfo->GetName(), methodInfo->GetDescriptor());
    word wMethodRef;
    if (methodInfo->IsStatic()) {
      Assert(!classInfo->IsInterface());
      u_int index = nStaticFields + nStaticMethods;
      wMethodRef = StaticMethodRef::New(theClass, index, nArgs)->ToWord();
      nStaticMethods++;
    } else if (methodHashTable->IsMember(wKey)) {
      // overriding a method does not contribute a new virtual table entry
      VirtualMethodRef *superMethodRef =
	VirtualMethodRef::FromWordDirect(methodHashTable->Get(wKey));
      u_int index = superMethodRef->GetIndex();
      wMethodRef = VirtualMethodRef::New(theClass, index, nArgs)->ToWord();
    } else if (classInfo->IsInterface()) {
      // interface methods contribute no virtual table entry
      wMethodRef =
	InterfaceMethodRef::New(theClass, iIndex++, nArgs)->ToWord();
    } else {
      // add new virtual table entry
      u_int index = nVirtualMethods++;
      wMethodRef = VirtualMethodRef::New(theClass, index, nArgs)->ToWord();
    }
    methodHashTable->Put(wKey, wMethodRef);
  }
  // Initialize class:
  b->InitArg(METHOD_HASH_TABLE_POS, methodHashTable->ToWord());
  b->InitArg(LOCK_POS, Lock::New()->ToWord());
  b->InitArg(INSTANCE_FIELD_TYPES_POS, instanceFieldTypes->ToWord());
  b->InitArg(CLASS_OBJECT_POS, null);
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
	  initialValue = JavaInt::ToWord(0);
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
  // Initialize static methods and construct virtual table:
  Table *virtualTable = Table::New(nVirtualMethods);
  b->InitArg(VIRTUAL_TABLE_POS, virtualTable->ToWord());
  Table *superVirtualTable = super == INVALID_POINTER?
    INVALID_POINTER: super->GetVirtualTable();
  for (i = nSuperVirtualMethods; i--; )
    virtualTable->Init(i, superVirtualTable->Get(i));
  RuntimeConstantPool *runtimeConstantPool =
    classInfo->GetRuntimeConstantPool();
  word classInitializer = null;
  JavaString *classInitializerName = JavaString::New("<clinit>");
  JavaString *classInitializerDescriptor = JavaString::New("()V");
  for (i = 0; i < nMethods; i++) {
    MethodInfo *methodInfo = MethodInfo::FromWordDirect(methods->Get(i));
    JavaString *name = methodInfo->GetName();
    JavaString *descriptor = methodInfo->GetDescriptor();
    JavaByteCode *byteCode = methodInfo->GetByteCode();
    word wClosure;
    if (byteCode != INVALID_POINTER) {
      Closure *closure = Closure::New(byteCode->ToWord(), 1);
      closure->Init(0, runtimeConstantPool->ToWord());
      wClosure = closure->ToWord();
    } else if (methodInfo->IsNative()) {
      Closure *closure =
	NativeMethodTable::Lookup(classInfo->GetName(), methodInfo->GetName(),
				  methodInfo->GetDescriptor());
      if (closure != INVALID_POINTER) {
	wClosure = closure->ToWord();
      } else {
	//--** throw LinkageError
	wClosure = String::New(classInfo->GetName()->Concat("#")->
			       Concat(methodInfo->GetName())->
			       Concat(methodInfo->GetDescriptor())->
			       ExportC())->ToWord();
      }
    } else {
      Assert(methodInfo->IsAbstract());
      wClosure = null;
    }
    if (methodInfo->IsTheMethod(classInitializerName,
				classInitializerDescriptor)) {
      Assert(methodInfo->IsStatic());
      Assert(classInitializer == null);
      classInitializer = wClosure;
    }
    word wMethodRef =
      methodHashTable->Get(MakeMethodKey(name, descriptor));
    if (methodInfo->IsStatic()) {
      StaticMethodRef *methodRef = StaticMethodRef::FromWordDirect(wMethodRef);
      Assert(methodRef->GetClass() == theClass);
      b->InitArg(BASE_SIZE + methodRef->GetIndex(), wClosure);
    } else if (!theClass->IsInterface()) {
      VirtualMethodRef *methodRef =
	VirtualMethodRef::FromWordDirect(wMethodRef);
      Assert(methodRef->GetClass() == theClass);
      virtualTable->Init(methodRef->GetIndex(), wClosure);
    }
  }
  b->InitArg(CLASS_INITIALIZER_POS, classInitializer);
  // Construct interface table:
  if (classInfo->IsInterface()) {
    b->InitArg(INTERFACE_TABLE_POS, null);
  } else {
    u_int nInterfaces = CountInterfaces(theClass);
    Table *interfaceTable = Table::New(nInterfaces);
    i = 0;
    FillInterfaceTable(theClass, i, interfaceTable,
		       methodHashTable, virtualTable);
    Assert(i == nInterfaces);
    b->InitArg(INTERFACE_TABLE_POS, interfaceTable->ToWord());
  }
  return theClass;
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
  if (wSuper == null) return INVALID_POINTER;
  Class *super = Class::FromWord(wSuper);
  Assert(super != INVALID_POINTER);
  return super;
}

Closure *Class::GetInterfaceMethod(Class *interface, u_int index) {
  Table *interfaceTable = GetInterfaceTable();
  for (u_int i = interfaceTable->GetCount(); i--; ) {
    Table *virtualTable = Table::FromWordDirect(interfaceTable->Get(i));
    if (Class::FromWord(virtualTable->Get(0)) == interface)
      return Closure::FromWordDirect(virtualTable->Get(index + 1));
  }
  return INVALID_POINTER;
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

ClassObject *Class::GetClassObject() {
  word wObject = GetArg(CLASS_OBJECT_POS);
  if (wObject != null)
    return static_cast<ClassObject *>(Object::FromWordDirect(wObject));
  ClassObject *classObject = ClassObject::New(this);
  ReplaceArg(CLASS_OBJECT_POS, classObject->ToWord());
  return classObject;
}

bool Class::Implements(Class *aClass) {
  Assert(aClass->IsInterface());
  Table *interfaceTable = GetInterfaceTable();
  for (u_int i = interfaceTable->GetCount(); i--; ) {
    Table *virtualTable = Table::FromWordDirect(interfaceTable->Get(i));
    if (Class::FromWord(virtualTable->Get(0)) == aClass)
      return true;
  }
  return false;
}

//
// ArrayType Implementation
//
ClassObject *ArrayType::GetClassObject() {
  word wObject = GetArg(CLASS_OBJECT_POS);
  if (wObject != null)
    return static_cast<ClassObject *>(Object::FromWordDirect(wObject));
  ClassObject *classObject = ClassObject::New(this);
  ReplaceArg(CLASS_OBJECT_POS, classObject->ToWord());
  return classObject;
}

//
// ClassObject Implementation
//
word ClassObject::wClass;

void ClassObject::Init() {
  wClass = ClassLoader::PreloadClass("java/lang/Class");
  RootSet::Add(wClass);
}

ClassObject *ClassObject::New(Type *type) {
  // Precondition: has not been called before for type
  Assert(type != INVALID_POINTER);
  Class *theClass = Class::FromWord(wClass);
  Assert(theClass != INVALID_POINTER);
  Object *classObject = Object::New(theClass, 1);
  u_int index = theClass->GetInstanceFieldTypes()->GetSize();
  classObject->InitInstanceField(index, type->ToWord());
  return static_cast<ClassObject *>(classObject);
}

//
// JavaString Implementation
//
word JavaString::wClass = null;

static word wInternTable;

static const u_int initialInternTableSize = 19; //--** to be determined

void JavaString::Init() {
  wClass = ClassLoader::PreloadClass("java/lang/String");
  RootSet::Add(wClass);
  wInternTable = ChunkMap::New(initialInternTableSize)->ToWord();
  RootSet::Add(wInternTable);
}

JavaString *JavaString::Intern() {
  ChunkMap *internTable = ChunkMap::FromWordDirect(wInternTable);
  BaseArray *array = ToArray();
  word key = array->ToWord();
  if (internTable->IsMember(key))
    return JavaString::FromWordDirect(internTable->Get(key));
  JavaString *result = JavaString::New(array, 0, array->GetLength());
  internTable->Put(key, result->ToWord());
  return result;
}
