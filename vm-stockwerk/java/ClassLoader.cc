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
#pragma implementation "java/ClassLoader.hh"
#endif

#include <cstdio>

#include "java/ThrowWorker.hh"
#include "java/ClassLoader.hh"
#include "java/ClassFile.hh"

//--** check loading constraints

class ClassTable: private ChunkMap {
private:
  static const u_int initialTableSize = 19; //--** to be determined
public:
  using Block::ToWord;

  static ClassTable *ClassTable::New() {
    return static_cast<ClassTable *>(ChunkMap::New(initialTableSize));
  }
  static ClassTable *FromWordDirect(word x) {
    return static_cast<ClassTable *>(ChunkMap::FromWordDirect(x));
  }

  word Lookup(JavaString *name) {
    word wName = name->ToArray()->ToWord();
    if (IsMember(wName))
      return Get(wName);
    else
      return word(0);
  }
  void Insert(JavaString *name, word wClass) {
    word wName = name->ToArray()->ToWord();
    Assert(!IsMember(wName));
    Put(wName, wClass);
  }
};

//
// PreloadWorker
//
class PreloadWorker: public Worker {
public:
  static PreloadWorker *self;
private:
  PreloadWorker() {}
public:
  static void Init() {
    self = new PreloadWorker();
  }

  static void PushFrame(Thread *thread);

  virtual u_int GetFrameSize(StackFrame *sFrame);
  virtual Result Run(StackFrame *sFrame);
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

class PreloadFrame: private StackFrame {
protected:
  enum { HOLE_POS, SIZE };
public:
  static PreloadFrame *New(Thread *thread) {
    NEW_THREAD_STACK_FRAME(frame, thread, PreloadWorker::self, SIZE);
    return static_cast<PreloadFrame *>(frame);
  }

  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
  void SetHole(Hole *hole) {
    ReplaceArg(HOLE_POS, hole->ToWord());
  }
  Hole *GetHole() {
    return static_cast<Hole *>(Store::DirectWordToTransient(GetArg(HOLE_POS)));
  }
};

PreloadWorker *PreloadWorker::self;

static word wPreloadQueue;

void PreloadWorker::PushFrame(Thread *thread) {
  PreloadFrame::New(thread);
}

u_int PreloadWorker::GetFrameSize(StackFrame *sFrame) {
  PreloadFrame *frame = static_cast<PreloadFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result PreloadWorker::Run(StackFrame *sFrame) {
  PreloadFrame *frame = static_cast<PreloadFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  if (Scheduler::nArgs == Scheduler::ONE_ARG) {
    Class *theClass = Class::FromWord(Scheduler::currentArgs[0]);
    Assert(theClass != INVALID_POINTER);
    Hole *hole = frame->GetHole();
    bool result = hole->Fill(theClass->ToWord());
    Assert(result); result = result;
  }
  Queue *preloadQueue = Queue::FromWordDirect(wPreloadQueue);
  if (preloadQueue->IsEmpty()) {
    Scheduler::PopFrame(frame->GetSize());
    Scheduler::nArgs = 0;
    return CONTINUE;
  }
  Tuple *tuple = Tuple::FromWordDirect(preloadQueue->Dequeue());
  String *string = String::FromWordDirect(tuple->Sel(0));
  JavaString *name =
    JavaString::New(reinterpret_cast<const char *>(string->GetValue()),
		    string->GetSize());
  Hole *hole =
    static_cast<Hole *>(Store::DirectWordToTransient(tuple->Sel(1)));
  frame->SetHole(hole);
  ClassLoader *classLoader = ClassLoader::GetBootstrapClassLoader();
  word wClass = classLoader->ResolveClass(name);
  Scheduler::nArgs = Scheduler::ONE_ARG;
  Scheduler::currentArgs[0] = wClass;
  if (Class::FromWord(wClass) == INVALID_POINTER) {
    Scheduler::currentData = wClass;
    return REQUEST;
  } else {
    return CONTINUE;
  }
}

const char *PreloadWorker::Identify() {
  return "PreloadWorker";
}

void PreloadWorker::DumpFrame(StackFrame *) {
  std::fprintf(stderr, "Preload classes\n");
}

//
// BuildClassWorker
//
class BuildClassWorker: public Worker {
public:
  static BuildClassWorker *self;
private:
  BuildClassWorker() {}
public:
  static void Init() {
    self = new BuildClassWorker();
  }

  static void PushFrame(ClassInfo *classInfo);

  virtual u_int GetFrameSize(StackFrame *sFrame);
  virtual Result Run(StackFrame *sFrame);
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

class BuildClassFrame: private StackFrame {
protected:
  enum { CLASS_INFO_POS, SIZE };
public:
  static BuildClassFrame *New(ClassInfo *classInfo) {
    NEW_STACK_FRAME(frame, BuildClassWorker::self, SIZE);
    frame->InitArg(CLASS_INFO_POS, classInfo->ToWord());
    return static_cast<BuildClassFrame *>(frame);
  }

  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
  ClassInfo *GetClassInfo() {
    return ClassInfo::FromWordDirect(GetArg(CLASS_INFO_POS));
  }
};

BuildClassWorker *BuildClassWorker::self;

void BuildClassWorker::PushFrame(ClassInfo *classInfo) {
  BuildClassFrame::New(classInfo);
}

u_int BuildClassWorker::GetFrameSize(StackFrame *sFrame) {
  BuildClassFrame *frame = static_cast<BuildClassFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result BuildClassWorker::Run(StackFrame *sFrame) {
  BuildClassFrame *frame = static_cast<BuildClassFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  ClassInfo *classInfo = frame->GetClassInfo();
  word wSuper = classInfo->GetSuper();
  if (Store::WordToTransient(wSuper) != INVALID_POINTER) {
    //--** detect ClassCircularityError
    Scheduler::currentData = wSuper;
    return REQUEST;
  }
  //--** if the class or interface named as the direct superclass of C is
  //--** in fact an interface, loading throws an IncompatibleClassChangeError
  Table *interfaces = classInfo->GetInterfaces();
  for (u_int i = interfaces->GetCount(); i--; ) {
    //--** detect ClassCircularityError
    word wInterface = interfaces->Get(i);
    if (Store::WordToTransient(wInterface) != INVALID_POINTER) {
      Scheduler::currentData = wInterface;
      return REQUEST;
    }
    //--** if any of the classes or interfaces named as direct
    //--** superinterfaces of C is not in fact an interface, loading
    //--** throws an IncompatibleClassChangeError
  }
  Scheduler::PopFrame(frame->GetSize());
  if (!classInfo->Verify()) {
    ThrowWorker::PushFrame(ThrowWorker::VerifyError, classInfo->GetName());
    Scheduler::nArgs = 0;
    return CONTINUE;
  }
  Scheduler::nArgs = Scheduler::ONE_ARG;
  Scheduler::currentArgs[0] = classInfo->Prepare()->ToWord();
  return CONTINUE;
}

const char *BuildClassWorker::Identify() {
  return "BuildClassWorker";
}

void BuildClassWorker::DumpFrame(StackFrame *sFrame) {
  BuildClassFrame *frame = static_cast<BuildClassFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  std::fprintf(stderr, "Build class %s\n",
	       frame->GetClassInfo()->GetName()->ExportC());
}

//
// ResolveInterpreter
//
class ResolveInterpreter: public Interpreter {
public:
  enum type {
    RESOLVE_CLASS, RESOLVE_FIELD, RESOLVE_METHOD, RESOLVE_INTERFACE_METHOD
  };
  static ResolveInterpreter *self;
private:
  ResolveInterpreter() {}

  static bool traceFlag;
public:
  static void Init() {
    self = new ResolveInterpreter();
    traceFlag = std::getenv("JAVA_TRACE_RESOLVER") != NULL;
  }

  virtual u_int GetFrameSize(StackFrame *sFrame);
  virtual Result Run(StackFrame *sFrame);
  virtual u_int GetInArity(ConcreteCode *concreteCode);
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
  virtual void PushCall(Closure *closure);
};

class ResolveFrame: private StackFrame {
protected:
  enum {
    CLASS_LOADER_POS, RESOLVE_TYPE_POS, CLASS_POS, NAME_POS, DESCRIPTOR_POS,
    SIZE
  };
public:
  static ResolveFrame *New(ClassLoader *classLoader, JavaString *name) {
    NEW_STACK_FRAME(frame, ResolveInterpreter::self, SIZE);
    frame->InitArg(CLASS_LOADER_POS, classLoader->ToWord());
    frame->InitArg(RESOLVE_TYPE_POS, ResolveInterpreter::RESOLVE_CLASS);
    frame->InitArg(NAME_POS, name->ToWord());
    return static_cast<ResolveFrame *>(frame);
  }
  static ResolveFrame *New(ClassLoader *classLoader,
			   ResolveInterpreter::type resolveType,
			   word theClass, JavaString *name,
			   JavaString *descriptor) {
    NEW_STACK_FRAME(frame, ResolveInterpreter::self, SIZE);
    frame->InitArg(CLASS_LOADER_POS, classLoader->ToWord());
    frame->InitArg(RESOLVE_TYPE_POS, resolveType);
    frame->InitArg(CLASS_POS, theClass);
    frame->InitArg(NAME_POS, name->ToWord());
    frame->InitArg(DESCRIPTOR_POS, descriptor->ToWord());
    return static_cast<ResolveFrame *>(frame);
  }

  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
  ClassLoader *GetClassLoader() {
    return ClassLoader::FromWordDirect(GetArg(CLASS_LOADER_POS));
  }
  ResolveInterpreter::type GetResolveType() {
    return static_cast<ResolveInterpreter::type>
      (Store::DirectWordToInt(GetArg(RESOLVE_TYPE_POS)));
  }
  word GetClass() {
    Assert(GetResolveType() != ResolveInterpreter::RESOLVE_CLASS);
    return GetArg(CLASS_POS);
  }
  void SetClass(Class *theClass) {
    Assert(GetResolveType() != ResolveInterpreter::RESOLVE_CLASS);
    ReplaceArg(CLASS_POS, theClass->ToWord());
  }
  JavaString *GetName() {
    return JavaString::FromWordDirect(GetArg(NAME_POS));
  }
  JavaString *GetDescriptor() {
    Assert(GetResolveType() != ResolveInterpreter::RESOLVE_CLASS);
    return JavaString::FromWordDirect(GetArg(DESCRIPTOR_POS));
  }
};

ResolveInterpreter *ResolveInterpreter::self;
bool ResolveInterpreter::traceFlag;

u_int ResolveInterpreter::GetFrameSize(StackFrame *sFrame) {
  ResolveFrame *frame = static_cast<ResolveFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result ResolveInterpreter::Run(StackFrame *sFrame) {
  ResolveFrame *frame = static_cast<ResolveFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  ClassLoader *classLoader = frame->GetClassLoader();
  switch (frame->GetResolveType()) {
  case RESOLVE_CLASS:
    {
      JavaString *name = frame->GetName();
      if (traceFlag)
	std::fprintf(stderr, "resolving class %s\n", name->ExportC());
      if (name->CharAt(0) == '[') { // special handling of array types
	Type *componentType = INVALID_POINTER;
	switch (name->CharAt(1)) {
	case 'B':
	  componentType =
	    static_cast<Type *>(PrimitiveType::New(PrimitiveType::Byte));
	  break;
	case 'C':
	  componentType =
	    static_cast<Type *>(PrimitiveType::New(PrimitiveType::Char));
	  break;
	case 'D':
	  componentType =
	    static_cast<Type *>(PrimitiveType::New(PrimitiveType::Double));
	  break;
	case 'F':
	  componentType =
	    static_cast<Type *>(PrimitiveType::New(PrimitiveType::Float));
	  break;
	case 'I':
	  componentType =
	    static_cast<Type *>(PrimitiveType::New(PrimitiveType::Int));
	  break;
	case 'J':
	  componentType =
	    static_cast<Type *>(PrimitiveType::New(PrimitiveType::Long));
	  break;
	case 'S':
	  componentType =
	    static_cast<Type *>(PrimitiveType::New(PrimitiveType::Short));
	  break;
	case 'Z':
	  componentType =
	    static_cast<Type *>(PrimitiveType::New(PrimitiveType::Boolean));
	  break;
	case 'L':
	  Assert(name->CharAt(name->GetLength() - 1) == ';');
	  name = name->Substring(2, name->GetLength() - 1);
	  break;
	case '[':
	  name = name->Substring(1, name->GetLength());
	  break;
	default:
	  Error("invalid descriptor");
	}
	if (componentType == INVALID_POINTER) {
	  // resolve and request component type:
	  word wComponentType = classLoader->ResolveClass(name);
	  componentType = Type::FromWord(wComponentType);
	  if (componentType == INVALID_POINTER) {
	    Scheduler::currentData = wComponentType;
	    return REQUEST;
	  }
	}
	Scheduler::PopFrame(frame->GetSize());
	Scheduler::nArgs = Scheduler::ONE_ARG;
	Scheduler::currentArgs[0] = ArrayType::New(componentType)->ToWord();
	return CONTINUE;
      }
      // Resolve from class file:
      JavaString *filename = name->Concat(JavaString::New(".class"));
      ClassFile *classFile = ClassFile::NewFromFile(filename);
      if (classFile == INVALID_POINTER) {
	ThrowWorker::PushFrame(ThrowWorker::NoClassDefFoundError, name);
	Scheduler::nArgs = 0;
	return CONTINUE;
      }
      ClassInfo *classInfo = classFile->Parse(classLoader);
      if (classInfo == INVALID_POINTER) {
	ThrowWorker::PushFrame(ThrowWorker::ClassFormatError, name);
	Scheduler::nArgs = 0;
	return CONTINUE;
      }
      if (!classInfo->GetName()->Equals(name)) {
	ThrowWorker::PushFrame(ThrowWorker::NoClassDefFoundError, name);
	Scheduler::nArgs = 0;
	return CONTINUE;
      }
      Scheduler::PopFrame(frame->GetSize());
      BuildClassWorker::PushFrame(classInfo);
      return CONTINUE;
    }
  case RESOLVE_FIELD:
    {
      word wClass = frame->GetClass();
      Class *theClass = Class::FromWord(wClass);
      if (theClass == INVALID_POINTER) {
	Scheduler::currentData = wClass;
	return REQUEST;
      }
      JavaString *name = frame->GetName();
      JavaString *descriptor = frame->GetDescriptor();
      if (traceFlag)
	std::fprintf(stderr, "resolving field %s#%s:%s\n",
		     theClass->GetClassInfo()->GetName()->ExportC(),
		     name->ExportC(), descriptor->ExportC());
      //--** look for field definitions in implemented interfaces
      ClassInfo *classInfo = theClass->GetClassInfo();
      Table *fields = classInfo->GetFields();
      u_int sIndex = 0, iIndex = 0, nFields = fields->GetCount();
      word wSuper = classInfo->GetSuper();
      if (wSuper != null)
	iIndex = Class::FromWord(wSuper)->GetInstanceFieldTypes()->GetSize();
      for (u_int i = 0; i < nFields; i++) {
	FieldInfo *fieldInfo = FieldInfo::FromWordDirect(fields->Get(i));
	if (fieldInfo->IsTheField(name, descriptor)) {
	  u_int nSlots = fieldInfo->GetNumberOfRequiredSlots();
	  Scheduler::PopFrame(frame->GetSize());
	  Scheduler::nArgs = Scheduler::ONE_ARG;
	  Scheduler::currentArgs[0] = fieldInfo->IsStatic()?
	    StaticFieldRef::New(theClass, sIndex, nSlots)->ToWord():
	    InstanceFieldRef::New(iIndex, nSlots)->ToWord();
	  return CONTINUE;
	} else {
	  if (fieldInfo->IsStatic())
	    sIndex++;
	  else
	    iIndex++;
	}
      }
      if (wSuper == null) {
	ThrowWorker::PushFrame(ThrowWorker::NoSuchFieldError, name);
	Scheduler::nArgs = 0;
	return CONTINUE;
      }
      frame->SetClass(Class::FromWord(wSuper));
      return CONTINUE;
    }
  case RESOLVE_METHOD:
    {
      word wClass = frame->GetClass();
      Class *theClass = Class::FromWord(wClass);
      if (theClass == INVALID_POINTER) {
	Scheduler::currentData = wClass;
	return REQUEST;
      }
      ClassInfo *classInfo = theClass->GetClassInfo();
      if (classInfo->IsInterface()) {
	ThrowWorker::PushFrame(ThrowWorker::IncompatibleClassChangeError,
			       classInfo->GetName());
	Scheduler::nArgs = 0;
	return CONTINUE;
      }
      JavaString *name = frame->GetName();
      JavaString *descriptor = frame->GetDescriptor();
      if (traceFlag)
	std::fprintf(stderr, "resolving method %s#%s%s\n",
		     theClass->GetClassInfo()->GetName()->ExportC(),
		     name->ExportC(), descriptor->ExportC());
      ChunkMap *methodChunkMap = theClass->GetMethodHashTable();
      word wKey = Class::MakeMethodKey(name, descriptor);
      if (methodChunkMap->IsMember(wKey)) {
	word wMethodRef = methodChunkMap->Get(wKey);
	//--** is the method is abstract, but C is not abstract,
	//--** throw AbstractMethodError
	Scheduler::PopFrame(frame->GetSize());
	Scheduler::nArgs = Scheduler::ONE_ARG;
	Scheduler::currentArgs[0] = wMethodRef;
	return CONTINUE;
      }
      word wSuper = classInfo->GetSuper();
      if (wSuper == null) {
	//--** we must attempt to locate the method in the superinterfaces
	//--** of the original class
	ThrowWorker::PushFrame(ThrowWorker::NoSuchMethodError, name);
	Scheduler::nArgs = 0;
	return CONTINUE;
      }
      frame->SetClass(Class::FromWord(wSuper));
      return CONTINUE;
    }
  case RESOLVE_INTERFACE_METHOD:
    {
      word wClass = frame->GetClass();
      Class *theClass = Class::FromWord(wClass);
      if (theClass == INVALID_POINTER) {
	Scheduler::currentData = wClass;
	return REQUEST;
      }
      ClassInfo *classInfo = theClass->GetClassInfo();
      if (!classInfo->IsInterface()) {
	ThrowWorker::PushFrame(ThrowWorker::IncompatibleClassChangeError,
			       classInfo->GetName());
	Scheduler::nArgs = 0;
	return CONTINUE;
      }
      JavaString *name = frame->GetName();
      JavaString *descriptor = frame->GetDescriptor();
      if (traceFlag)
	std::fprintf(stderr, "resolving interface method %s#%s%s\n",
		     theClass->GetClassInfo()->GetName()->ExportC(),
		     name->ExportC(), descriptor->ExportC());
      ChunkMap *methodChunkMap = theClass->GetMethodHashTable();
      word wKey = Class::MakeMethodKey(name, descriptor);
      if (methodChunkMap->IsMember(wKey)) {
	word wMethodRef = methodChunkMap->Get(wKey);
	//--** is the method is abstract, but C is not abstract,
	//--** throw AbstractMethodError
	Assert(MethodRef::FromWordDirect(wMethodRef)->GetLabel() ==
	       JavaLabel::InterfaceMethodRef);
	Scheduler::PopFrame(frame->GetSize());
	Scheduler::nArgs = Scheduler::ONE_ARG;
	Scheduler::currentArgs[0] = wMethodRef;
	return CONTINUE;
      }
      //--** we must attempt to locate the method in the superinterfaces
      //--** of the original class, and in super (class Object)
      ThrowWorker::PushFrame(ThrowWorker::NoSuchMethodError, name);
      Scheduler::nArgs = 0;
      return CONTINUE;
    }
  default:
    Error("invalid resolution type");
  }
}

u_int ResolveInterpreter::GetInArity(ConcreteCode *) {
  return 0;
}

void ResolveInterpreter::PushCall(Closure *closure) {
  ClassLoader *classLoader = ClassLoader::FromWordDirect(closure->Sub(0));
  ResolveInterpreter::type resolveType =
    static_cast<ResolveInterpreter::type>
    (Store::DirectWordToInt(closure->Sub(1)));
  if (resolveType == ResolveInterpreter::RESOLVE_CLASS) {
    JavaString *name = JavaString::FromWordDirect(closure->Sub(2));
    ResolveFrame::New(classLoader, name);
  } else {
    word theClass = closure->Sub(2);
    JavaString *name = JavaString::FromWordDirect(closure->Sub(3));
    JavaString *descriptor = JavaString::FromWordDirect(closure->Sub(4));
    ResolveFrame::New(classLoader, resolveType, theClass, name, descriptor);
  }
}

const char *ResolveInterpreter::Identify() {
  return "ResolveInterpreter";
}

void ResolveInterpreter::DumpFrame(StackFrame *sFrame) {
  ResolveFrame *frame = static_cast<ResolveFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  std::fprintf(stderr, "Resolve class %s\n", frame->GetName()->ExportC());
}

//
// ClassLoader Implementation
//
word ClassLoader::wBootstrapClassLoader;

static const u_int PRELOAD_QUEUE_INITIAL_SIZE = 2; //--** to be determined

void ClassLoader::Init() {
  wBootstrapClassLoader = ClassLoader::New()->ToWord();
  RootSet::Add(wBootstrapClassLoader);
  wPreloadQueue = Queue::New(PRELOAD_QUEUE_INITIAL_SIZE)->ToWord();
  RootSet::Add(wPreloadQueue);
  PreloadWorker::Init();
  BuildClassWorker::Init();
  ResolveInterpreter::Init();
}

ClassLoader *ClassLoader::New() {
  Block *b = Store::AllocBlock(JavaLabel::ClassLoader, SIZE);
  b->InitArg(CLASS_TABLE_POS, ClassTable::New()->ToWord());
  return static_cast<ClassLoader *>(b);
}

ClassTable *ClassLoader::GetClassTable() {
  return ClassTable::FromWordDirect(GetArg(CLASS_TABLE_POS));
}

word ClassLoader::PreloadClass(const char *name) {
  Hole *hole = Hole::New();
  Tuple *tuple = Tuple::New(2);
  tuple->Init(0, String::New(name)->ToWord());
  tuple->Init(1, hole->ToWord());
  Queue::FromWordDirect(wPreloadQueue)->Enqueue(tuple->ToWord());
  return hole->ToWord();
}

void ClassLoader::PushPreloadFrame(Thread *thread) {
  PreloadWorker::PushFrame(thread);
}

word ClassLoader::ResolveClass(JavaString *name) {
  ClassTable *classTable = GetClassTable();
  word wClass = classTable->Lookup(name);
  if (wClass == (word) 0) {
    ConcreteCode *concreteCode =
      ConcreteCode::New(ResolveInterpreter::self, 0);
    Closure *closure = Closure::New(concreteCode->ToWord(), 3);
    closure->Init(0, ToWord());
    closure->Init(1, Store::IntToWord(ResolveInterpreter::RESOLVE_CLASS));
    closure->Init(2, name->ToWord());
    wClass = Byneed::New(closure->ToWord())->ToWord();
    classTable->Insert(name, wClass);
  }
  return wClass;
}

word ClassLoader::ResolveFieldRef(word theClass, JavaString *name,
				  JavaString *descriptor) {
  ConcreteCode *concreteCode = ConcreteCode::New(ResolveInterpreter::self, 0);
  Closure *closure = Closure::New(concreteCode->ToWord(), 5);
  closure->Init(0, ToWord());
  closure->Init(1, Store::IntToWord(ResolveInterpreter::RESOLVE_FIELD));
  closure->Init(2, theClass);
  closure->Init(3, name->ToWord());
  closure->Init(4, descriptor->ToWord());
  return Byneed::New(closure->ToWord())->ToWord();
}

word ClassLoader::ResolveMethodRef(word theClass, JavaString *name,
				   JavaString *descriptor) {
  ConcreteCode *concreteCode = ConcreteCode::New(ResolveInterpreter::self, 0);
  Closure *closure = Closure::New(concreteCode->ToWord(), 5);
  closure->Init(0, ToWord());
  closure->Init(1, Store::IntToWord(ResolveInterpreter::RESOLVE_METHOD));
  closure->Init(2, theClass);
  closure->Init(3, name->ToWord());
  closure->Init(4, descriptor->ToWord());
  return Byneed::New(closure->ToWord())->ToWord();
}

word ClassLoader::ResolveInterfaceMethodRef(word theClass, JavaString *name,
					    JavaString *descriptor) {
  ConcreteCode *concreteCode = ConcreteCode::New(ResolveInterpreter::self, 0);
  Closure *closure = Closure::New(concreteCode->ToWord(), 5);
  closure->Init(0, ToWord());
  u_int resolveType = ResolveInterpreter::RESOLVE_INTERFACE_METHOD;
  closure->Init(1, Store::IntToWord(resolveType));
  closure->Init(2, theClass);
  closure->Init(3, name->ToWord());
  closure->Init(4, descriptor->ToWord());
  return Byneed::New(closure->ToWord())->ToWord();
}
