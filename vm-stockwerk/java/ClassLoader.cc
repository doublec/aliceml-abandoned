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
#include "adt/HashTable.hh"
#include "generic/Transients.hh"
#include "generic/ConcreteCode.hh"
#include "generic/Interpreter.hh"
#include "java/StackFrame.hh"
#include "java/ClassLoader.hh"
#include "java/ClassFile.hh"

//--** check loading constraints

class ClassTable: private HashTable {
private:
  static const u_int initialTableSize = 19; //--** to be determined
public:
  using Block::ToWord;

  static ClassTable *ClassTable::New() {
    return static_cast<ClassTable *>
      (HashTable::New(HashTable::BLOCK_KEY, initialTableSize));
  }
  static ClassTable *FromWordDirect(word x) {
    return static_cast<ClassTable *>(HashTable::FromWordDirect(x));
  }

  word Lookup(JavaString *name) {
    word wName = name->ToWord();
    if (IsMember(wName))
      return GetItem(wName);
    else
      return word(0);
  }
  void Insert(JavaString *name, word wClass) {
    word wName = name->ToWord();
    Assert(!IsMember(wName));
    InsertItem(wName, wClass);
  }
};

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

  virtual Result Run();
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

class BuildClassFrame: private StackFrame {
protected:
  enum { CLASS_INFO_POS, SIZE };
public:
  using Block::ToWord;

  static BuildClassFrame *New(ClassInfo *classInfo) {
    StackFrame *frame =
      StackFrame::New(BUILD_CLASS_FRAME, BuildClassWorker::self, SIZE);
    frame->InitArg(CLASS_INFO_POS, classInfo->ToWord());
    return static_cast<BuildClassFrame *>(frame);
  }
  static BuildClassFrame *FromWordDirect(word x) {
    StackFrame *frame = StackFrame::FromWordDirect(x);
    Assert(frame->GetLabel() == BUILD_CLASS_FRAME);
    return static_cast<BuildClassFrame *>(frame);
  }

  ClassInfo *GetClassInfo() {
    return ClassInfo::FromWordDirect(GetArg(CLASS_INFO_POS));
  }
};

BuildClassWorker *BuildClassWorker::self;

void BuildClassWorker::PushFrame(ClassInfo *classInfo) {
  Scheduler::PushFrame(BuildClassFrame::New(classInfo)->ToWord());
}

Worker::Result BuildClassWorker::Run() {
  BuildClassFrame *frame =
    BuildClassFrame::FromWordDirect(Scheduler::GetFrame());
  ClassInfo *classInfo = frame->GetClassInfo();
  word wSuper = classInfo->GetSuper();
  if (Store::WordToTransient(wSuper) != INVALID_POINTER) {
    //--** detect ClassCircularityError
    Scheduler::currentData = wSuper;
    return Worker::REQUEST;
  }
  //--** if the class or interface named as the direct superclass of C is
  //--** in fact an interface, loading throws an IncompatibleClassChangeError
  Table *interfaces = classInfo->GetInterfaces();
  for (u_int i = interfaces->GetCount(); i--; ) {
    //--** detect ClassCircularityError
    word wInterface = interfaces->Get(i);
    if (Store::WordToTransient(wInterface) != INVALID_POINTER) {
      Scheduler::currentData = wInterface;
      return Worker::REQUEST;
    }
    //--** if any of the classes or interfaces named as direct
    //--** superinterfaces of C is not in fact an interface, loading
    //--** throws an IncompatibleClassChangeError
  }
  if (!classInfo->Verify())
    Error("VerifyError"); //--** raise VerifyError
  Scheduler::PopFrame();
  Scheduler::nArgs = Scheduler::ONE_ARG;
  Scheduler::currentArgs[0] = classInfo->Prepare()->ToWord();
  //--** run static initializer <clinit>
  return Worker::CONTINUE;
}

const char *BuildClassWorker::Identify() {
  return "BuildClassWorker";
}

void BuildClassWorker::DumpFrame(word frame) {
  BuildClassFrame *buildClassFrame = BuildClassFrame::FromWordDirect(frame);
  std::fprintf(stderr, "Build class %s\n",
	       buildClassFrame->GetClassInfo()->GetName()->ExportC());
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
public:
  static void Init() {
    self = new ResolveInterpreter();
  }

  virtual Result Run();
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
  virtual void PushCall(Closure *closure);
};

class ResolveFrame: private StackFrame {
protected:
  enum {
    CLASS_LOADER_POS, RESOLVE_TYPE_POS, CLASS_POS, NAME_POS, DESCRIPTOR_POS,
    SIZE
  };
public:
  using Block::ToWord;

  static ResolveFrame *New(ClassLoader *classLoader, JavaString *name) {
    StackFrame *frame =
      StackFrame::New(RESOLVE_FRAME, ResolveInterpreter::self, SIZE);
    frame->InitArg(CLASS_LOADER_POS, classLoader->ToWord());
    frame->InitArg(RESOLVE_TYPE_POS, ResolveInterpreter::RESOLVE_CLASS);
    frame->InitArg(NAME_POS, name->ToWord());
    return static_cast<ResolveFrame *>(frame);
  }
  static ResolveFrame *New(ClassLoader *classLoader,
			   ResolveInterpreter::type resolveType,
			   word theClass, JavaString *name,
			   JavaString *descriptor) {
    StackFrame *frame =
      StackFrame::New(RESOLVE_FRAME, ResolveInterpreter::self, SIZE);
    frame->InitArg(CLASS_LOADER_POS, classLoader->ToWord());
    frame->InitArg(RESOLVE_TYPE_POS, resolveType);
    frame->InitArg(CLASS_POS, theClass);
    frame->InitArg(NAME_POS, name->ToWord());
    frame->InitArg(DESCRIPTOR_POS, descriptor->ToWord());
    return static_cast<ResolveFrame *>(frame);
  }
  static ResolveFrame *FromWordDirect(word x) {
    StackFrame *frame = StackFrame::FromWordDirect(x);
    Assert(frame->GetLabel() == RESOLVE_FRAME);
    return static_cast<ResolveFrame *>(frame);
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

Worker::Result ResolveInterpreter::Run() {
  ResolveFrame *frame = ResolveFrame::FromWordDirect(Scheduler::GetFrame());
  switch (frame->GetResolveType()) {
  case RESOLVE_CLASS:
    {
      JavaString *name = frame->GetName();
      std::fprintf(stderr, "resolving class %s\n", name->ExportC());
      JavaString *filename = name->Concat(JavaString::New(".class"));
      ClassFile *classFile = ClassFile::NewFromFile(filename);
      if (classFile == INVALID_POINTER)
	Error("NoClassDefFoundError"); //--** raise NoClassDefFoundError
      ClassInfo *classInfo = classFile->Parse(frame->GetClassLoader());
      if (classInfo == INVALID_POINTER)
	Error("ClassFormatError"); //--** raise ClassFormatError
      if (!classInfo->GetName()->Equals(name))
	Error("NoClassDefFoundError"); //--** raise NoClassDefFoundError
      Scheduler::PopFrame();
      BuildClassWorker::PushFrame(classInfo);
      return Worker::CONTINUE;
    }
  case RESOLVE_FIELD:
    {
      word wClass = frame->GetClass();
      Class *theClass = Class::FromWord(wClass);
      if (theClass == INVALID_POINTER) {
	Scheduler::currentData = wClass;
	return Worker::REQUEST;
      }
      JavaString *name = frame->GetName();
      JavaString *descriptor = frame->GetDescriptor();
      //--** look for field definitions in implemented interfaces
      ClassInfo *classInfo = theClass->GetClassInfo();
      Table *fields = classInfo->GetFields();
      u_int sIndex = 0, iIndex = 0, nFields = fields->GetCount();
      for (u_int i = 0; i < nFields; i++) {
	FieldInfo *fieldInfo = FieldInfo::FromWordDirect(fields->Get(i));
	if (fieldInfo->IsTheField(name, descriptor)) {
	  Scheduler::PopFrame();
	  Scheduler::nArgs = Scheduler::ONE_ARG;
	  Scheduler::currentArgs[0] = fieldInfo->IsStatic()?
	    StaticFieldRef::New(theClass, sIndex)->ToWord():
	    InstanceFieldRef::New(iIndex)->ToWord();
	  return Worker::CONTINUE;
	} else {
	  if (fieldInfo->IsStatic())
	    sIndex++;
	  else
	    iIndex++;
	}
      }
      word wSuper = classInfo->GetSuper();
      if (wSuper == Store::IntToWord(0))
	Error("NoSuchField"); //--** raise
      frame->SetClass(Class::FromWord(wSuper));
      return CONTINUE;
    }
  case RESOLVE_METHOD:
    {
      word wClass = frame->GetClass();
      Class *theClass = Class::FromWord(wClass);
      if (theClass == INVALID_POINTER) {
	Scheduler::currentData = wClass;
	return Worker::REQUEST;
      }
      JavaString *name = frame->GetName();
      JavaString *descriptor = frame->GetDescriptor();
      std::fprintf(stderr, "resolving method %s#%s%s\n",
		   theClass->GetClassInfo()->GetName()->ExportC(),
		   name->ExportC(), descriptor->ExportC());
      //--** if the method is defined in one of the superinterfaces,
      //--** raise IncompatibleClassChangeError
      ClassInfo *classInfo = theClass->GetClassInfo();
      Table *methods = classInfo->GetMethods();
      u_int sIndex = 0, vIndex = 0, nMethods = methods->GetCount();
      for (u_int i = 0; i < nMethods; i++) {
	MethodInfo *methodInfo = MethodInfo::FromWordDirect(methods->Get(i));
	if (methodInfo->IsTheMethod(name, descriptor)) {
	  u_int nArgs = methodInfo->GetNumberOfArguments();
	  Scheduler::PopFrame();
	  Scheduler::nArgs = Scheduler::ONE_ARG;
	  if (methodInfo->IsStatic()) {
	    // Indices of static methods start after static fields:
	    Table *fields = classInfo->GetFields();
	    u_int nStaticFields = 0, nFields = fields->GetCount();
	    for (u_int i = 0; i < nFields; i++) {
	      FieldInfo *fieldInfo = FieldInfo::FromWordDirect(fields->Get(i));
	      if (fieldInfo->IsStatic()) nStaticFields++;
	    }
	    Scheduler::currentArgs[0] =
	      StaticMethodRef::New(theClass, nStaticFields + sIndex, nArgs)->
	      ToWord();
	  } else {
	    Scheduler::currentArgs[0] =
	      VirtualMethodRef::New(theClass, vIndex, nArgs)->ToWord();
	  }
	  return Worker::CONTINUE;
	} else {
	  if (methodInfo->IsStatic())
	    sIndex++;
	  else
	    vIndex++;
	}
      }
      word wSuper = classInfo->GetSuper();
      if (wSuper == Store::IntToWord(0))
	Error("NoSuchMethod"); //--** raise
      frame->SetClass(Class::FromWord(wSuper));
      return CONTINUE;
    }
  case RESOLVE_INTERFACE_METHOD:
    Error("interface methods not implemented yet"); //--**
  default:
    Error("invalid resolution type");
  }
}

void ResolveInterpreter::PushCall(Closure *closure) {
  ClassLoader *classLoader = ClassLoader::FromWordDirect(closure->Sub(0));
  ResolveInterpreter::type resolveType =
    static_cast<ResolveInterpreter::type>
    (Store::DirectWordToInt(closure->Sub(1)));
  if (resolveType == ResolveInterpreter::RESOLVE_CLASS) {
    JavaString *name = JavaString::FromWordDirect(closure->Sub(2));
    Scheduler::PushFrame(ResolveFrame::New(classLoader, name)->ToWord());
  } else {
    word theClass = closure->Sub(2);
    JavaString *name = JavaString::FromWordDirect(closure->Sub(3));
    JavaString *descriptor = JavaString::FromWordDirect(closure->Sub(4));
    Scheduler::PushFrame(ResolveFrame::New(classLoader, resolveType, theClass,
					   name, descriptor)->ToWord());
  }
}

const char *ResolveInterpreter::Identify() {
  return "ResolveInterpreter";
}

void ResolveInterpreter::DumpFrame(word frame) {
  ResolveFrame *resolveClassFrame = ResolveFrame::FromWordDirect(frame);
  std::fprintf(stderr, "Resolve class %s\n",
	       resolveClassFrame->GetName()->ExportC());
}

//
// ClassLoader Method Implementations
//

void ClassLoader::Init() {
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

word ClassLoader::ResolveType(JavaString *name) {
  //--** cache results
  u_int n = name->GetLength();
  u_wchar *p = name->GetBase();
  u_int dimensions = 0;
  while (dimensions < n && p[dimensions] == '[') dimensions++;
  p += dimensions;
  n -= dimensions;
  Assert(n > 0);
  word wClass;
  switch (n--, *p++) {
  case 'B':
    wClass = BaseType::New(BaseType::Byte)->ToWord();
    break;
  case 'C':
    wClass = BaseType::New(BaseType::Char)->ToWord();
    break;
  case 'D':
    wClass = BaseType::New(BaseType::Double)->ToWord();
    break;
  case 'F':
    wClass = BaseType::New(BaseType::Float)->ToWord();
    break;
  case 'I':
    wClass = BaseType::New(BaseType::Int)->ToWord();
    break;
  case 'J':
    wClass = BaseType::New(BaseType::Long)->ToWord();
    break;
  case 'S':
    wClass = BaseType::New(BaseType::Short)->ToWord();
    break;
  case 'Z':
    wClass = BaseType::New(BaseType::Boolean)->ToWord();
    break;
  case 'L':
    {
      u_int i = 0;
      while (p[i++] != ';') n--;
      i--, n--;
      wClass = ResolveClass(JavaString::New(p, i));
      break;
    }
  default:
    Error("invalid descriptor"); //--** return failed future?
  }
  while (dimensions--) wClass = ArrayType::New(wClass)->ToWord();
  Assert(n == 0);
  return wClass;
}

word ClassLoader::ResolveFieldRef(JavaString *className, JavaString *name,
				  JavaString *descriptor) {
  ConcreteCode *concreteCode = ConcreteCode::New(ResolveInterpreter::self, 0);
  Closure *closure = Closure::New(concreteCode->ToWord(), 5);
  closure->Init(0, ToWord());
  closure->Init(1, Store::IntToWord(ResolveInterpreter::RESOLVE_FIELD));
  closure->Init(2, ResolveClass(className));
  closure->Init(3, name->ToWord());
  closure->Init(4, descriptor->ToWord());
  return Byneed::New(closure->ToWord())->ToWord();
}

word ClassLoader::ResolveMethodRef(JavaString *className, JavaString *name,
				   JavaString *descriptor) {
  ConcreteCode *concreteCode = ConcreteCode::New(ResolveInterpreter::self, 0);
  Closure *closure = Closure::New(concreteCode->ToWord(), 5);
  closure->Init(0, ToWord());
  closure->Init(1, Store::IntToWord(ResolveInterpreter::RESOLVE_METHOD));
  closure->Init(2, ResolveClass(className));
  closure->Init(3, name->ToWord());
  closure->Init(4, descriptor->ToWord());
  return Byneed::New(closure->ToWord())->ToWord();
}

word ClassLoader::ResolveInterfaceMethodRef(JavaString *className,
					    JavaString *name,
					    JavaString *descriptor) {
  ConcreteCode *concreteCode = ConcreteCode::New(ResolveInterpreter::self, 0);
  Closure *closure = Closure::New(concreteCode->ToWord(), 5);
  closure->Init(0, ToWord());
  u_int resolveType = ResolveInterpreter::RESOLVE_INTERFACE_METHOD;
  closure->Init(1, Store::IntToWord(resolveType));
  closure->Init(2, ResolveClass(className));
  closure->Init(3, name->ToWord());
  closure->Init(4, descriptor->ToWord());
  return Byneed::New(closure->ToWord())->ToWord();
}
