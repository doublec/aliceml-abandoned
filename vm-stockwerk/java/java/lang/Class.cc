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
#include "java/ClassLoader.hh"
#include "java/ClassInfo.hh"
#include "java/Authoring.hh"

//
// ReflectConstructorsWorker
//
static word wConstructorClass;
static word wConstructorMethodRef;

class ReflectConstructorsWorker: public Worker {
public:
  static ReflectConstructorsWorker *self;
private:
  ReflectConstructorsWorker() {}
public:
  static void Init() {
    self = new ReflectConstructorsWorker();
  }

  static void PushFrame(ObjectArray *array, Class *theClass, Type *classType);

  virtual u_int GetFrameSize(StackFrame *sFrame);
  virtual Result Run(StackFrame *sFrame);
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

class ReflectConstructorsFrame: private StackFrame {
protected:
  enum { ARRAY_POS, INDEX_POS, CLASS_POS, CLASS_TYPE_POS, SIZE };
public:
  static ReflectConstructorsFrame *New(ObjectArray *array, Class *theClass,
				       Type *classType) {
    NEW_STACK_FRAME(frame, ReflectConstructorsWorker::self, SIZE);
    frame->InitArg(ARRAY_POS, array->ToWord());
    frame->InitArg(INDEX_POS, 0);
    frame->InitArg(CLASS_POS, theClass->ToWord());
    frame->InitArg(CLASS_TYPE_POS, classType->ToWord());
    return static_cast<ReflectConstructorsFrame *>(frame);
  }

  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
  ObjectArray *GetArray() {
    return ObjectArray::FromWordDirect(GetArg(ARRAY_POS));
  }
  u_int GetIndex() {
    return Store::DirectWordToInt(GetArg(INDEX_POS));
  }
  void SetIndex(u_int index) {
    ReplaceArg(INDEX_POS, index);
  }
  Class *GetClass() {
    return Class::FromWordDirect(GetArg(CLASS_POS));
  }
  Type *GetClassType() {
    return Type::FromWordDirect(GetArg(CLASS_TYPE_POS));
  }
};

ReflectConstructorsWorker *ReflectConstructorsWorker::self;

void ReflectConstructorsWorker::PushFrame(ObjectArray *array,
					  Class *theClass, Type *classType) {
  ReflectConstructorsFrame::New(array, theClass, classType);
}

u_int ReflectConstructorsWorker::GetFrameSize(StackFrame *sFrame) {
  ReflectConstructorsFrame *frame =
    static_cast<ReflectConstructorsFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result ReflectConstructorsWorker::Run(StackFrame *sFrame) {
  ReflectConstructorsFrame *frame =
    static_cast<ReflectConstructorsFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  ObjectArray *array = frame->GetArray();
  Class *theClass = frame->GetClass();
  u_int index = frame->GetIndex();
  Assert(Scheduler::nArgs == 0);
  if (index == array->GetLength()) {
    Scheduler::PopFrame(frame->GetSize());
    Scheduler::nArgs = Scheduler::ONE_ARG;
    Scheduler::currentArgs[0] = array->ToWord();
    return CONTINUE;
  }
  VirtualMethodRef *methodRef =
    VirtualMethodRef::FromWord(wConstructorMethodRef);
  if (methodRef == INVALID_POINTER) {
    Scheduler::currentData = wConstructorMethodRef;
    return REQUEST;
  }
  Object *constructorObject = Object::New(Class::FromWord(wConstructorClass));
  MethodInfo *methodInfo = MethodInfo::FromWordDirect(array->Load(index));
  array->Store(index, constructorObject->ToWord());
  frame->SetIndex(index + 1);

  Type *classType = frame->GetClassType();

  ObjectArray *parameterTypes = ObjectArray::New(classType, 0); //--**
  ObjectArray *checkedExceptions = ObjectArray::New(classType, 0); //--**
  u_int slot = 0; //--** lookup in theClass->methodHashTable

  Scheduler::nArgs = 6;
  Scheduler::currentArgs[0] = constructorObject->ToWord();
  Scheduler::currentArgs[1] = theClass->GetClassObject()->ToWord();
  Scheduler::currentArgs[2] = parameterTypes->ToWord();
  Scheduler::currentArgs[3] = checkedExceptions->ToWord();
  Scheduler::currentArgs[4] = JavaInt::ToWord(methodInfo->GetAccessFlags());
  Scheduler::currentArgs[5] = JavaInt::ToWord(slot);

  Closure *closure =
    methodRef->GetClass()->GetVirtualMethod(methodRef->GetIndex());
  return Scheduler::PushCall(closure->ToWord());
}

const char *ReflectConstructorsWorker::Identify() {
  return "ReflectConstructorsWorker";
}

void ReflectConstructorsWorker::DumpFrame(StackFrame *sFrame) {
  ReflectConstructorsFrame *frame =
    static_cast<ReflectConstructorsFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  std::fprintf(stderr, "Reflect constructor %d/%d of class %s\n",
	       frame->GetIndex(), frame->GetArray()->GetLength(),
	       frame->GetClass()->GetClassInfo()->GetName()->ExportC());
}

//
// Native Method Implementations
//
DEFINE0(registerNatives) {
  RETURN_VOID;
} END

DEFINE3(forName0) {
  DECLARE_JAVA_STRING(name, x0);
  DECLARE_BOOL(initialize, x1);
  DECLARE_CLASS_LOADER(classLoader, x2);
  u_int length = name->GetLength();
  BaseArray *a = BaseArray::New(PrimitiveType::Char, length);
  for (u_int i = length; i--; ) {
    u_wchar c = name->CharAt(i);
    if (c == '.') c = '/';
    a->StoreChar(i, c);
  }
  JavaString *internalName = JavaString::New(a, 0, length);
  if (classLoader == INVALID_POINTER)
    classLoader = ClassLoader::GetBootstrapClassLoader();
  word wClass = classLoader->ResolveClass(internalName);
  Class *theClass = Class::FromWord(wClass);
  if (theClass == INVALID_POINTER) REQUEST(wClass);
  if (!theClass->IsInitialized() && initialize) {
    Future *future = theClass->GetLock()->Acquire();
    Assert(future == INVALID_POINTER); future = future;
    PUSH_PRIM_SELF();
    return theClass->RunInitializer();
  }
  RETURN(theClass->GetClassObject()->ToWord());
} END

DEFINE1(isInterface) {
  DECLARE_OBJECT(_this, x0);
  ClassObject *classObject = static_cast<ClassObject *>(_this);
  Type *type = classObject->GetRepresentedType();
  RETURN_BOOL(type->GetLabel() == JavaLabel::Class &&
	      static_cast<Class *>(type)->IsInterface());
} END

DEFINE1(isArray) {
  DECLARE_OBJECT(_this, x0);
  ClassObject *classObject = static_cast<ClassObject *>(_this);
  Type *type = classObject->GetRepresentedType();
  if (type->GetLabel() == JavaLabel::ArrayType) {
    RETURN_BOOL(true);
  } else {
    Assert(type->GetLabel() == JavaLabel::Class ||
	   type->GetLabel() == JavaLabel::PrimitiveType);
    RETURN_BOOL(false);
  }
} END

DEFINE1(isPrimitive) {
  DECLARE_OBJECT(_this, x0);
  ClassObject *classObject = static_cast<ClassObject *>(_this);
  Type *type = classObject->GetRepresentedType();
  if (type->GetLabel() == JavaLabel::PrimitiveType) {
    RETURN_BOOL(true);
  } else {
    Assert(type->GetLabel() == JavaLabel::Class ||
	   type->GetLabel() == JavaLabel::ArrayType);
    RETURN_BOOL(false);
  }
} END

static JavaString *TypeToName(Type *type) {
  switch (type->GetLabel()) {
  case JavaLabel::Class:
    return static_cast<Class *>(type)->GetClassInfo()->GetName();
  case JavaLabel::PrimitiveType:
    switch (static_cast<PrimitiveType *>(type)->GetType()) {
    case PrimitiveType::Boolean:
      return JavaString::New("boolean");
    case PrimitiveType::Byte:
      return JavaString::New("byte");
    case PrimitiveType::Char:
      return JavaString::New("char");
    case PrimitiveType::Double:
      return JavaString::New("double");
    case PrimitiveType::Float:
      return JavaString::New("float");
    case PrimitiveType::Int:
      return JavaString::New("int");
    case PrimitiveType::Long:
      return JavaString::New("long");
    case PrimitiveType::Short:
      return JavaString::New("short");
    case PrimitiveType::Void:
      return JavaString::New("void");
    default:
      Error("invalid primitive type");
    }
  case JavaLabel::ArrayType:
    return JavaString::New("[")->
      Concat(TypeToName(static_cast<ArrayType *>(type)->GetElementType()));
  default:
    Error("invalid type");
  }
}

DEFINE1(getName) {
  DECLARE_OBJECT(_this, x0);
  ClassObject *classObject = static_cast<ClassObject *>(_this);
  RETURN(TypeToName(classObject->GetRepresentedType())->ToWord());
} END

DEFINE1(getClassLoader0) {
  DECLARE_OBJECT(_this, x0);
  RETURN(null); //--**
} END

DEFINE1(getSuperclass) {
  DECLARE_OBJECT(_this, x0);
  ClassObject *classObject = static_cast<ClassObject *>(_this);
  Type *type = classObject->GetRepresentedType();
  switch (type->GetLabel()) {
  case JavaLabel::Class:
    {
      Class *theClass = static_cast<Class *>(type);
      if (theClass->IsInterface()) RETURN(null);
      Class *superClass = theClass->GetSuperClass();
      if (superClass == INVALID_POINTER) RETURN(null);
      RETURN(superClass->GetClassObject()->ToWord());
    }
  case JavaLabel::PrimitiveType:
    RETURN(null);
  case JavaLabel::ArrayType:
    RETURN(null); //--** return java/lang/Object class
  default:
    Error("illegal type");
  }
} END

DEFINE1(getInterfaces) {
  DECLARE_OBJECT(_this, x0);
  ClassObject *classObject = static_cast<ClassObject *>(_this);
  Type *type = classObject->GetRepresentedType();
  switch (type->GetLabel()) {
  case JavaLabel::Class:
    {
      Class *theClass = static_cast<Class *>(type);
      Table *interfaces = theClass->GetClassInfo()->GetInterfaces();
      u_int nInterfaces = interfaces->GetCount();
      ObjectArray *array =
	ObjectArray::New(static_cast<Type *>(_this->GetClass()), nInterfaces);
      for (u_int i = nInterfaces; i--; ) {
	Class *interfaceClass = Class::FromWordDirect(interfaces->Get(i));
	array->Store(i, interfaceClass->GetClassObject()->ToWord());
      }
      RETURN(array->ToWord());
    }
  case JavaLabel::PrimitiveType:
  case JavaLabel::ArrayType:
    {
      ObjectArray *array =
	ObjectArray::New(static_cast<Type *>(_this->GetClass()), 0);
      RETURN(array->ToWord());
    }
  default:
    Error("illegal type");
  }
} END

DEFINE1(getComponentType) {
  DECLARE_OBJECT(_this, x0);
  ClassObject *classObject = static_cast<ClassObject *>(_this);
  Type *type = classObject->GetRepresentedType();
  switch (type->GetLabel()) {
  case JavaLabel::Class:
  case JavaLabel::PrimitiveType:
    RETURN(null);
  case JavaLabel::ArrayType:
    RETURN(static_cast<ArrayType *>(type)->GetElementType()->GetClassObject()->
	   ToWord());
  default:
    Error("illegal type");
  }
} END

//--** duplicated in sun/reflect/Reflection.cc, getClassAccessFlags
static u_int GetTypeAccessFlags(Type *type) {
  switch (type->GetLabel()) {
  case JavaLabel::Class:
    return static_cast<Class *>(type)->GetClassInfo()->GetAccessFlags();
  case JavaLabel::PrimitiveType:
    // If this object represents an array class, a
    // primitive type or void, then its final modifier is always
    // true and its interface modifier is always false.
    return ClassInfo::ACC_PUBLIC | ClassInfo::ACC_FINAL;
  case JavaLabel::ArrayType:
    {
      Type *elementType = static_cast<ArrayType *>(type)->GetElementType();
      u_int accessFlags = GetTypeAccessFlags(elementType);
      // If the underlying class is an array class, then its
      // public, private and protected modifiers are the same
      // as those of its component type.
      return (accessFlags & ClassInfo::ACC_PUBLIC) | ClassInfo::ACC_FINAL;
    }
  default:
    Error("illegal type");
  }
}

DEFINE1(getModifiers) {
  DECLARE_OBJECT(_this, x0);
  ClassObject *classObject = static_cast<ClassObject *>(_this);
  RETURN_JINT(GetTypeAccessFlags(classObject->GetRepresentedType()));
} END

DEFINE1(getPrimitiveClass) {
  DECLARE_JAVA_STRING(name, x0);
  if (name->Equals("boolean")) {
    RETURN(PrimitiveType::GetClassObject(PrimitiveType::Boolean)->ToWord());
  } else if (name->Equals("byte")) {
    RETURN(PrimitiveType::GetClassObject(PrimitiveType::Byte)->ToWord());
  } else if (name->Equals("char")) {
    RETURN(PrimitiveType::GetClassObject(PrimitiveType::Char)->ToWord());
  } else if (name->Equals("double")) {
    RETURN(PrimitiveType::GetClassObject(PrimitiveType::Double)->ToWord());
  } else if (name->Equals("float")) {
    RETURN(PrimitiveType::GetClassObject(PrimitiveType::Float)->ToWord());
  } else if (name->Equals("int")) {
    RETURN(PrimitiveType::GetClassObject(PrimitiveType::Int)->ToWord());
  } else if (name->Equals("long")) {
    RETURN(PrimitiveType::GetClassObject(PrimitiveType::Long)->ToWord());
  } else if (name->Equals("short")) {
    RETURN(PrimitiveType::GetClassObject(PrimitiveType::Short)->ToWord());
  } else if (name->Equals("void")) {
    RETURN(PrimitiveType::GetClassObject(PrimitiveType::Void)->ToWord());
  } else {
    Error("unknown primitive class");
  }
} END

DEFINE2(getDeclaredConstructors0) {
  DECLARE_OBJECT(_this, x0);
  DECLARE_BOOL(publicOnly, x1);
  Type *constructorType = Type::FromWord(wConstructorClass);
  if (constructorType == INVALID_POINTER) REQUEST(wConstructorClass);
  ClassObject *classObject = static_cast<ClassObject *>(_this);
  Type *type = classObject->GetRepresentedType();
  switch (type->GetLabel()) {
  case JavaLabel::Class:
    {
      Class *theClass = static_cast<Class *>(type);
      Table *methods = theClass->GetClassInfo()->GetMethods();
      u_int nMethods = methods->GetCount();
      JavaString *constructorName = JavaString::New("<init>");
      Table *constructors = Table::New(nMethods); // pessimistic assumption
      u_int nConstructors = 0;
      for (u_int i = 0; i < nMethods; i++) {
	MethodInfo *methodInfo = MethodInfo::FromWordDirect(methods->Get(i));
	if ((!publicOnly || methodInfo->IsPublic()) &&
	    methodInfo->GetName()->Equals(constructorName)) {
	  constructors->Init(nConstructors++, methodInfo->ToWord());
	}
      }
      ObjectArray *array = ObjectArray::New(constructorType, nConstructors);
      for (u_int j = nConstructors; j--; )
	array->Store(j, constructors->Get(j));
      ReflectConstructorsWorker::PushFrame
	(array, theClass, static_cast<Type *>(_this->GetClass()));
      RETURN_VOID;
    }
  case JavaLabel::PrimitiveType:
  case JavaLabel::ArrayType:
    RETURN(ObjectArray::New(constructorType, 0)->ToWord());
  default:
    Error("illegal type");
  }
} END

DEFINE1(desiredAssertionStatus0) {
  x0 = x0; // ignored
  RETURN_BOOL(false);
} END

void NativeMethodTable::java_lang_Class(JavaString *className) {
  ReflectConstructorsWorker::Init();
  ClassLoader *classLoader = ClassLoader::GetBootstrapClassLoader();
  wConstructorClass = classLoader->ResolveClass
    (JavaString::New("java/lang/reflect/Constructor"));
  RootSet::Add(wConstructorClass);
  wConstructorMethodRef = classLoader->ResolveMethodRef
    (wConstructorClass, JavaString::New("<init>"),
     JavaString::New("(Ljava/lang/Class;[Ljava/lang/Class;[Ljava/lang/Class;"
		     "II)V"));
  RootSet::Add(wConstructorMethodRef);
  Register(className, "registerNatives", "()V", registerNatives, 0, false);
  Register(className, "forName0",
	   "(Ljava/lang/String;ZLjava/lang/ClassLoader;)Ljava/lang/Class;",
	   forName0, 3, false);
  //--** isInstance
  //--** isAssignableFrom
  Register(className, "isInterface", "()Z", isInterface, 1, true);
  Register(className, "isArray", "()Z", isArray, 1, true);
  Register(className, "isPrimitive", "()Z", isPrimitive, 1, true);
  Register(className, "getName", "()Ljava/lang/String;", getName, 1, true);
  Register(className, "getClassLoader0", "()Ljava/lang/ClassLoader;",
	   getClassLoader0, 1, true);
  //--** getClassLoader0
  Register(className, "getSuperclass", "()Ljava/lang/Class;",
	   getSuperclass, 1, true);
  Register(className, "getInterfaces", "()[Ljava/lang/Class;",
	   getInterfaces, 1, true);
  Register(className, "getComponentType", "()Ljava/lang/Class;",
	   getComponentType, 1, true);
  Register(className, "getModifiers", "()I", getModifiers, 1, true);
  //--** getSigners
  //--** setSigners
  //--** getDeclaringClass
  //--** getProtectionDomain0
  //--** setProtectionDomain0
  Register(className, "getPrimitiveClass",
	   "(Ljava/lang/String;)Ljava/lang/Class;",
	   getPrimitiveClass, 1, false);
  //--** getDeclaredFields0
  //--** getDeclaredMethods0
  Register(className, "getDeclaredConstructors0",
	   "(Z)[Ljava/lang/reflect/Constructor;",
	   getDeclaredConstructors0, 2, true);
  //--** getDeclaredClasses0
  Register(className, "desiredAssertionStatus0", "(Ljava/lang/Class;)Z",
	   desiredAssertionStatus0, 1, false);
}
