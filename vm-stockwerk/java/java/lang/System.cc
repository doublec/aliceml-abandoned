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
#include <cstdlib>
#include <cstring>
#if defined(__MINGW32__) || defined(_MSC_VER)
#include <windows.h>
#else
#include <time.h>
#endif

#include "java/ClassLoader.hh"
#include "java/Authoring.hh"

struct Property {
  const char *name, *value;
};

static Property defaultProperties[] = {
  // Properties required according to java.lang.System#getProperties:
  {"java.version", "1.4.1"},
  {"java.vendor", "Programming Systems Lab, Universität des Saarlandes"},
  {"java.vendor.url", "http://www.ps.uni-sb.de/alice/"},
  {"java.vendor.url.bug", "http://www.ps.uni-sb.de/alice/bugs/"},
  {"java.home", "/opt/stockhausen-devel"}, //--**
  {"java.vm.specification.version", "1.0"},
  {"java.vm.specification.vendor", "Sun Microsystems Inc."},
  {"java.vm.specification.name", "Java Virtual Machine Specification"},
  {"java.vm.version", "Operette 3"},
  {"java.vm.vendor", "Programming Systems Lab, Universität des Saarlandes"},
  {"java.vm.name", "Java VM running on Stockwerk"},
  {"java.specification.version", "1.4"},
  {"java.specification.vendor", "Sun Microsystems Inc."},
  {"java.specification.name", "Java Platform API Specification"},
  {"java.class.version", "48.0"}, //--** ClassFile::SupportedVersion
  {"java.class.path", "."}, //--**
  {"java.library.path", ""}, //--**
  {"java.io.tmpdir", "/"}, //--**
  {"java.compiler", ""},
  {"java.ext.dirs", ""},
#if defined(__MINGW32__) || defined(_MSC_VER)
  {"os.name", "Windows"}, //--**
#else
  {"os.name", "Unix"}, //--**
#endif
  {"os.arch", "x86"}, //--**
  {"os.version", "5.1"}, //--**
#if defined(__MINGW32__) || defined(_MSC_VER)
  {"file.separator", "\\"},
  {"path.separator", ";"},
  {"line.separator", "\r\n"},
#else
  {"file.separator", "/"},
  {"path.separator", ":"},
  {"line.separator", "\n"},
#endif
  {"user.name", ""}, //--**
  {"user.home", "/"}, //--**
  {"user.dir", "/"}, //--**
  // Extra properties:
  {"file.encoding.pkg", "sun.io"},
#if defined(__MINGW32__) || defined(_MSC_VER)
  {"file.encoding", "Cp1252"},
#else
  {"file.encoding", "ANSI_X3.4-1968"},
#endif
  {"sun.io.unicode.encoding", "UnicodeLittle"},
  {NULL, NULL}
};

class PutPropertiesWorker: public Worker {
private:
  PutPropertiesWorker() {}
public:
  static PutPropertiesWorker *self;

  static void Init() {
    self = new PutPropertiesWorker();
  }

  static void PushFrame(Object *object, word wMethodRef, Property *properties);

  virtual u_int GetFrameSize(StackFrame *sFrame);
  virtual Result Run(StackFrame *sFrame);
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

class PutPropertiesFrame: private StackFrame {
protected:
  enum { OBJECT_POS, METHOD_REF_POS, PROPERTIES_POS, SIZE };
public:
  static PutPropertiesFrame *New(Object *object, word wMethodRef,
				 Property *properties) {
    NEW_STACK_FRAME(frame, PutPropertiesWorker::self, SIZE);
    frame->InitArg(OBJECT_POS, object->ToWord());
    frame->InitArg(METHOD_REF_POS, wMethodRef);
    frame->InitArg(PROPERTIES_POS, Store::UnmanagedPointerToWord(properties));
    return static_cast<PutPropertiesFrame *>(frame);
  }

  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
  Object *GetObject() {
    return Object::FromWordDirect(GetArg(OBJECT_POS));
  }
  word GetMethodRef() {
    return GetArg(METHOD_REF_POS);
  }
  Property *GetProperties() {
    return static_cast<Property *>
      (Store::DirectWordToUnmanagedPointer(GetArg(PROPERTIES_POS)));
  }
  void SetProperties(Property *properties) {
    ReplaceArg(PROPERTIES_POS, Store::UnmanagedPointerToWord(properties));
  }
};

PutPropertiesWorker *PutPropertiesWorker::self;

void PutPropertiesWorker::PushFrame(Object *object, word wMethodRef,
				    Property *properties) {
  PutPropertiesFrame::New(object, wMethodRef, properties);
}

u_int PutPropertiesWorker::GetFrameSize(StackFrame *sFrame) {
  PutPropertiesFrame *frame = static_cast<PutPropertiesFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

Worker::Result PutPropertiesWorker::Run(StackFrame *sFrame) {
  PutPropertiesFrame *frame = static_cast<PutPropertiesFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  word wMethodRef = frame->GetMethodRef();
  MethodRef *methodRef = MethodRef::FromWord(wMethodRef);
  if (methodRef == INVALID_POINTER) {
    Scheduler::currentData = wMethodRef;
    return Worker::REQUEST;
  }
  if (methodRef->GetLabel() != JavaLabel::VirtualMethodRef) {
    ThrowWorker::PushFrame(ThrowWorker::NoSuchMethodError,
			   JavaString::New("setProperty"));
    Scheduler::nArgs = 0;
    return CONTINUE;
  }
  VirtualMethodRef *virtualMethodRef =
    static_cast<VirtualMethodRef *>(methodRef);
  Object *object = frame->GetObject();
  Property *properties = frame->GetProperties();
  if (properties->name == NULL) {
    Scheduler::PopFrame(frame->GetSize());
    Scheduler::nArgs = Scheduler::ONE_ARG;
    Scheduler::currentArgs[0] = object->ToWord();
    return CONTINUE;
  }
  Scheduler::nArgs = 3;
  Scheduler::currentArgs[0] = object->ToWord();
  Scheduler::currentArgs[1] = JavaString::New(properties->name)->ToWord();
  Scheduler::currentArgs[2] = JavaString::New(properties->value)->ToWord();
  frame->SetProperties(++properties);
  Closure *closure = object->GetVirtualMethod(virtualMethodRef->GetIndex());
  return Scheduler::PushCall(closure->ToWord());
}

const char *PutPropertiesWorker::Identify() {
  return "PutPropertiesWorker";
}

void PutPropertiesWorker::DumpFrame(StackFrame *) {
  std::fprintf(stderr, "Initialize system properties\n");
}

//
// Native Method Implementations
//
DEFINE0(registerNatives) {
  RETURN_VOID;
} END

static Class *GetSystemClass() {
  ClassLoader *classLoader = ClassLoader::GetBootstrapClassLoader();
  word wClass = classLoader->ResolveClass(JavaString::New("java/lang/System"));
  Class *theClass = Class::FromWord(wClass);
  Assert(theClass != INVALID_POINTER);
  return theClass;
}

DEFINE1(setIn0) {
  GetSystemClass()->PutStaticField(0, x0);
  RETURN_VOID;
} END

DEFINE1(setOut0) {
  GetSystemClass()->PutStaticField(1, x0);
  RETURN_VOID;
} END

DEFINE1(setErr0) {
  GetSystemClass()->PutStaticField(2, x0);
  RETURN_VOID;
} END

DEFINE0(currentTimeMillis) {
#if defined(__MINGW32__) || defined(_MSC_VER)
  //--** since 1970
  SYSTEMTIME st;
  GetSystemTime(&st);
  FILETIME ft;
  if (SystemTimeToFileTime(&st, &ft) == FALSE)
    Error("SystemTimeToFileTime failed"); //--**
  DRETURN(JavaLong::New(ft.dwHighDateTime, ft.dwLowDateTime)->ToWord());
#else
  s_int64 t = time(NULL);
  DRETURN(JavaLong::New(t * 1000)->ToWord()); //--**
#endif
} END

DEFINE5(arraycopy) {
  word wSrc = x0;
  DECLARE_JINT(srcPos, x1);
  word wDest = x2;
  DECLARE_JINT(destPos, x3);
  DECLARE_JINT(length, x4);
  Block *destBlock = Store::WordToBlock(wDest);
  if (destBlock == INVALID_POINTER) {
    if (Store::WordToInt(wDest) == INVALID_POINTER) REQUEST(wDest);
    THROW(NullPointerException, "dest");
  }
  Block *srcBlock = Store::WordToBlock(wSrc);
  if (srcBlock == INVALID_POINTER) {
    if (Store::WordToInt(wSrc) == INVALID_POINTER) REQUEST(wSrc);
    THROW(NullPointerException, "src");
  }
  BlockLabel destLabel = destBlock->GetLabel();
  BlockLabel srcLabel = srcBlock->GetLabel();
  if (destLabel == JavaLabel::BaseArray) {
    if (srcLabel == JavaLabel::BaseArray) {
      BaseArray *destArray = static_cast<BaseArray *>(destBlock);
      BaseArray *srcArray = static_cast<BaseArray *>(srcBlock);
      PrimitiveType::type destType = destArray->GetElementType();
      PrimitiveType::type srcType = srcArray->GetElementType();
      if (destType == srcType) {
	if (srcPos < 0)
	  THROW(IndexOutOfBoundsException, "srcPos is negative");
	if (destPos < 0)
	  THROW(IndexOutOfBoundsException, "destPos is negative");
	if (length < 0)
	  THROW(IndexOutOfBoundsException, "length is negative");
	if (static_cast<u_int>(srcPos + length) > srcArray->GetLength())
	  THROW(IndexOutOfBoundsException, "src");
	if (static_cast<u_int>(destPos + length) > destArray->GetLength())
	  THROW(IndexOutOfBoundsException, "dest");
	destArray->Copy(destPos, srcArray, srcPos, length);
      } else {
	THROW(ArrayStoreException,
	      "src and dest have different primitive component types");
      }
    } else if (srcLabel == JavaLabel::ObjectArray) {
      THROW(ArrayStoreException, "src and dest are incompatible array types");
    } else {
      THROW(ArrayStoreException, "src is not an array type");
    }
  } else if (destLabel == JavaLabel::ObjectArray) {
    if (srcLabel == JavaLabel::ObjectArray) {
      ObjectArray *destArray = static_cast<ObjectArray *>(destBlock);
      ObjectArray *srcArray = static_cast<ObjectArray *>(srcBlock);
      if (srcPos < 0)
	THROW(IndexOutOfBoundsException, "srcPos is negative");
      if (destPos < 0)
	THROW(IndexOutOfBoundsException, "destPos is negative");
      if (length < 0)
	THROW(IndexOutOfBoundsException, "length is negative");
      if (static_cast<u_int>(srcPos + length) > srcArray->GetLength())
	THROW(IndexOutOfBoundsException, "src");
      if (static_cast<u_int>(destPos + length) > destArray->GetLength())
	THROW(IndexOutOfBoundsException, "dest");
      //--** check assignment compatibility
      if (srcArray == destArray && srcPos > destPos) {
	for (u_int i = 0; i < static_cast<u_int>(length); i++)
	  destArray->Store(destPos + i, srcArray->Load(srcPos + i));
      } else
	for (u_int i = length; i--; )
	  destArray->Store(destPos + i, srcArray->Load(srcPos + i));
    } else if (srcLabel == JavaLabel::BaseArray) {
      THROW(ArrayStoreException, "src and dest are incompatible array types");
    } else {
      THROW(ArrayStoreException, "src is not an array type");
    }
  } else {
    THROW(ArrayStoreException, "dest is not an array type");
  }
  RETURN_VOID;
} END

DEFINE1(initProperties) {
  DECLARE_OBJECT(object, x0);
  if (x0 == INVALID_POINTER) THROW(NullPointerException, "props");
  ClassLoader *classLoader = ClassLoader::GetBootstrapClassLoader();
  JavaString *className = JavaString::New("java/util/Properties");
  word theClass = classLoader->ResolveClass(className);
  JavaString *name = JavaString::New("setProperty");
  JavaString *descriptor =
    JavaString::New("(Ljava/lang/String;Ljava/lang/String;)"
		    "Ljava/lang/Object;");
  word wMethodRef = classLoader->ResolveMethodRef(theClass, name, descriptor);
  PutPropertiesWorker::PushFrame(object, wMethodRef, defaultProperties);
  RETURN_VOID;
} END

void NativeMethodTable::java_lang_System(JavaString *className) {
  PutPropertiesWorker::Init();
  Register(className, "registerNatives", "()V", registerNatives, 0, false);
  Register(className, "setIn0", "(Ljava/io/InputStream;)V",
	   setIn0, 1, false);
  Register(className, "setOut0", "(Ljava/io/PrintStream;)V",
	   setOut0, 1, false);
  Register(className, "setErr0", "(Ljava/io/PrintStream;)V",
	   setErr0, 1, false);
  Register(className, "currentTimeMillis", "()J", currentTimeMillis, 0, false);
  Register(className, "arraycopy",
	   "(Ljava/lang/Object;ILjava/lang/Object;II)V", arraycopy, 5, false);
  //--** identityHashCode
  Register(className, "initProperties",
	   "(Ljava/util/Properties;)Ljava/util/Properties;",
	   initProperties, 1, false);
  //--** mapLibraryName
}
