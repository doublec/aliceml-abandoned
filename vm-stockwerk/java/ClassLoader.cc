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
// ResolveClassInterpreter
//

class ResolveClassInterpreter: public Interpreter {
public:
  static ResolveClassInterpreter *self;
private:
  ResolveClassInterpreter() {}
public:
  static void Init() {
    self = new ResolveClassInterpreter();
  }

  virtual Result Run();
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
  virtual void PushCall(Closure *closure);
};

class ResolveClassFrame: private StackFrame {
protected:
  enum { CLASS_LOADER_POS, NAME_POS, SIZE };
public:
  using Block::ToWord;

  static ResolveClassFrame *New(ClassLoader *classLoader, JavaString *name) {
    StackFrame *frame =
      StackFrame::New(RESOLVE_CLASS_FRAME,
		      ResolveClassInterpreter::self, SIZE);
    frame->InitArg(CLASS_LOADER_POS, classLoader->ToWord());
    frame->InitArg(NAME_POS, name->ToWord());
    return static_cast<ResolveClassFrame *>(frame);
  }
  static ResolveClassFrame *FromWordDirect(word x) {
    StackFrame *frame = StackFrame::FromWordDirect(x);
    Assert(frame->GetLabel() == RESOLVE_CLASS_FRAME);
    return static_cast<ResolveClassFrame *>(frame);
  }

  ClassLoader *GetClassLoader() {
    return ClassLoader::FromWordDirect(GetArg(CLASS_LOADER_POS));
  }
  JavaString *GetName() {
    return JavaString::FromWordDirect(GetArg(NAME_POS));
  }
};

Worker::Result ResolveClassInterpreter::Run() {
  ResolveClassFrame *frame =
    ResolveClassFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  JavaString *name = frame->GetName();
  ClassFile *classFile = ClassFile::NewFromFile(name);
  ClassInfo *classInfo = classFile->Parse(frame->GetClassLoader());
  Scheduler::nArgs = Scheduler::ONE_ARG;
  Scheduler::currentArgs[0] =
    classInfo->ToWord(); //--** return the class object
  return Worker::CONTINUE;
}

void ResolveClassInterpreter::PushCall(Closure *closure) {
  ClassLoader *classLoader = ClassLoader::FromWordDirect(closure->Sub(0));
  JavaString *name = JavaString::FromWordDirect(closure->Sub(1));
  Scheduler::PushFrame(ResolveClassFrame::New(classLoader, name)->ToWord());
}

const char *ResolveClassInterpreter::Identify() {
  return "ResolveClassInterpreter";
}

void ResolveClassInterpreter::DumpFrame(word frame) {
  ResolveClassFrame *resolveClassFrame =
    ResolveClassFrame::FromWordDirect(frame);
  std::fprintf(stderr, "Resolve class %s\n",
	       resolveClassFrame->GetName()->ExportC());
}

//
// ClassLoader Method Implementations
//

void ClassLoader::Init() {
  ResolveClassInterpreter::Init();
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
  ConcreteCode *concreteCode =
    ConcreteCode::New(ResolveClassInterpreter::self, 2);
  concreteCode->Init(0, ToWord());
  concreteCode->Init(1, name->ToWord());
  Closure *closure = Closure::New(concreteCode->ToWord(), 0);
  Byneed *byneed = Byneed::New(closure->ToWord());
  return byneed->ToWord();
}

word ClassLoader::ResolveType(JavaString *name) {
  ClassTable *classTable = GetClassTable();
  word wClass = classTable->Lookup(name);
  if (wClass == (word) 0) {
    u_int n = name->GetLength();
    u_wchar *p = name->GetBase();
    u_int dimensions = 0;
    while (dimensions < n && p[dimensions] == '[') dimensions++;
    p += dimensions;
    n -= dimensions;
    if (dimensions) { // array type
      Assert(n > 0);
      switch (n--, *p++) {
      case 'B':
	wClass = BaseArrayType::New(BaseType::Byte, dimensions)->ToWord();
	break;
      case 'C':
	wClass = BaseArrayType::New(BaseType::Char, dimensions)->ToWord();
	break;
      case 'D':
	wClass = BaseArrayType::New(BaseType::Double, dimensions)->ToWord();
	break;
      case 'F':
	wClass = BaseArrayType::New(BaseType::Float, dimensions)->ToWord();
	break;
      case 'I':
	wClass = BaseArrayType::New(BaseType::Int, dimensions)->ToWord();
	break;
      case 'J':
	wClass = BaseArrayType::New(BaseType::Long, dimensions)->ToWord();
	break;
      case 'S':
	wClass = BaseArrayType::New(BaseType::Short, dimensions)->ToWord();
	break;
      case 'Z':
	wClass = BaseArrayType::New(BaseType::Boolean, dimensions)->ToWord();
	break;
      case 'L':
	{
	  u_int i = 0;
	  while (p[i++] != ';');
	  n -= i + 1;
	  word classType = ResolveClass(JavaString::New(p - 1, i + 2));
	  wClass = ObjectArrayType::New(classType, dimensions)->ToWord();
	  break;
	}
      default:
	Error("invalid descriptor"); //--** return failed future?
      }
    } else {
      u_int i = 0;
      while (p[i++] != ';');
      n -= i + 1;
      wClass = ResolveClass(JavaString::New(p - 1, i + 2));
    }
    Assert(n == 0);
    classTable->Insert(name, wClass);
  }
  return wClass;
}
