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

#include "adt/HashTable.hh"
#include "generic/Transients.hh"
#include "generic/Interpreter.hh"
#include "java/ClassLoader.hh"

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

//
// ResolveClassClosure
//

class ResolveClassClosure: private Closure {
public:
  using Block::ToWord;

  static ResolveClassClosure *New(JavaString *name);
};

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
  Byneed *byneed = Byneed::New(ResolveClassClosure::New(name)->ToWord());
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
	  word classType = ResolveClass(JavaString::New(p, i));
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
      wClass = ResolveClass(JavaString::New(p, i));
    }
    Assert(n == 0);
    classTable->Insert(name, wClass);
  }
  return wClass;
}
