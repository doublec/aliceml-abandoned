//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000-2001
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "emulator/RootSet.hh"
#endif

#include "emulator/RootSet.hh"
#include "adt/Queue.hh"

class Element: private Block {
private:
  static const u_int POINTER_POS = 0;
  static const u_int VALUE_POS   = 1;
  static const u_int SIZE        = 2;
public:
  using Block::ToWord;

  static Element *New(word *pointer) {
    Block *b = Store::AllocBlock(ROOTSETELEMENT_LABEL, SIZE);
    b->InitArg(POINTER_POS, Store::UnmanagedPointerToWord(pointer));
    return static_cast<Element *>(b);
  }
  static Element *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == ROOTSETELEMENT_LABEL);
    return static_cast<Element *>(b);
  }

  word *GetPointer() {
    return static_cast<word *>
      (Store::DirectWordToUnmanagedPointer(GetArg(POINTER_POS)));
  }
  void PreGC() {
    ReplaceArg(VALUE_POS, *GetPointer());
  }
  void PostGC() {
    *GetPointer() = GetArg(VALUE_POS);
  }
};

//--** This should probably implement a real set instead of using Queue.
class Set: private Queue {
private:
  static const u_int initialSize = 8; //--** to be checked
public:
  using Queue::ToWord;

  static Set *New() {
    return static_cast<Set *>(Queue::New(initialSize));
  }
  static Set *FromWordDirect(word x) {
    return static_cast<Set *>(Queue::FromWordDirect(x));
  }

  void Add(word &root) {
    Enqueue(Element::New(&root)->ToWord());
  }
  void Remove(word &root) {
    for (u_int i = GetNumberOfElements(); i--; ) {
      Element *element = Element::FromWordDirect(GetNthElement(i));
      if (element->GetPointer() == &root) {
	Queue::RemoveNthElement(i);
  	break;
      }
    }
  }
  void PreGC() {
    Blank();
    for (u_int i = GetNumberOfElements(); i--; )
      Element::FromWordDirect(GetNthElement(i))->PreGC();
  }
  void PostGC() {
    for (u_int i = GetNumberOfElements(); i--; )
      Element::FromWordDirect(GetNthElement(i))->PostGC();
  }
};

static Set *set;

void RootSet::Init() {
  set = Set::New();
}

void RootSet::Add(word &root) {
  set->Add(root);
}

void RootSet::Remove(word &root) {
  set->Remove(root);
}

void RootSet::DoGarbageCollection() {
  set->PreGC();
  word w = set->ToWord();
  Store::DoGC(w);
  set = Set::FromWordDirect(w);
  set->PostGC();
}
