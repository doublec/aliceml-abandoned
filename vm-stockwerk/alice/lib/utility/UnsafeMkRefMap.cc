//
// Authors:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002
//   Thorsten Brunklaus, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cstdio>
#include "adt/HashTable.hh"
#include "alice/primitives/Authoring.hh"

static const BlockLabel ENTRY_LABEL = TUPLE_LABEL;
static const BlockLabel REFMAP_LABEL = TUPLE_LABEL;

//
// Abstract Data Type: RefMap
//

class Entry: private Block {
private:
  enum { KEY_POS, VALUE_POS, PREV_POS, NEXT_POS, SIZE };
public:
  using Block::ToWord;

  static Entry *New(int key, word value, Entry *next) {
    Block *b = Store::AllocBlock(ENTRY_LABEL, SIZE);
    b->InitArg(KEY_POS, key);
    b->InitArg(VALUE_POS, value);
    b->InitArg(PREV_POS, 0);
    if (next != INVALID_POINTER) {
      b->InitArg(NEXT_POS, next->ToWord());
      next->ReplaceArg(PREV_POS, b->ToWord());
    } else
      b->InitArg(NEXT_POS, 0);
    return static_cast<Entry *>(b);
  }
  static Entry *FromWordDirect(word w) {
    Block *b = Store::DirectWordToBlock(w);
    Assert(b->GetLabel() == ENTRY_LABEL && b->GetSize() == SIZE);
    return static_cast<Entry *>(b);
  }

  int GetKey() {
    return Store::DirectWordToInt(GetArg(KEY_POS));
  }
  word GetValue() {
    return GetArg(VALUE_POS);
  }
  Entry *GetNext() {
    word next = GetArg(NEXT_POS);
    if (next == Store::IntToWord(0))
      return INVALID_POINTER;
    else
      return Entry::FromWordDirect(next);
  }

  word Unlink() {
    // Returns:
    //    Integer 1, iff unlink was completed without the need to touch head.
    //    Integer 0, iff head needs to be set to the empty list.
    //    New head, otherwise.
    word prev = GetArg(PREV_POS);
    word next = GetArg(NEXT_POS);
    if (next != Store::IntToWord(0)) {
      Entry *nextEntry = Entry::FromWordDirect(next);
      nextEntry->ReplaceArg(PREV_POS, prev);
    }
    if (prev != Store::IntToWord(0)) {
      Entry *prevEntry = Entry::FromWordDirect(prev);
      prevEntry->ReplaceArg(NEXT_POS, next);
      return Store::IntToWord(1);
    } else
      return next;
  }
};

class RefMap: private Block {
private:
  enum { HASHTABLE_POS, HEAD_POS, SIZE };

  static const u_int initialSize = 8; //--** to be determined
public:
  using Block::ToWord;

  static RefMap *New() {
    Block *b = Store::AllocBlock(REFMAP_LABEL, SIZE);
    b->InitArg(HASHTABLE_POS,
	       HashTable::New(HashTable::INT_KEY, initialSize)->ToWord());
    b->InitArg(HEAD_POS, 0);
    return static_cast<RefMap *>(b);
  }
  static RefMap *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == REFMAP_LABEL && b->GetSize() == SIZE);
    return static_cast<RefMap *>(b);
  }

  void Insert(int key, word value) {
    word head = GetArg(HEAD_POS);
    Entry *headEntry = head == Store::IntToWord(0)?
      INVALID_POINTER: Entry::FromWordDirect(head);
    Entry *entry = Entry::New(key, value, headEntry);
    ReplaceArg(HEAD_POS, entry->ToWord());
    HashTable *hashTable = HashTable::FromWordDirect(GetArg(HASHTABLE_POS));
    hashTable->InsertItem(Store::IntToWord(key), entry->ToWord());
  }
  void Delete(int key) {
    word wKey = Store::IntToWord(key);
    HashTable *hashTable = HashTable::FromWordDirect(GetArg(HASHTABLE_POS));
    if (hashTable->IsMember(wKey)) {
      Entry *entry = Entry::FromWordDirect(hashTable->GetItem(wKey));
      hashTable->DeleteItem(wKey);
      word result = entry->Unlink();
      if (result != Store::IntToWord(1))
	ReplaceArg(HEAD_POS, result);
    }
  }
  void DeleteAll() {
    HashTable::FromWordDirect(GetArg(HASHTABLE_POS))->Clear();
    ReplaceArg(HEAD_POS, 0);
  }
  word Lookup(int key) {
    word wKey = Store::IntToWord(key);
    HashTable *hashTable = HashTable::FromWordDirect(GetArg(HASHTABLE_POS));
    if (hashTable->IsMember(wKey)) {
      TagVal *some = TagVal::New(1, 1); // SOME ...
      Entry *entry = Entry::FromWordDirect(hashTable->GetItem(wKey));
      some->Init(0, entry->GetValue());
      return some->ToWord();
    } else {
      return Store::IntToWord(0); // NONE
    }
  }
  bool Member(int key) {
    return HashTable::FromWordDirect(GetArg(HASHTABLE_POS))->
      IsMember(Store::IntToWord(key));
  }
  bool IsEmpty() {
    return HashTable::FromWordDirect(GetArg(HASHTABLE_POS))->IsEmpty();
  }
  Entry *GetHead() {
    word head = GetArg(HEAD_POS);
    if (head == Store::IntToWord(0))
      return INVALID_POINTER;
    else
      return Entry::FromWordDirect(head);
  }
};

#define DECLARE_REF_MAP(refMap, x) DECLARE_BLOCKTYPE(RefMap, refMap, x)

//
// Interpreter for Iterating over RefMaps
//

enum operation { app, appi, fold, foldi };

class RefMapIteratorInterpreter: public Interpreter {
private:
  static RefMapIteratorInterpreter *self;
  RefMapIteratorInterpreter(): Interpreter() {}
public:
  static void Init() {
    self = new RefMapIteratorInterpreter();
  }
  // Frame Handling
  static void PushFrame(TaskStack *taskStack,
			Entry *entry, word closure, operation op);
  // Execution
  virtual Result Run(TaskStack *taskStack);
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

class RefMapIteratorFrame: private StackFrame {
private:
  enum { ENTRY_POS, CLOSURE_POS, OPERATION_POS, SIZE };
public:
  using StackFrame::ToWord;

  static RefMapIteratorFrame *New(Interpreter *interpreter,
				  Entry *entry, word closure, operation op) {
    StackFrame *frame =
      StackFrame::New(REFMAP_ITERATOR_FRAME, interpreter, SIZE);
    frame->InitArg(ENTRY_POS, entry->ToWord());
    frame->InitArg(CLOSURE_POS, closure);
    frame->InitArg(OPERATION_POS, Store::IntToWord(op));
    return static_cast<RefMapIteratorFrame *>(frame);
  }
  static RefMapIteratorFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == REFMAP_ITERATOR_FRAME);
    return static_cast<RefMapIteratorFrame *>(p);
  }

  Entry *GetEntry() {
    return Entry::FromWordDirect(GetArg(ENTRY_POS));
  }
  void SetEntry(Entry *entry) {
    ReplaceArg(ENTRY_POS, entry->ToWord());
  }
  word GetClosure() {
    return GetArg(CLOSURE_POS);
  }
  operation GetOperation() {
    return static_cast<operation>
      (Store::DirectWordToInt(GetArg(OPERATION_POS)));
  }
};

RefMapIteratorInterpreter *RefMapIteratorInterpreter::self;

void RefMapIteratorInterpreter::PushFrame(TaskStack *taskStack, Entry *entry,
					  word closure, operation op) {
  RefMapIteratorFrame *frame =
    RefMapIteratorFrame::New(self, entry, closure, op);
  taskStack->PushFrame(frame->ToWord());
}

Interpreter::Result RefMapIteratorInterpreter::Run(TaskStack *taskStack) {
  RefMapIteratorFrame *frame =
    RefMapIteratorFrame::FromWordDirect(taskStack->GetFrame());
  Entry *entry = frame->GetEntry();
  word closure = frame->GetClosure();
  operation op = frame->GetOperation();
  Entry *nextEntry = entry->GetNext();
  if (nextEntry != INVALID_POINTER)
    frame->SetEntry(nextEntry);
  else
    taskStack->PopFrame();
  switch (op) {
  case app:
    Scheduler::nArgs = Scheduler::ONE_ARG;
    Scheduler::currentArgs[0] = entry->GetValue();
    break;
  case appi:
    Scheduler::nArgs = 2;
    Scheduler::currentArgs[0] = Store::IntToWord(entry->GetKey());
    Scheduler::currentArgs[1] = entry->GetValue();
    break;
  case fold:
    Construct();
    Scheduler::nArgs = 2;
    Scheduler::currentArgs[1] = Scheduler::currentArgs[0];
    Scheduler::currentArgs[0] = entry->GetValue();
    break;
  case foldi:
    Construct();
    Scheduler::nArgs = 3;
    Scheduler::currentArgs[2] = Scheduler::currentArgs[0];
    Scheduler::currentArgs[0] = Store::IntToWord(entry->GetKey());
    Scheduler::currentArgs[1] = entry->GetValue();
    break;
  }
  return taskStack->PushCall(closure);
}

const char *RefMapIteratorInterpreter::Identify() {
  return "RefMapIteratorInterpreter";
}

void RefMapIteratorInterpreter::DumpFrame(word wFrame) {
  RefMapIteratorFrame *frame = RefMapIteratorFrame::FromWordDirect(wFrame);
  const char *name;
  switch (frame->GetOperation()) {
  case app: name = "app"; break;
  case appi: name = "appi"; break;
  case fold: name = "fold"; break;
  case foldi: name = "foldi"; break;
  default:
    Error("UnsafeMkRefMap: Unknown operation\n");
  }
  std::fprintf(stderr, "RefMap.%s\n", name);
}

//
// Define Native Operations
//

DEFINE0(UnsafeMkRefMap_new) {
  RETURN(RefMap::New()->ToWord());
} END

DEFINE1(UnsafeMkRefMap_clone) {
  DECLARE_REF_MAP(refMap, x0);
  Error("UnsafeMkRefMap.clone not implemented"); //--** to be done
  RETURN(refMap->ToWord());
} END

DEFINE3(UnsafeMkRefMap_insert) {
  DECLARE_REF_MAP(refMap, x0);
  DECLARE_INT(key, x1);
  refMap->Insert(key, x2);
  RETURN_UNIT;
} END

DEFINE2(UnsafeMkRefMap_delete) {
  DECLARE_REF_MAP(refMap, x0);
  DECLARE_INT(key, x1);
  refMap->Delete(key);
  RETURN_UNIT;
} END

DEFINE1(UnsafeMkRefMap_deleteAll) {
  DECLARE_REF_MAP(refMap, x0);
  refMap->DeleteAll();
  RETURN_UNIT;
} END

DEFINE2(UnsafeMkRefMap_lookup) {
  DECLARE_REF_MAP(refMap, x0);
  DECLARE_INT(key, x1);
  RETURN(refMap->Lookup(key));
} END

DEFINE2(UnsafeMkRefMap_member) {
  DECLARE_REF_MAP(refMap, x0);
  DECLARE_INT(key, x1);
  RETURN_BOOL(refMap->Member(key));
} END

DEFINE1(UnsafeMkRefMap_isEmpty) {
  DECLARE_REF_MAP(refMap, x0);
  RETURN_BOOL(refMap->IsEmpty());
} END

DEFINE2(UnsafeMkRefMap_app) {
  word closure = x0;
  DECLARE_REF_MAP(refMap, x1);
  Entry *entry = refMap->GetHead();
  if (entry != INVALID_POINTER)
    RefMapIteratorInterpreter::PushFrame(taskStack, entry, closure, app);
  RETURN_UNIT;
} END

DEFINE2(UnsafeMkRefMap_appi) {
  word closure = x0;
  DECLARE_REF_MAP(refMap, x1);
  Entry *entry = refMap->GetHead();
  if (entry != INVALID_POINTER)
    RefMapIteratorInterpreter::PushFrame(taskStack, entry, closure, appi);
  RETURN_UNIT;
} END

DEFINE3(UnsafeMkRefMap_fold) {
  word closure = x0;
  word zero = x1;
  DECLARE_REF_MAP(refMap, x2);
  Entry *entry = refMap->GetHead();
  if (entry != INVALID_POINTER)
    RefMapIteratorInterpreter::PushFrame(taskStack, entry, closure, fold);
  RETURN(zero);
} END

DEFINE3(UnsafeMkRefMap_foldi) {
  word closure = x0;
  word zero = x1;
  DECLARE_REF_MAP(refMap, x2);
  Entry *entry = refMap->GetHead();
  if (entry != INVALID_POINTER)
    RefMapIteratorInterpreter::PushFrame(taskStack, entry, closure, foldi);
  RETURN(zero);
} END

//
// Build Export Structure
//

word UnsafeMkRefMap(void) {
  RefMapIteratorInterpreter::Init();
  Tuple *t = Tuple::New(12);
  t->Init(0, Primitive::MakeClosure("UnsafeMkRefMap.app",
				    UnsafeMkRefMap_app, 2, true));
  t->Init(1, Primitive::MakeClosure("UnsafeMkRefMap.appi",
				    UnsafeMkRefMap_appi, 2, true));
  t->Init(2, Primitive::MakeClosure("UnsafeMkRefMap.clone",
				    UnsafeMkRefMap_clone, 1, true));
  t->Init(3, Primitive::MakeClosure("UnsafeMkRefMap.delete",
				    UnsafeMkRefMap_delete, 2, true));
  t->Init(4, Primitive::MakeClosure("UnsafeMkRefMap.deleteAll",
				    UnsafeMkRefMap_deleteAll, 1, true));
  t->Init(5, Primitive::MakeClosure("UnsafeMkRefMap.fold",
				    UnsafeMkRefMap_fold, 3, true));
  t->Init(6, Primitive::MakeClosure("UnsafeMkRefMap.foldi",
				    UnsafeMkRefMap_foldi, 3, true));
  t->Init(7, Primitive::MakeClosure("UnsafeMkRefMap.insert",
				    UnsafeMkRefMap_insert, 3, true));
  t->Init(8, Primitive::MakeClosure("UnsafeMkRefMap.isEmpty",
				    UnsafeMkRefMap_isEmpty, 1, true));
  t->Init(9, Primitive::MakeClosure("UnsafeMkRefMap.lookup",
				    UnsafeMkRefMap_lookup, 2, true));
  t->Init(10, Primitive::MakeClosure("UnsafeMkRefMap.member",
				    UnsafeMkRefMap_member, 2, true));
  t->Init(11, Primitive::MakeClosure("UnsafeMkRefMap.new",
				    UnsafeMkRefMap_new, 0, true));
  RETURN_STRUCTURE(t);
}
