//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cstdio>
#include "store/Map.hh"
#include "generic/Tuple.hh"
#include "alice/StackFrame.hh"
#include "alice/Authoring.hh"

static const BlockLabel ENTRY_LABEL = TUPLE_LABEL;
static const BlockLabel CELLMAP_LABEL = TUPLE_LABEL;

//
// Abstract Data Type: CellMap
//

class CellMapEntry: private Block {
private:
  enum { KEY_POS, VALUE_POS, PREV_POS, NEXT_POS, SIZE };
public:
  using Block::ToWord;

  static CellMapEntry *New(Cell *key, word value, CellMapEntry *next) {
    Block *b = Store::AllocBlock(ENTRY_LABEL, SIZE);
    b->InitArg(KEY_POS, key->ToWord());
    b->InitArg(VALUE_POS, value);
    b->InitArg(PREV_POS, 0);
    if (next != INVALID_POINTER) {
      b->InitArg(NEXT_POS, next->ToWord());
      next->ReplaceArg(PREV_POS, b->ToWord());
    } else
      b->InitArg(NEXT_POS, 0);
    return static_cast<CellMapEntry *>(b);
  }
  static CellMapEntry *FromWordDirect(word w) {
    Block *b = Store::DirectWordToBlock(w);
    Assert(b->GetLabel() == ENTRY_LABEL && b->GetSize() == SIZE);
    return static_cast<CellMapEntry *>(b);
  }

  Cell *GetKey() {
    return Cell::FromWordDirect(GetArg(KEY_POS));
  }
  word GetValue() {
    return GetArg(VALUE_POS);
  }
  void SetValue(word value) {
    ReplaceArg(VALUE_POS, value);
  }
  CellMapEntry *GetNext() {
    word next = GetArg(NEXT_POS);
    if (next == Store::IntToWord(0))
      return INVALID_POINTER;
    else
      return CellMapEntry::FromWordDirect(next);
  }

  word Unlink() {
    // Returns:
    //    Integer 1, iff unlink was completed without the need to touch head.
    //    Integer 0, iff head needs to be set to the empty list.
    //    New head, otherwise.
    word prev = GetArg(PREV_POS);
    word next = GetArg(NEXT_POS);
    if (next != Store::IntToWord(0)) {
      CellMapEntry *nextEntry = CellMapEntry::FromWordDirect(next);
      nextEntry->ReplaceArg(PREV_POS, prev);
    }
    if (prev != Store::IntToWord(0)) {
      CellMapEntry *prevEntry = CellMapEntry::FromWordDirect(prev);
      prevEntry->ReplaceArg(NEXT_POS, next);
      return Store::IntToWord(1);
    } else
      return next;
  }
};

//--** CellMaps should be picklable
class CellMap: private Block {
private:
  enum { HASHTABLE_POS, HEAD_POS, SIZE };

  static const u_int initialSize = 8; //--** to be determined

  Map *GetHashTable() {
    return Map::FromWordDirect(GetArg(HASHTABLE_POS));
  }
public:
  using Block::ToWord;

  static CellMap *New() {
    Block *b = Store::AllocBlock(CELLMAP_LABEL, SIZE);
    b->InitArg(HASHTABLE_POS, Map::New(initialSize)->ToWord());
    b->InitArg(HEAD_POS, 0);
    return static_cast<CellMap *>(b);
  }
  static CellMap *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == CELLMAP_LABEL && b->GetSize() == SIZE);
    return static_cast<CellMap *>(b);
  }

  void Insert(Cell *key, word value) {
    Map *hashTable = GetHashTable();
    word wKey = key->ToWord();
    Assert(!hashTable->IsMember(wKey));
    word head = GetArg(HEAD_POS);
    CellMapEntry *headEntry = head == Store::IntToWord(0)?
      INVALID_POINTER: CellMapEntry::FromWordDirect(head);
    CellMapEntry *entry = CellMapEntry::New(key, value, headEntry);
    ReplaceArg(HEAD_POS, entry->ToWord());
    hashTable->Put(wKey, entry->ToWord());
  }
  void Delete(Cell *key) {
    word wKey = key->ToWord();
    Map *hashTable = GetHashTable();
    Assert(hashTable->IsMember(wKey));
    CellMapEntry *entry =
      CellMapEntry::FromWordDirect(hashTable->Get(wKey));
    hashTable->Remove(wKey);
    word result = entry->Unlink();
    if (result != Store::IntToWord(1))
      ReplaceArg(HEAD_POS, result);
  }
  void DeleteAll() {
    GetHashTable()->Clear();
    ReplaceArg(HEAD_POS, 0);
  }
  CellMapEntry *LookupEntry(Cell *key) {
    word wKey = key->ToWord();
    Map *hashTable = GetHashTable();
    Assert(hashTable->IsMember(wKey));
    return CellMapEntry::FromWordDirect(hashTable->Get(wKey));
  }
  word Lookup(Cell *key) {
    word wKey = key->ToWord();
    Map *hashTable = GetHashTable();
    if (hashTable->IsMember(wKey)) {
      TagVal *some = TagVal::New(1, 1); // SOME ...
      CellMapEntry *entry =
	CellMapEntry::FromWordDirect(hashTable->Get(wKey));
      some->Init(0, entry->GetValue());
      return some->ToWord();
    } else {
      return Store::IntToWord(0); // NONE
    }
  }
  bool Member(Cell *key) {
    return GetHashTable()->IsMember(key->ToWord());
  }
  bool IsEmpty() {
    return GetHashTable()->IsEmpty();
  }
  u_int GetSize() {
    return GetHashTable()->GetSize();
  }
  CellMapEntry *GetHead() {
    word head = GetArg(HEAD_POS);
    if (head == Store::IntToWord(0))
      return INVALID_POINTER;
    else
      return CellMapEntry::FromWordDirect(head);
  }
};

#define DECLARE_CELL_MAP(cellMap, x) DECLARE_BLOCKTYPE(CellMap, cellMap, x)

//
// Worker for insertWithi
//

class CellMapInsertWorker: public Worker {
private:
  static CellMapInsertWorker *self;
  CellMapInsertWorker(): Worker() {}
public:
  static void Init() {
    self = new CellMapInsertWorker();
  }
  // Frame Handling
  static void PushFrame(CellMapEntry *entry);
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

class CellMapInsertFrame: private StackFrame {
private:
  enum { ENTRY_POS, SIZE };
public:
  using StackFrame::ToWord;

  static CellMapInsertFrame *New(Worker *worker, CellMapEntry *entry) {
    StackFrame *frame = StackFrame::New(CELLMAP_INSERT_FRAME, worker, SIZE);
    frame->InitArg(ENTRY_POS, entry->ToWord());
    return static_cast<CellMapInsertFrame *>(frame);
  }
  static CellMapInsertFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == CELLMAP_INSERT_FRAME);
    return static_cast<CellMapInsertFrame *>(p);
  }

  CellMapEntry *GetEntry() {
    return CellMapEntry::FromWordDirect(GetArg(ENTRY_POS));
  }
};

CellMapInsertWorker *CellMapInsertWorker::self;

void CellMapInsertWorker::PushFrame(CellMapEntry *entry) {
  Scheduler::PushFrame(CellMapInsertFrame::New(self, entry)->ToWord());
}

Worker::Result CellMapInsertWorker::Run() {
  CellMapInsertFrame *frame =
    CellMapInsertFrame::FromWordDirect(Scheduler::GetAndPopFrame());
  CellMapEntry *entry = frame->GetEntry();
  Construct();
  entry->SetValue(Scheduler::currentArgs[0]);
  Scheduler::nArgs = 0;
  return Worker::CONTINUE;
}

const char *CellMapInsertWorker::Identify() {
  return "CellMapInsertWorker";
}

void CellMapInsertWorker::DumpFrame(word) {
  std::fprintf(stderr, "UnsafeCell.Map.insertWithi\n");
}

//
// Worker for Iterating over CellMaps
//

class CellMapIteratorWorker: public Worker {
private:
  static CellMapIteratorWorker *self;
  CellMapIteratorWorker(): Worker() {}
public:
  enum operation { app, appi, fold, foldi };

  static void Init() {
    self = new CellMapIteratorWorker();
  }
  // Frame Handling
  static void PushFrame(CellMapEntry *entry, word closure, operation op);
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

class CellMapIteratorFrame: private StackFrame {
private:
  enum { ENTRY_POS, CLOSURE_POS, OPERATION_POS, SIZE };
public:
  using StackFrame::ToWord;

  static CellMapIteratorFrame *New(Worker *worker,
				   CellMapEntry *entry, word closure,
				   CellMapIteratorWorker::operation op) {
    StackFrame *frame = StackFrame::New(CELLMAP_ITERATOR_FRAME, worker, SIZE);
    frame->InitArg(ENTRY_POS, entry->ToWord());
    frame->InitArg(CLOSURE_POS, closure);
    frame->InitArg(OPERATION_POS, Store::IntToWord(op));
    return static_cast<CellMapIteratorFrame *>(frame);
  }
  static CellMapIteratorFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == CELLMAP_ITERATOR_FRAME);
    return static_cast<CellMapIteratorFrame *>(p);
  }

  CellMapEntry *GetEntry() {
    return CellMapEntry::FromWordDirect(GetArg(ENTRY_POS));
  }
  void SetEntry(CellMapEntry *entry) {
    ReplaceArg(ENTRY_POS, entry->ToWord());
  }
  word GetClosure() {
    return GetArg(CLOSURE_POS);
  }
  CellMapIteratorWorker::operation GetOperation() {
    return static_cast<CellMapIteratorWorker::operation>
      (Store::DirectWordToInt(GetArg(OPERATION_POS)));
  }
};

CellMapIteratorWorker *CellMapIteratorWorker::self;

void CellMapIteratorWorker::PushFrame(CellMapEntry *entry,
				      word closure, operation op) {
  CellMapIteratorFrame *frame =
    CellMapIteratorFrame::New(self, entry, closure, op);
  Scheduler::PushFrame(frame->ToWord());
}

Worker::Result CellMapIteratorWorker::Run() {
  CellMapIteratorFrame *frame =
    CellMapIteratorFrame::FromWordDirect(Scheduler::GetFrame());
  CellMapEntry *entry = frame->GetEntry();
  word closure = frame->GetClosure();
  operation op = frame->GetOperation();
  CellMapEntry *nextEntry = entry->GetNext();
  if (nextEntry != INVALID_POINTER)
    frame->SetEntry(nextEntry);
  else
    Scheduler::PopFrame();
  switch (op) {
  case app:
    Scheduler::nArgs = Scheduler::ONE_ARG;
    Scheduler::currentArgs[0] = entry->GetValue();
    break;
  case appi:
    Scheduler::nArgs = 2;
    Scheduler::currentArgs[0] = entry->GetKey()->ToWord();
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
    Scheduler::currentArgs[0] = entry->GetKey()->ToWord();
    Scheduler::currentArgs[1] = entry->GetValue();
    break;
  }
  return Scheduler::PushCall(closure);
}

const char *CellMapIteratorWorker::Identify() {
  return "CellMapIteratorWorker";
}

void CellMapIteratorWorker::DumpFrame(word wFrame) {
  CellMapIteratorFrame *frame = CellMapIteratorFrame::FromWordDirect(wFrame);
  const char *name;
  switch (frame->GetOperation()) {
  case app: name = "app"; break;
  case appi: name = "appi"; break;
  case fold: name = "fold"; break;
  case foldi: name = "foldi"; break;
  default:
    Error("unknown CellMapIteratorWorker operation\n");
  }
  std::fprintf(stderr, "UnsafeCell.Map.%s\n", name);
}

//
// Worker for Searching in CellMaps
//

class CellMapFindWorker: public Worker {
private:
  static CellMapFindWorker *self;
  CellMapFindWorker(): Worker() {}
public:
  enum operation { find, findi };

  static void Init() {
    self = new CellMapFindWorker();
  }
  // Frame Handling
  static void PushFrame(CellMapEntry *entry, word closure, operation op);
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

class CellMapFindFrame: private StackFrame {
private:
  enum { ENTRY_POS, CLOSURE_POS, OPERATION_POS, SIZE };
public:
  using StackFrame::ToWord;

  static CellMapFindFrame *New(Worker *worker,
			       CellMapEntry *entry, word closure,
			       CellMapFindWorker::operation op) {
    StackFrame *frame =
      StackFrame::New(CELLMAP_FIND_FRAME, worker, SIZE);
    frame->InitArg(ENTRY_POS, entry->ToWord());
    frame->InitArg(CLOSURE_POS, closure);
    frame->InitArg(OPERATION_POS, Store::IntToWord(op));
    return static_cast<CellMapFindFrame *>(frame);
  }
  static CellMapFindFrame *FromWordDirect(word frame) {
    StackFrame *p = StackFrame::FromWordDirect(frame);
    Assert(p->GetLabel() == CELLMAP_FIND_FRAME);
    return static_cast<CellMapFindFrame *>(p);
  }

  CellMapEntry *GetEntry() {
    return CellMapEntry::FromWordDirect(GetArg(ENTRY_POS));
  }
  void SetEntry(CellMapEntry *entry) {
    ReplaceArg(ENTRY_POS, entry->ToWord());
  }
  word GetClosure() {
    return GetArg(CLOSURE_POS);
  }
  CellMapFindWorker::operation GetOperation() {
    return static_cast<CellMapFindWorker::operation>
      (Store::DirectWordToInt(GetArg(OPERATION_POS)));
  }
};

CellMapFindWorker *CellMapFindWorker::self;

void CellMapFindWorker::PushFrame(CellMapEntry *entry,
				  word closure, operation op) {
  CellMapFindFrame *frame = CellMapFindFrame::New(self, entry, closure, op);
  Scheduler::PushFrame(frame->ToWord());
}

Worker::Result CellMapFindWorker::Run() {
  CellMapFindFrame *frame =
    CellMapFindFrame::FromWordDirect(Scheduler::GetFrame());
  CellMapEntry *entry = frame->GetEntry();
  word closure = frame->GetClosure();
  operation op = frame->GetOperation();
  Assert(Scheduler::nArgs == Scheduler::ONE_ARG);
  switch (Store::WordToInt(Scheduler::currentArgs[0])) {
  case 0: // false
    entry = entry->GetNext();
    if (entry != INVALID_POINTER) {
      frame->SetEntry(entry);
      switch (op) {
      case find:
	Scheduler::nArgs = Scheduler::ONE_ARG;
	Scheduler::currentArgs[0] = entry->GetValue();
	break;
      case findi:
	Scheduler::nArgs = 2;
	Scheduler::currentArgs[0] = entry->GetKey()->ToWord();
	Scheduler::currentArgs[1] = entry->GetValue();
	break;
      }
      return Scheduler::PushCall(closure);
    } else {
      Scheduler::PopFrame();
      Scheduler::nArgs = Scheduler::ONE_ARG;
      Scheduler::currentArgs[0] = Store::IntToWord(0); // NONE
      return Worker::CONTINUE;
    }
  case 1: // true
    {
      Scheduler::PopFrame();
      TagVal *some = TagVal::New(1, 1); // SOME ...
      switch (op) {
      case find:
	some->Init(0, entry->GetValue());
	break;
      case findi:
	Tuple *pair = Tuple::New(2);
	pair->Init(0, entry->GetKey()->ToWord());
	pair->Init(1, entry->GetValue());
	some->Init(0, pair->ToWord());
	break;
      }
      Scheduler::nArgs = Scheduler::ONE_ARG;
      Scheduler::currentArgs[0] = some->ToWord();
      return Worker::CONTINUE;
    }
  case INVALID_INT:
    Scheduler::currentData = Scheduler::currentArgs[0];
    return Worker::REQUEST;
  default:
    Error("CellMapFindWorker: boolean expected");
  }
}

const char *CellMapFindWorker::Identify() {
  return "CellMapFindWorker";
}

void CellMapFindWorker::DumpFrame(word wFrame) {
  CellMapFindFrame *frame = CellMapFindFrame::FromWordDirect(wFrame);
  const char *name;
  switch (frame->GetOperation()) {
  case find: name = "find"; break;
  case findi: name = "findi"; break;
  default:
    Error("unknown CellMapFindWorker operation\n");
  }
  std::fprintf(stderr, "UnsafeCell.Map.%s\n", name);
}

//
// Primitives
//

DEFINE1(UnsafeCell_cell) {
  RETURN(Cell::New(x0)->ToWord());
} END

DEFINE1(UnsafeCell_content) {
  DECLARE_CELL(cell, x0);
  RETURN(cell->Access());
} END

DEFINE2(UnsafeCell_replace) {
  DECLARE_CELL(cell, x0);
  cell->Assign(x1);
  RETURN_UNIT;
} END

DEFINE0(UnsafeCell_Map_map) {
  RETURN(CellMap::New()->ToWord());
} END

DEFINE1(UnsafeCell_Map_clone) {
  DECLARE_CELL_MAP(cellMap, x0);
  CellMap *newCellMap = CellMap::New();
  CellMapEntry *entry = cellMap->GetHead();
  while (entry != INVALID_POINTER) {
    newCellMap->Insert(entry->GetKey(), entry->GetValue());
    entry = entry->GetNext();
  }
  RETURN(newCellMap->ToWord());
} END

DEFINE4(UnsafeCell_Map_insertWithi) {
  word closure = x0;
  DECLARE_CELL_MAP(cellMap, x1);
  DECLARE_CELL(key, x2);
  word value = x3;
  if (cellMap->Member(key)) {
    CellMapEntry *entry = cellMap->LookupEntry(key);
    CellMapInsertWorker::PushFrame(entry);
    Scheduler::nArgs = 3;
    Scheduler::currentArgs[0] = key->ToWord();
    Scheduler::currentArgs[1] = entry->GetValue();
    Scheduler::currentArgs[2] = value;
    return Scheduler::PushCall(closure);
  } else {
    cellMap->Insert(key, value);
    RETURN_UNIT;
  }
} END

DEFINE3(UnsafeCell_Map_deleteWith) {
  word closure = x0;
  DECLARE_CELL_MAP(cellMap, x1);
  DECLARE_CELL(key, x2);
  if (cellMap->Member(key)) {
    cellMap->Delete(key);
    RETURN_UNIT;
  } else {
    Scheduler::nArgs = Scheduler::ONE_ARG;
    Scheduler::currentArgs[0] = key->ToWord();
    return Scheduler::PushCall(closure);
  }
} END

DEFINE1(UnsafeCell_Map_deleteAll) {
  DECLARE_CELL_MAP(cellMap, x0);
  cellMap->DeleteAll();
  RETURN_UNIT;
} END

DEFINE2(UnsafeCell_Map_lookup) {
  DECLARE_CELL_MAP(cellMap, x0);
  DECLARE_CELL(key, x1);
  RETURN(cellMap->Lookup(key));
} END

DEFINE1(UnsafeCell_Map_isEmpty) {
  DECLARE_CELL_MAP(cellMap, x0);
  RETURN_BOOL(cellMap->IsEmpty());
} END

DEFINE1(UnsafeCell_Map_size) {
  DECLARE_CELL_MAP(cellMap, x0);
  RETURN_INT(cellMap->GetSize());
} END

DEFINE2(UnsafeCell_Map_app) {
  word closure = x0;
  DECLARE_CELL_MAP(cellMap, x1);
  CellMapEntry *entry = cellMap->GetHead();
  if (entry != INVALID_POINTER)
    CellMapIteratorWorker::PushFrame(entry, closure,
				     CellMapIteratorWorker::app);
  RETURN_UNIT;
} END

DEFINE2(UnsafeCell_Map_appi) {
  word closure = x0;
  DECLARE_CELL_MAP(cellMap, x1);
  CellMapEntry *entry = cellMap->GetHead();
  if (entry != INVALID_POINTER)
    CellMapIteratorWorker::PushFrame(entry, closure,
				     CellMapIteratorWorker::appi);
  RETURN_UNIT;
} END

DEFINE3(UnsafeCell_Map_fold) {
  word closure = x0;
  word zero = x1;
  DECLARE_CELL_MAP(cellMap, x2);
  CellMapEntry *entry = cellMap->GetHead();
  if (entry != INVALID_POINTER)
    CellMapIteratorWorker::PushFrame(entry, closure,
				     CellMapIteratorWorker::fold);
  RETURN(zero);
} END

DEFINE3(UnsafeCell_Map_foldi) {
  word closure = x0;
  word zero = x1;
  DECLARE_CELL_MAP(cellMap, x2);
  CellMapEntry *entry = cellMap->GetHead();
  if (entry != INVALID_POINTER)
    CellMapIteratorWorker::PushFrame(entry, closure,
				     CellMapIteratorWorker::foldi);
  RETURN(zero);
} END

DEFINE2(UnsafeCell_Map_find) {
  word closure = x0;
  DECLARE_CELL_MAP(cellMap, x1);
  CellMapEntry *entry = cellMap->GetHead();
  if (entry != INVALID_POINTER) {
    CellMapFindWorker::PushFrame(entry, closure,
				 CellMapFindWorker::find);
    Scheduler::nArgs = Scheduler::ONE_ARG;
    Scheduler::currentArgs[0] = entry->GetValue();
    return Scheduler::PushCall(closure);
  } else {
    RETURN_INT(0); // NONE
  }
} END

DEFINE2(UnsafeCell_Map_findi) {
  word closure = x0;
  DECLARE_CELL_MAP(cellMap, x1);
  CellMapEntry *entry = cellMap->GetHead();
  if (entry != INVALID_POINTER) {
    CellMapFindWorker::PushFrame(entry, closure,
				 CellMapFindWorker::findi);
    Scheduler::nArgs = 2;
    Scheduler::currentArgs[0] = entry->GetKey()->ToWord();
    Scheduler::currentArgs[1] = entry->GetValue();
    return Scheduler::PushCall(closure);
  } else {
    RETURN_INT(0); // NONE
  }
} END

static word UnsafeCell_Map() {
  Record *record = Record::New(14);
  INIT_STRUCTURE(record, "UnsafeCell.Map", "map",
		 UnsafeCell_Map_map, 0);
  INIT_STRUCTURE(record, "UnsafeCell.Map", "clone",
		 UnsafeCell_Map_clone, 1);
  INIT_STRUCTURE(record, "UnsafeCell.Map", "insertWithi",
		 UnsafeCell_Map_insertWithi, 4);
  INIT_STRUCTURE(record, "UnsafeCell.Map", "deleteWith",
		 UnsafeCell_Map_deleteWith, 3);
  INIT_STRUCTURE(record, "UnsafeCell.Map", "deleteAll",
		 UnsafeCell_Map_deleteAll, 1);
  INIT_STRUCTURE(record, "UnsafeCell.Map", "lookup",
		 UnsafeCell_Map_lookup, 2);
  INIT_STRUCTURE(record, "UnsafeCell.Map", "isEmpty",
		 UnsafeCell_Map_isEmpty, 1);
  INIT_STRUCTURE(record, "UnsafeCell.Map", "size",
		 UnsafeCell_Map_size, 1);
  INIT_STRUCTURE(record, "UnsafeCell.Map", "app",
		 UnsafeCell_Map_app, 2);
  INIT_STRUCTURE(record, "UnsafeCell.Map", "appi",
		 UnsafeCell_Map_appi, 2);
  INIT_STRUCTURE(record, "UnsafeCell.Map", "fold",
		 UnsafeCell_Map_fold, 3);
  INIT_STRUCTURE(record, "UnsafeCell.Map", "foldi",
		 UnsafeCell_Map_foldi, 3);
  INIT_STRUCTURE(record, "UnsafeCell.Map", "find",
		 UnsafeCell_Map_find, 2);
  INIT_STRUCTURE(record, "UnsafeCell.Map", "findi",
		 UnsafeCell_Map_findi, 2);
  return record->ToWord();
}

word UnsafeCell() {
  CellMapInsertWorker::Init();
  CellMapIteratorWorker::Init();
  CellMapFindWorker::Init();

  Record *record = Record::New(4);
  INIT_STRUCTURE(record, "UnsafeCell", "cell",
		 UnsafeCell_cell, 1);
  INIT_STRUCTURE(record, "UnsafeCell", "content",
		 UnsafeCell_content, 1);
  INIT_STRUCTURE(record, "UnsafeCell", "replace",
		 UnsafeCell_replace, 2);
  record->Init("Map$", UnsafeCell_Map());
  RETURN_STRUCTURE("UnsafeCell$", record);
}
