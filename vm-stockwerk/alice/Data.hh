//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//   Leif Kornstaedt, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __ALICE__DATA_HH__
#define __ALICE__DATA_HH__

#if defined(INTERFACE)
#pragma interface "alice/Data.hh"
#endif

#include <cstring>
#include "generic/UniqueString.hh"
#include "generic/ConcreteRepresentation.hh"
#include "generic/Double.hh"

class Transform;

class DllExport Alice {
public:
  static const BlockLabel Array   = MIN_DATA_LABEL;
  static const BlockLabel Cell    = (BlockLabel) (MIN_DATA_LABEL + 1);
  static const BlockLabel ConVal  = (BlockLabel) (MIN_DATA_LABEL + 2);
  static const BlockLabel Record  = (BlockLabel) (MIN_DATA_LABEL + 3);
  static const BlockLabel Vector  = (BlockLabel) (MIN_DATA_LABEL + 4);
  static const BlockLabel MIN_TAG = (BlockLabel) (MIN_DATA_LABEL + 5);
  static const BlockLabel MAX_TAG = MAX_DATA_LABEL;

  static bool IsTag(BlockLabel l) {
    return l >= MIN_TAG && l <= MAX_TAG;
  }
  static BlockLabel TagToLabel(u_int l) {
    Assert(l <= MAX_TAG - MIN_TAG);
    return Store::MakeLabel(MIN_TAG + l);
  }
  static u_int LabelToTag(BlockLabel l) {
    Assert(IsTag(l));
    return l - MIN_TAG;
  }
};

class DllExport Array: private Block {
private:
  enum { LENGTH_POS, BASE_SIZE };
public:
  static const u_int maxLen = MAX_BIGBLOCKSIZE - BASE_SIZE;

  using Block::ToWord;

  static Array *New(u_int length) {
    Block *b = Store::AllocBlock(Alice::Array, BASE_SIZE + length);
    b->InitArg(LENGTH_POS, length);
    return static_cast<Array *>(b);
  }
  static Array *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == Alice::Array);
    return static_cast<Array *>(b);
  }
  static Array *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == Alice::Array);
    return static_cast<Array *>(b);
  }

  u_int GetLength() {
    return Store::DirectWordToInt(GetArg(LENGTH_POS));
  }
  void Init(u_int index, word value) {
    InitArg(BASE_SIZE + index, value);
  }
  void Update(u_int index, word value) {
    ReplaceArg(BASE_SIZE + index, value);
  }
  word Sub(u_int index) {
    return GetArg(BASE_SIZE + index);
  }
};

class DllExport Cell: private Block {
protected:
  enum { VALUE_POS, SIZE };
public:
  using Block::ToWord;

  static Cell *New() {
    return static_cast<Cell *>(Store::AllocBlock(Alice::Cell, SIZE));
  }
  static Cell *New(word value) {
    Block *b = Store::AllocBlock(Alice::Cell, SIZE);
    b->InitArg(VALUE_POS, value);
    return static_cast<Cell *>(b);
  }
  static Cell *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == Alice::Cell);
    return static_cast<Cell *>(b);
  }
  static Cell *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == Alice::Cell);
    return static_cast<Cell *>(b);
  }

  void Init(word value) {
    InitArg(VALUE_POS, value);
  }
  void Assign(word value) {
    ReplaceArg(VALUE_POS, value);
  }
  word Access() {
    return GetArg(VALUE_POS);
  }
  word Exchange(word value) {
    word val = GetArg(VALUE_POS);
    ReplaceArg(VALUE_POS, value);
    return val;
  }
};

class DllExport Constructor: private ConcreteRepresentation {
private:
  enum { NAME_POS, TRANSFORM_POS, SIZE };
  static ConcreteRepresentationHandler *handler;
public:
  using Block::ToWord;

  static void Init();

  static Constructor *New(String *name, ::Block *guid);
  static Constructor *New(String *name) {
    ConcreteRepresentation *b = ConcreteRepresentation::New(handler, SIZE);
    b->Init(NAME_POS, name->ToWord());
    b->Init(TRANSFORM_POS, Store::IntToWord(0)); // constructed lazily
    return static_cast<Constructor *>(b);
  }
  static Constructor *FromWord(word x) {
    ConcreteRepresentation *b = ConcreteRepresentation::FromWord(x);
    Assert(b == INVALID_POINTER || b->GetSize() == SIZE);
    return static_cast<Constructor *>(b);
  }
  static Constructor *FromWordDirect(word x) {
    ConcreteRepresentation *b = ConcreteRepresentation::FromWordDirect(x);
    Assert(b->GetSize() == SIZE);
    return static_cast<Constructor *>(b);
  }

  String *GetName() {
    return String::FromWordDirect(Get(NAME_POS));
  }
  Transform *GetTransform();
};

class DllExport ConVal: private Block {
protected:
  enum { CON_POS, BASE_SIZE };
public:
  using Block::ToWord;

  static ConVal *New(Block *constructor, u_int n) {
    Block *b = Store::AllocBlock(Alice::ConVal, BASE_SIZE + n);
    b->InitArg(CON_POS, constructor->ToWord());
    return static_cast<ConVal *>(b);
  }
  static ConVal *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == Alice::ConVal ||
	   b->GetLabel() == CONCRETE_LABEL);
    return static_cast<ConVal *>(b);
  }
  static ConVal *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == Alice::ConVal ||
	   b->GetLabel() == CONCRETE_LABEL);
    return static_cast<ConVal *>(b);
  }

  bool IsConVal() { // as opposed to a Constructor, see FromWord
    return GetLabel() == Alice::ConVal;
  }
  Block *GetConstructor() {
    Assert(GetLabel() == Alice::ConVal);
    return Store::DirectWordToBlock(GetArg(CON_POS));
  }
  void AssertWidth(u_int n) {
    Assert(Store::SizeToBlockSize(BASE_SIZE + n) == GetSize()); n = n;
  }
  void Init(u_int index, word value) {
    InitArg(BASE_SIZE + index, value);
  }
  word Sel(u_int index) {
    return GetArg(BASE_SIZE + index);
  }
};

#define Real Double

class DllExport TagVal: private Block {
public:
  using Block::ToWord;

  static TagVal *New(u_int tag, u_int n) {
    return static_cast<TagVal *>(Store::AllocBlock(Alice::TagToLabel(tag), n));
  }
  static TagVal *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    Assert(p == INVALID_POINTER || Alice::IsTag(p->GetLabel()));
    return static_cast<TagVal *>(p);
  }
  static TagVal *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);
    Assert(Alice::IsTag(p->GetLabel()));
    return static_cast<TagVal *>(p);
  }

  u_int GetTag() {
    return Alice::LabelToTag(GetLabel());
  }
  void AssertWidth(u_int n) {
    Assert(Store::SizeToBlockSize(n) == GetSize()); n = n;
  }
  void Init(u_int index, word value) {
    InitArg(index, value);
  }
  word Sel(u_int index) {
    return GetArg(index);
  }
};

class DllExport UniqueConstructor: public Constructor {
public:
  static UniqueConstructor *New(String *id) {
    return static_cast<UniqueConstructor *>
      (Constructor::New(id, static_cast<Block *>(id)));
  }
  static UniqueConstructor *FromWord(word x) {
    return static_cast<UniqueConstructor *>(Constructor::FromWord(x));
  }
  static UniqueConstructor *FromWordDirect(word x) {
    return static_cast<UniqueConstructor *>(Constructor::FromWordDirect(x));
  }
};

class DllExport Vector: private Block {
protected:
  enum { LENGTH_POS, BASE_SIZE };
public:
  static const u_int maxLen = MAX_BIGBLOCKSIZE - BASE_SIZE;

  using Block::ToWord;

  static Vector *New(u_int length) {
    Block *b =
      Store::AllocBlock(Alice::Vector, BASE_SIZE + length);
    b->InitArg(LENGTH_POS, length);
    return static_cast<Vector *>(b);
  }
  static Vector *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == Alice::Vector);
    return static_cast<Vector *>(b);
  }
  static Vector *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == Alice::Vector);
    return static_cast<Vector *>(b);
  }

  u_int GetLength() {
    return Store::DirectWordToInt(GetArg(LENGTH_POS));
  }
  void Init(u_int index, word value) {
    InitArg(BASE_SIZE + index, value);
  }
  void LateInit(u_int index, word value) {
    // This is only meant to be called by Vector.tabulate
    ReplaceArg(BASE_SIZE + index, value);
  }
  word Sub(u_int index) {
    return GetArg(BASE_SIZE + index);
  }
};

class DllExport Word8Array: private String {
  //--** deriving from String yields wrong equality (must be token equality)
public:
  static const u_int maxLen = String::maxSize;

  using Chunk::ToWord;
  using String::GetValue;

  static Word8Array *New(u_int length) {
    return static_cast<Word8Array *>(String::New(length));
  }
  static Word8Array *FromWord(word x) {
    return static_cast<Word8Array *>(String::FromWord(x));
  }
  static Word8Array *FromWordDirect(word x) {
    return static_cast<Word8Array *>(String::FromWordDirect(x));
  }

  u_int GetLength() {
    return GetSize();
  }
  void Init(u_int index, word value) {
    Assert(index < GetSize());
    s_int i = Store::WordToInt(value);
    Assert(i >= 0 && i <= 0xFF);
    GetValue()[index] = i;
  }
  void Update(u_int index, word value) {
    Init(index, value);
  }
  word Sub(u_int index) {
    return Store::IntToWord(GetValue()[index]);
  }
};

class DllExport Word8Vector: private String {
public:
  static const u_int maxLen = String::maxSize;

  using Chunk::ToWord;
  using String::GetValue;

  static Word8Vector *New(u_int length) {
    return static_cast<Word8Vector *>(String::New(length));
  }
  static Word8Vector *FromWord(word x) {
    return static_cast<Word8Vector *>(String::FromWord(x));
  }
  static Word8Vector *FromWordDirect(word x) {
    return static_cast<Word8Vector *>(String::FromWordDirect(x));
  }

  u_int GetLength() {
    return GetSize();
  }
  void Init(u_int index, word value) {
    Assert(index < GetSize());
    s_int i = Store::WordToInt(value);
    Assert(i >= 0 && i <= 0xFF);
    GetValue()[index] = i;
  }
  void LateInit(u_int index, word value) {
    // This is only meant to be called by Vector.tabulate
    Init(index, value);
  }
  word Sub(u_int index) {
    return Store::IntToWord(GetValue()[index]);
  }
};

class DllExport Record: private Block {
protected:
  enum { WIDTH_POS, BASE_SIZE };
public:
  using Block::ToWord;

  static Record *New(u_int n) {
    Block *b = Store::AllocBlock(Alice::Record, BASE_SIZE + n * 2);
    b->InitArg(WIDTH_POS, n);
    return static_cast<Record *>(b);
  }
  static Record *New(Vector *labels) {
    u_int n = labels->GetLength();
    Block *b = Store::AllocBlock(Alice::Record, BASE_SIZE + n * 2);
    b->InitArg(WIDTH_POS, n);
    for (u_int i = n; i--; ) {
      UniqueString *label = UniqueString::FromWordDirect(labels->Sub(i));
      b->InitArg(BASE_SIZE + i * 2, label->ToWord());
    }
    return static_cast<Record *>(b);
  }
  static Record *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    Assert(p == INVALID_POINTER || p->GetLabel() == Alice::Record);
    return static_cast<Record *>(p);
  }
  static Record *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);
    Assert(p->GetLabel() == Alice::Record);
    return static_cast<Record *>(p);
  }

  void Init(u_int i, word value) {
    InitArg(BASE_SIZE + i * 2 + 1, value);
  }
  void Init(const char *s, word value);
  void AssertLabel(u_int i, UniqueString *label) {
    Assert(GetArg(BASE_SIZE + i * 2) == label->ToWord()); i = i; label = label;
  }
  word PolySel(UniqueString *label) {
    word wLabel = label->ToWord();
    u_int n = Store::DirectWordToInt(GetArg(WIDTH_POS));
    Assert(n != 0);
    u_int index = label->Hash() % n;
    u_int i = index;
    while (true) {
      if (GetArg(BASE_SIZE + i * 2) == wLabel)
	return GetArg(BASE_SIZE + i * 2 + 1);
      i = (i + 1) % n;
      Assert(i != index);
    }
  }
};

#endif
