//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//   Leif Kornstaedt, 2000-2001
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __ALICE__DATA_HH__
#define __ALICE__DATA_HH__

#if defined(INTERFACE)
#pragma interface "emulator/Alice.hh"
#endif

#include "store/Store.hh"

#define MAX_SIZE(t) \
  (MAX_BIGBLOCKSIZE * sizeof(u_int) / sizeof(t))

class Alice {
public:
  enum label {
    Array        = MIN_DATA_LABEL,
    Cell         = MIN_DATA_LABEL + 1,
    Constructor  = MIN_DATA_LABEL + 2,
    ConVal       = MIN_DATA_LABEL + 3,
    Vector       = MIN_DATA_LABEL + 4,
    FIRST_LABEL  = Array,
    LAST_LABEL   = Vector,

    MIN_TAG      = LAST_LABEL + 1,
    MAX_TAG      = MAX_DATA_LABEL
  };

  static BlockLabel TagToLabel(u_int l) {
    Assert(l <= MAX_TAG - MIN_TAG);
    return Store::MakeLabel(MIN_TAG + l);
  }
  static bool IsTag(BlockLabel l) {
    u_int i = static_cast<u_int>(l);
    return i >= MIN_TAG && i <= MAX_TAG;
  }
  static u_int LabelToTag(BlockLabel l) {
    Assert(IsTag(l));
    return static_cast<u_int>(l) - MIN_TAG;
  }
  static BlockLabel ToBlockLabel(label l) {
    Assert(l >= FIRST_LABEL && l <= LAST_LABEL);
    return Store::MakeLabel(l);
  }
};

class Array: private Block {
private:
  static const u_int LENGTH_POS = 0;
  static const u_int BASE_SIZE = 1;
public:
  static const u_int maxLen = MAX_BIGBLOCKSIZE - BASE_SIZE;

  using Block::ToWord;

  static Array *New(u_int length) {
    Block *b =
      Store::AllocBlock(Alice::ToBlockLabel(Alice::Array), BASE_SIZE + length);
    b->InitArg(LENGTH_POS, length);
    return static_cast<Array *>(b);
  }
  static Array *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == Alice::ToBlockLabel(Alice::Array));
    return static_cast<Array *>(b);
  }
  static Array *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == Alice::ToBlockLabel(Alice::Array));
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

class Cell: private Block {
private:
  static const u_int VAL_POS = 0;
  static const u_int SIZE = 1;
public:
  using Block::ToWord;

  static Cell *New() {
    return static_cast<Cell *>(Store::AllocBlock(Alice::ToBlockLabel(Alice::Cell), SIZE));
  }
  static Cell *New(word value) {
    Cell *c = static_cast<Cell *>(Store::AllocBlock(Alice::ToBlockLabel(Alice::Cell), SIZE));
    c->InitArg(VAL_POS, value);
    return c;
  }
  static Cell *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == Alice::ToBlockLabel(Alice::Cell));
    return static_cast<Cell *>(b);
  }
  static Cell *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == Alice::ToBlockLabel(Alice::Cell));
    return static_cast<Cell *>(b);
  }

  void Init(word value) {
    InitArg(VAL_POS, value);
  }
  void Assign(word value) {
    ReplaceArg(VAL_POS, value);
  }
  word Access() {
    return GetArg(VAL_POS);
  }
  word Exchange(word value) {
    word val = GetArg(VAL_POS);
    ReplaceArg(VAL_POS, value);
    return val;
  }
};

class Constructor: private Block {
  //--** store the constructor's print name?
public:
  using Block::ToWord;

  static Constructor *New() {
    Block *b =
      Store::AllocBlock(Alice::ToBlockLabel(Alice::Constructor),
			MIN_BLOCKSIZE);
    return static_cast<Constructor *>(b);
  }
  static Constructor *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == Alice::ToBlockLabel(Alice::Constructor));
    return static_cast<Constructor *>(b);
  }
  static Constructor *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == Alice::ToBlockLabel(Alice::Constructor));
    return static_cast<Constructor *>(b);
  }
};

class ConVal: private Block {
private:
  static const u_int CON_POS = 0;
  static const u_int BASE_SIZE = 1;
public:
  using Block::ToWord;

  static ConVal *New(Constructor *cons, u_int n) {
    Block *b =
      Store::AllocBlock(Alice::ToBlockLabel(Alice::ConVal), BASE_SIZE + n);
    b->InitArg(CON_POS, cons->ToWord());
    return static_cast<ConVal *>(b);
  }
  static ConVal *FromWord(word x) {
    //--** not nice: also succeeds for Constructor
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == Alice::ToBlockLabel(Alice::ConVal) ||
	   b->GetLabel() == Alice::ToBlockLabel(Alice::Constructor));
    return static_cast<ConVal *>(b);
  }
  static ConVal *FromWordDirect(word x) {
    //--** not nice: also succeeds for constructor
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == Alice::ToBlockLabel(Alice::ConVal) ||
	   b->GetLabel() == Alice::ToBlockLabel(Alice::Constructor));
    return static_cast<ConVal *>(b);
  }

  bool IsConVal() { // as opposed to a Constructor, see above
    return GetLabel() == Alice::ToBlockLabel(Alice::ConVal);
  }
  Constructor *GetConstructor() {
    Assert(GetLabel() == Alice::ToBlockLabel(Alice::ConVal));
    return Constructor::FromWordDirect(GetArg(CON_POS));
  }
  void AssertWidth(u_int n) {
    Assert(Store::SizeToBlockSize(BASE_SIZE + n) == GetSize());
  }
  void Init(u_int index, word value) {
    InitArg(BASE_SIZE + index, value);
  }
  word Sel(u_int index) {
    return GetArg(BASE_SIZE + index);
  }
};

class Real: private Chunk {
public:
  using Chunk::ToWord;

  static Real *New(double value) {
    Chunk *chunk = Store::AllocChunk(sizeof(double));
    std::memcpy(chunk->GetBase(), &value, sizeof(double));
    return static_cast<Real *>(chunk);
  }
  static Real *FromWord(word x) {
    Chunk *chunk = Store::WordToChunk(x);
    Assert(chunk == INVALID_POINTER || chunk->GetSize() == sizeof(double));
    return static_cast<Real *>(chunk);
  }
  static Real *FromWordDirect(word x) {
    Chunk *chunk = Store::DirectWordToChunk(x);
    Assert(chunk->GetSize() == Store::SizeToChunkSize(sizeof(double)));
    return static_cast<Real *>(chunk);
  }

  double GetValue() {
    double result;
    std::memcpy(GetBase(), &result, sizeof(double));
    return result;
  }
};

class String: private Chunk {
public:
  static const u_int maxSize = MAX_SIZE(char);

  using Chunk::ToWord;

  static String *New(u_int len) {
    return static_cast<String *>(Store::AllocChunk(len));
  }
  static String *New(const char *str, u_int len) {
    Chunk *chunk = Store::AllocChunk(len);
    std::memcpy(chunk->GetBase(), str, len);
    return static_cast<String *>(chunk);
  }
  static String *New(const char *str) {
    return New(str, strlen(str));
  }
  static String *FromWord(word x) {
    Chunk *chunk = Store::WordToChunk(x);
    return static_cast<String *>(chunk);
  }
  static String *FromWordDirect(word x) {
    Chunk *chunk = Store::DirectWordToChunk(x);
    return static_cast<String *>(chunk);
  }

  u_int GetSize() {
    return Chunk::GetSize();
  }
  char *GetValue() {
    return GetBase();
  }
};

class TagVal: private Block {
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
    Assert(Store::SizeToBlockSize(n) == GetSize());
  }
  void Init(u_int index, word value) {
    InitArg(index, value);
  }
  word Sel(u_int index) {
    return GetArg(index);
  }
};

class UniqueConstructor: public Constructor {
private:
  static const u_int ID_POS = 0;
  static const u_int SIZE = 1;
public:
  static UniqueConstructor *New(String *id) {
    Block *b =
      Store::AllocBlock(Alice::ToBlockLabel(Alice::Constructor), SIZE);
    b->InitArg(ID_POS, id->ToWord());
    return static_cast<UniqueConstructor *>(b);
  }
  static UniqueConstructor *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == Alice::ToBlockLabel(Alice::Constructor));
    return static_cast<UniqueConstructor *>(b);
  }
  static UniqueConstructor *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == Alice::ToBlockLabel(Alice::Constructor));
    return static_cast<UniqueConstructor *>(b);
  }
};

class Vector: private Block {
private:
  static const u_int LENGTH_POS = 0;
  static const u_int BASE_SIZE = 1;
public:
  static const u_int maxLen = MAX_BIGBLOCKSIZE - BASE_SIZE;

  using Block::ToWord;

  static Vector *New(u_int length) {
    Block *b =
      Store::AllocBlock(Alice::ToBlockLabel(Alice::Vector),
			BASE_SIZE + length);
    b->InitArg(LENGTH_POS, length);
    return static_cast<Vector *>(b);
  }
  static Vector *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == Alice::ToBlockLabel(Alice::Vector));
    return static_cast<Vector *>(b);
  }
  static Vector *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == Alice::ToBlockLabel(Alice::Vector));
    return static_cast<Vector *>(b);
  }

  u_int GetLength() {
    return Store::DirectWordToInt(GetArg(LENGTH_POS));
  }
  void Init(u_int index, word value) {
    InitArg(BASE_SIZE + index, value);
  }
  void Replace(u_int index, word value) {
    // This is only meant to be called by Vector.tabulate.
    ReplaceArg(BASE_SIZE + index, value);
  }
  word Sub(u_int index) {
    return GetArg(BASE_SIZE + index);
  }
};

class WideString: private Chunk {
public:
  static const u_int maxSize = MAX_SIZE(wchar_t);

  using Chunk::ToWord;

  static WideString *New(wchar_t *str, u_int len) {
    u_int nchars = len * sizeof(wchar_t);
    Chunk *chunk = Store::AllocChunk(nchars);
    std::memcpy(chunk->GetBase(), str, nchars);
    return static_cast<WideString *>(chunk);
  }
  static WideString *FromWord(word x) {
    Chunk *chunk = Store::WordToChunk(x);
    return static_cast<WideString *>(chunk);
  }
  static WideString *FromWordDirect(word x) {
    Chunk *chunk = Store::DirectWordToChunk(x);
    return static_cast<WideString *>(chunk);
  }

  u_int GetSize() {
    //--** wrong! store size explicitly
    return Chunk::GetSize() / sizeof(wchar_t);
  }
  wchar_t *GetValue() {
    return reinterpret_cast<wchar_t *>(GetBase());
  }
};

#endif __ALICE__DATA_HH__
