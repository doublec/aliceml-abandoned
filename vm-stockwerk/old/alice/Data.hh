//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __DATALAYER__ALICE_HH__
#define __DATALAYER__ALICE_HH__

#if defined(INTERFACE)
#pragma interface "datalayer/Alice.hh"
#endif

#include "store/Store.hh"

#define MAX_SIZE(t) \
  (MAX_BLOCKSIZE * sizeof(u_int) / sizeof(t))

class Alice {
public:
  enum label {
    Array        = MIN_DATA_LABEL,
    ArrayZero    = MIN_DATA_LABEL + 1,
    Cell         = MIN_DATA_LABEL + 2,
    Constructor  = MIN_DATA_LABEL + 3,
    ConVal       = MIN_DATA_LABEL + 4,
    GlobalStamp  = MIN_DATA_LABEL + 5,
    Tuple        = MIN_DATA_LABEL + 6,
    Vector       = MIN_DATA_LABEL + 7,
    VectorZero   = MIN_DATA_LABEL + 8,
    FIRST_LABEL  = MIN_DATA_LABEL,
    LAST_LABEL   = MIN_DATA_LABEL + 8,

    MIN_TAG      = MIN_DATA_LABEL + 9,
    MAX_TAG      = MAX_DATA_LABEL
  };

  static BlockLabel TagToLabel(u_int l) {
    Assert(l <= MAX_TAG - MIN_TAG);
    return Store::MakeLabel(MIN_TAG + l);
  }
  static bool IsTag(BlockLabel l) {
    u_int i = static_cast<u_int>(l);
    return i >= MAX_TAG && i <= MIN_TAG;
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
public:
  static const u_int maxLen = MAX_BLOCKSIZE;

  using Block::ToWord;

  static Array *New(u_int length) {
    Block *b;
    if (length == 0) {
      b = Store::AllocBlock(Alice::ToBlockLabel(Alice::ArrayZero), 1);
    } else {
      b = Store::AllocBlock(Alice::ToBlockLabel(Alice::Array), length);
    }
    return static_cast<Array *>(b);
  }
  static Array *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == Alice::ToBlockLabel(Alice::Array) ||
	   b->GetLabel() == Alice::ToBlockLabel(Alice::ArrayZero));
    return static_cast<Array *>(b);
  }
  static Array *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == Alice::ToBlockLabel(Alice::Array) ||
	   b->GetLabel() == Alice::ToBlockLabel(Alice::ArrayZero));
    return static_cast<Array *>(b);
  }

  u_int GetLength() {
    return GetLabel() == Alice::ToBlockLabel(Alice::ArrayZero)? 0: GetSize();
  }
  void Init(u_int index, word value) {
    InitArg(index + 1, value);
  }
  void Update(u_int index, word value) {
    ReplaceArg(index + 1, value);
  }
  word Sub(u_int index) {
    return GetArg(index + 1);
  }
};

class Cell: private Block {
private:
  static const u_int SIZE    = 1;
  static const u_int VAL_POS = 1;
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
public:
  using Block::ToWord;

  static Constructor *New() {
    Block *b = Store::AllocBlock(Alice::ToBlockLabel(Alice::Constructor), 1);
    b->InitArg(1, 0); //--** print name?
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
  static const u_int CON_POS = 1;
public:
  using Block::ToWord;

  static ConVal *New(Constructor *cons, u_int n) {
    Block *b = Store::AllocBlock(Alice::ToBlockLabel(Alice::ConVal), n + 1);

    b->InitArg(CON_POS, cons->ToWord());
    return static_cast<ConVal *>(b);
  }
  static ConVal *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == Alice::ToBlockLabel(Alice::ConVal) ||
	   b->GetLabel() == Alice::ToBlockLabel(Alice::Constructor));
    return static_cast<ConVal *>(b);
  }
  static ConVal *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == Alice::ToBlockLabel(Alice::ConVal) ||
	   b->GetLabel() == Alice::ToBlockLabel(Alice::Constructor));
    return static_cast<ConVal *>(b);
  }

  bool IsConVal() { // as opposed to a Constructor
    return GetLabel() == Alice::ToBlockLabel(Alice::ConVal);
  }
  Constructor *GetConstructor() {
    Assert(GetLabel() == Alice::ToBlockLabel(Alice::ConVal));
    return Constructor::FromWordDirect(GetArg(1));
  }
  u_int GetWidth() {
    return GetSize() - 1;
  }
  void Init(u_int index, word value) {
    InitArg(index + 2, value);
  }
  word Sel(u_int index) {
    return GetArg(index + 2);
  }
};

class Real: private Chunk {
public:
  using Chunk::ToWord;

  static Real *New(double value) {
    Chunk *chunk = Store::AllocChunk(sizeof(double));
    memcpy(chunk->GetBase(), &value, sizeof(double));
    return static_cast<Real *>(chunk);
  }
  static Real *FromWord(word x) {
    Chunk *chunk = Store::WordToChunk(x);
    Assert(chunk == INVALID_POINTER || chunk->GetSize() == sizeof(double));
    return static_cast<Real *>(chunk);
  }
  static Real *FromWordDirect(word x) {
    Chunk *chunk = Store::DirectWordToChunk(x);
    Assert(chunk->GetSize() == sizeof(double));
    return static_cast<Real *>(chunk);
  }

  double GetValue() {
    double result;
    memcpy(GetBase(), &result, sizeof(double));
    return result;
  }
};

class String: private Chunk {
public:
  static const u_int maxSize = MAX_SIZE(char);

  using Chunk::ToWord;
  using Chunk::GetSize;

  static String *New(u_int len) {
    return static_cast<String *>(Store::AllocChunk(len));
  }
  static String *New(const char *str, u_int len) {
    Chunk *chunk = Store::AllocChunk(len);
    memcpy(chunk->GetBase(), str, len);
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
  u_int GetWidth() {
    return GetSize();
  }
  void Init(u_int index, word value) {
    InitArg(index + 1, value);
  }
  word Sel(u_int index) {
    return GetArg(index + 1);
  }
};

class Tuple: private Block {
public:
  using Block::ToWord;

  static Tuple *New(u_int n) {
    return static_cast<Tuple *>(Store::AllocBlock(Alice::ToBlockLabel(Alice::Tuple), n));
  }
  static Tuple *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == Alice::ToBlockLabel(Alice::Tuple));
    return static_cast<Tuple *>(b);
  }
  static Tuple *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == Alice::ToBlockLabel(Alice::Tuple));
    return static_cast<Tuple *>(b);
  }

  u_int GetWidth() {
    return GetSize();
  }
  void Init(u_int index, word value) {
    InitArg(index + 1, value);
  }
  word Sel(u_int index) {
    return GetArg(index + 1);
  }
};

class UniqueConstructor: public Constructor {
public:
  static UniqueConstructor *New(String *id) {
    Block *b = Store::AllocBlock(Alice::ToBlockLabel(Alice::Constructor), 1);
    b->InitArg(1, id->ToWord());
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

class GlobalStamp: private Block {
private:
  static const u_int SIZE = 2;
  static const u_int HASH_CODE_POS = 1;
  static const u_int NAME_POS = 2;

  static u_int hashCode;
public:
  using Block::ToWord;

  static GlobalStamp *New() {
    Block *b = Store::AllocBlock(Alice::ToBlockLabel(Alice::GlobalStamp), SIZE);
    b->InitArg(HASH_CODE_POS, hashCode++);
    return static_cast<GlobalStamp *>(b);
  }
  static GlobalStamp *New(String *name) {
    Block *b = Store::AllocBlock(Alice::ToBlockLabel(Alice::GlobalStamp), SIZE);
    b->InitArg(HASH_CODE_POS, hashCode++);
    b->InitArg(NAME_POS, name->ToWord());
    return static_cast<GlobalStamp *>(b);
  }
  static GlobalStamp *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == Alice::ToBlockLabel(Alice::GlobalStamp));
    return static_cast<GlobalStamp *>(b);
  }
  static GlobalStamp *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == Alice::ToBlockLabel(Alice::GlobalStamp));
    return static_cast<GlobalStamp *>(b);
  }

  u_int GetHashCode() {
    return Store::DirectWordToInt(GetArg(HASH_CODE_POS));
  }
  String *GetName() {
    return String::FromWord(GetArg(NAME_POS));
  }
};

class Vector: private Block {
public:
  static const u_int maxLen = MAX_BLOCKSIZE;

  using Block::ToWord;

  static Vector *New(u_int length) {
    return static_cast<Vector *>(Store::AllocBlock(Alice::ToBlockLabel(Alice::Vector), length));
  }
  static Vector *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == Alice::ToBlockLabel(Alice::Vector) ||
	   b->GetLabel() == Alice::ToBlockLabel(Alice::VectorZero));
    return static_cast<Vector *>(b);
  }
  static Vector *FromWordDirect(word x) {
    Block *b = Store::DirectWordToBlock(x);
    Assert(b->GetLabel() == Alice::ToBlockLabel(Alice::Vector) ||
	   b->GetLabel() == Alice::ToBlockLabel(Alice::VectorZero));
    return static_cast<Vector *>(b);
  }

  u_int GetLength() {
    return GetLabel() == Alice::ToBlockLabel(Alice::VectorZero)? 0: GetSize();
  }
  void Init(u_int index, word value) {
    InitArg(index + 1, value);
  }
  void Replace(u_int index, word value) {
    // This is only meant to be called by Vector.tabulate.
    ReplaceArg(index + 1, value);
  }
  word Sub(u_int index) {
    return GetArg(index + 1);
  }
};

class WideString: private Chunk {
public:
  static const u_int maxSize = MAX_SIZE(wchar_t);

  using Chunk::ToWord;

  static WideString *New(wchar_t *str, u_int len) {
    u_int nchars = len * sizeof(wchar_t);
    Chunk *chunk = Store::AllocChunk(nchars);
    memcpy(chunk->GetBase(), str, nchars);
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
    return Chunk::GetSize() / sizeof(wchar_t);
  }
  wchar_t *GetValue() {
    return reinterpret_cast<wchar_t *>(GetBase());
  }
};

#endif __DATALAYER__ALICE_HH__
