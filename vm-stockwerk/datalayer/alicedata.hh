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

#ifndef __DATALAYER__ALICEDATA_HH__
#define __DATALAYER__ALICEDATA_HH__

#if defined(INTERFACE)
#pragma interface "datalayer/alicedata.hh"
#endif

#include "store/store.hh"

#define WORDS_NEEDED(n, t) \
  ((sizeof(t) * n + sizeof(word) - 1) / sizeof(word))

typedef unsigned short w_char;

class Alice {
public:
  enum label {
    MIN_LABEL    = MIN_DATALABELSIZE,
    MAX_LABEL    = MAX_DATALABELSIZE - 9,

    Array        = MAX_DATALABELSIZE - 8,
    ArrayZero    = MAX_DATALABELSIZE - 7,
    Cell         = MAX_DATALABELSIZE - 6,
    Constructor  = MAX_DATALABELSIZE - 5,
    ConVal       = MAX_DATALABELSIZE - 4,
    GlobalStamp  = MAX_DATALABELSIZE - 3,
    Tuple        = MAX_DATALABELSIZE - 2,
    Vector       = MAX_DATALABELSIZE - 1,
    VectorZero   = MAX_DATALABELSIZE,
    LAST_LABEL   = VectorZero
  };
  
  static BlockLabel MakeLabel(u_int l) {
    Assert(l <= MAX_LABEL - MIN_LABEL);
    return Store::MakeLabel(MIN_LABEL + l);
  }
  static BlockLabel ToBlockLabel(label l) {
    Assert(l > MAX_LABEL && l <= LAST_LABEL);
    return Store::MakeLabel(l);
  }
};

class Array: private Block {
public:
  static const u_int maxLen = 0x0FFFFFFF; //--** is this correct?

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
    b->InitArg(1, Store::IntToWord(0)); //--** print name?
    return static_cast<Constructor *>(b);
  }
  static Constructor *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == Alice::ToBlockLabel(Alice::Constructor));
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

  bool IsConVal() { // as opposed to a Constructor
    return GetLabel() == Alice::ToBlockLabel(Alice::ConVal);
  }
  Constructor *GetConstructor() {
    Assert(GetLabel() == Alice::ToBlockLabel(Alice::ConVal));
    return Constructor::FromWord(GetArg(1));
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

class Real: private Block {
private:
  static const u_int SIZE = 2;
public:
  using Block::ToWord;

  //--** alignment?
  static Real *New(double v) {
    Block *b = Store::AllocChunk(SIZE);
    reinterpret_cast<double *>(b->GetBase())[0] = v;
    return static_cast<Real *>(b);
  }
  static Real *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == CHUNK && b->GetSize() == SIZE);
    return static_cast<Real *>(b);
  }

  double GetValue() {
    return reinterpret_cast<double *>(GetBase())[0];
  }
};

class String: private Block {
private:
  static const u_int LEN_POS = 1;
public:
  static const u_int maxSize = 0x3FFFFFFF; //--** is this correct?

  using Block::ToWord;

  static String *New(u_int len) {
    Block *b = Store::AllocChunk(WORDS_NEEDED(len, char) + 1);
    b->InitArg(LEN_POS, Store::IntToWord(len));
    return static_cast<String *>(b);
  }
  static String *New(const char *str) {
    u_int len  = strlen(str);
    Block *b = Store::AllocChunk(WORDS_NEEDED(len, char) + 1);
    b->InitArg(LEN_POS, Store::IntToWord(len));
    memcpy(reinterpret_cast<char *>(b->GetBase() + 1), str, len);
    return static_cast<String *>(b);
  }
  static String *New(const char *str, u_int len) {
    Block *b = Store::AllocChunk(WORDS_NEEDED(len, char) + 1);
    b->InitArg(LEN_POS, Store::IntToWord(len));
    memcpy(reinterpret_cast<char *>(b->GetBase() + 1), str, len);
    return static_cast<String *>(b);
  }
  static String *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == CHUNK);
    return static_cast<String *>(b);
  }

  char *GetValue() {
    return reinterpret_cast<char *>(GetBase() + 1);
  }
  u_int GetLength() {
    return Store::WordToInt(GetArg(LEN_POS));
  }
};

class TagVal: private Block {
public:
  using Block::ToWord;

  static TagVal *New(u_int tag, u_int n) {
    return static_cast<TagVal *>(Store::AllocBlock(Alice::MakeLabel(tag), n));
  }
  static TagVal *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    Assert(p == INVALID_POINTER ||
	   p->GetLabel() >= Alice::ToBlockLabel(Alice::MIN_LABEL) &&
	   p->GetLabel() <= Alice::ToBlockLabel(Alice::MAX_LABEL));
    return static_cast<TagVal *>(p);
  }

  u_int GetTag() {
    return static_cast<u_int>(GetLabel());
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
    b->InitArg(HASH_CODE_POS, Store::IntToWord(hashCode++));
    return static_cast<GlobalStamp *>(b);
  }
  static GlobalStamp *New(String *name) {
    Block *b = Store::AllocBlock(Alice::ToBlockLabel(Alice::GlobalStamp), SIZE);
    b->InitArg(HASH_CODE_POS, Store::IntToWord(hashCode++));
    b->InitArg(NAME_POS, name->ToWord());
    return static_cast<GlobalStamp *>(b);
  }
  static GlobalStamp *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == Alice::ToBlockLabel(Alice::GlobalStamp));
    return static_cast<GlobalStamp *>(b);
  }

  u_int GetHashCode() {
    return Store::WordToInt(GetArg(HASH_CODE_POS));
  }
  String *GetName() {
    return static_cast<String *>(Store::WordToBlock(GetArg(NAME_POS)));
  }
};

class Vector: private Block {
public:
  static const u_int maxLen = 0x0FFFFFFF; //--** is this correct?

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

  u_int GetLength() {
    return GetLabel() == Alice::ToBlockLabel(Alice::VectorZero)? 0: GetSize();
  }
  void Init(u_int index, word value) {
    InitArg(index + 1, value);
  }
  word Sub(u_int index) {
    return GetArg(index + 1);
  }
};

class WideString: private Block {
private:
  static const u_int LEN_POS = 1;
public:
  static const u_int maxSize = 0x1FFFFFFF; //--** is this correct?

  using Block::ToWord;

  static WideString *New(w_char *str, u_int len) {
    Block *b = Store::AllocChunk(WORDS_NEEDED(len, w_char) + 1);
    b->InitArg(LEN_POS, Store::IntToWord(len));
    memcpy(reinterpret_cast<char *>(b->GetBase() + 1), str,
	   len * sizeof(w_char));
    return static_cast<WideString *>(b);
  }
  static WideString *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER || b->GetLabel() == CHUNK);
    return static_cast<WideString *>(b);
  }

  w_char *GetValue() {
    return reinterpret_cast<w_char *>(GetBase() + 1);
  }
};

#undef WORDS_NEEDED

#endif __DATALAYER__ALICEDATA_HH__
