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
#ifndef __ALICEDATA_HH__
#define __ALICEDATA_HH__

#if defined(INTERFACE)
#pragma interface
#endif

#include "scheduler/TaskStack.hh"
#include "scheduler/Interpreter.hh"

typedef unsigned short w_char;

class AliceLabel {
public:
  enum AliceDataLabel {
    MIN_LABEL    = 0,
    MAX_LABEL    = (MAX_DATALABELSIZE - 13),
    Array        = (MAX_DATALABELSIZE - 12),
    ArrayZero    = (MAX_DATALABELSIZE - 11),
    Cell         = (MAX_DATALABELSIZE - 10),
    Closure      = (MAX_DATALABELSIZE - 9),
    Constructor  = (MAX_DATALABELSIZE - 8),
    ConVal       = (MAX_DATALABELSIZE - 7),
    Real         = (MAX_DATALABELSIZE - 6),
    String       = (MAX_DATALABELSIZE - 5),
    Tuple        = (MAX_DATALABELSIZE - 4),
    Vector       = (MAX_DATALABELSIZE - 3),
    VectorZero   = (MAX_DATALABELSIZE - 2),
    WideString   = (MAX_DATALABELSIZE - 1)
  };
  
  static BlockLabel Make(int l) {
    Assert(l >= MIN_LABEL);
    Assert(l <= MAX_LABEL);
    return static_cast<BlockLabel>(l);
  }
  static BlockLabel ToBlockLabel(int l) {
    return static_cast<BlockLabel>(l);
  }
};

class Array: private Block {
public:
  using Block::ToWord;

  static Array *New(u_int length) {
    Block *b;
    if (length == 0) {
      b = Store::AllocBlock(AliceLabel::ToBlockLabel(AliceLabel::ArrayZero), 1);
    } else {
      b = Store::AllocBlock(AliceLabel::ToBlockLabel(AliceLabel::Array), length);
    }
    return static_cast<Array *>(b);
  }
  static Array *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == AliceLabel::ToBlockLabel(AliceLabel::Array) ||
	   b->GetLabel() == AliceLabel::ToBlockLabel(AliceLabel::ArrayZero));
    return static_cast<Array *>(b);
  }

  u_int GetLength() {
    if (GetLabel() == AliceLabel::ToBlockLabel(AliceLabel::ArrayZero)) {
      return 0;
    } else {
      return GetSize();
    }
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
  static const int SIZE    = 1;
  static const int VAL_POS = 1;
public:
  using Block::ToWord;

  static Cell *New() {
    return static_cast<Cell *>(Store::AllocBlock(AliceLabel::ToBlockLabel(AliceLabel::Cell), SIZE));
  }
  static Cell *New(word value) {
    Cell *c = static_cast<Cell *>(Store::AllocBlock(AliceLabel::ToBlockLabel(AliceLabel::Cell), SIZE));
    c->InitArg(VAL_POS, value);
    return c;
  }
  static Cell *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == AliceLabel::ToBlockLabel(AliceLabel::Cell));
    return static_cast<Cell *>(b);
  }
  word Access() {
    return GetArg(VAL_POS);
  }
  void Assign(word value) {
    ReplaceArg(VAL_POS, value);
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
    Block *b = Store::AllocBlock(AliceLabel::ToBlockLabel(AliceLabel::Constructor), 1);
    b->InitArg(1, Store::IntToWord(0)); //--** print name?
    return static_cast<Constructor *>(b);
  }
  static Constructor *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == AliceLabel::ToBlockLabel(AliceLabel::Constructor));
    return static_cast<Constructor *>(b);
  }
};

class ConVal: private Block {
private:
  static const int CON_POS = 1;
public:
  using Block::ToWord;

  static ConVal *New(Constructor *cons, u_int n) {
    Block *b = Store::AllocBlock(AliceLabel::ToBlockLabel(AliceLabel::ConVal), (n + 1));

    b->InitArg(CON_POS, cons->ToWord());
    return static_cast<ConVal *>(b);
  }
  static ConVal *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == AliceLabel::ToBlockLabel(AliceLabel::ConVal) ||
	   b->GetLabel() == AliceLabel::ToBlockLabel(AliceLabel::Constructor));
    return static_cast<ConVal *>(b);
  }

  bool IsConVal() { // as opposed to a Constructor
    return GetLabel() == AliceLabel::ToBlockLabel(AliceLabel::ConVal);
  }
  Constructor *GetConstructor() {
    Assert(GetLabel() == AliceLabel::ToBlockLabel(AliceLabel::ConVal));
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
  static const int SIZE = 2;
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
  static const int LEN_POS = 1;
public:
  using Block::ToWord;

  static String *New(int len) {
    Block *b = Store::AllocChunk((len + 2 * sizeof(word) - 1) / sizeof(word));
    b->InitArg(LEN_POS, Store::IntToWord(len));
    return static_cast<String *>(b);
  }
  static String *New(const char *str) {
    int len  = strlen(str);
    Block *b = Store::AllocChunk((len + 2 * sizeof(word) - 1) / sizeof(word));
    b->InitArg(LEN_POS, Store::IntToWord(len));
    memcpy(reinterpret_cast<char *>(b->GetBase() + 1), str, len);
    return static_cast<String *>(b);
  }
  static String *New(const char *str, int len) {
    Block *b = Store::AllocChunk((len + 2 * sizeof(word) - 1) / sizeof(word));
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
  int GetLength() {
    return Store::WordToInt(GetArg(LEN_POS));
  }
};

class TagVal: private Block {
public:
  using Block::ToWord;

  static TagVal *New(u_int tag, u_int n) {
    return static_cast<TagVal *>(Store::AllocBlock(AliceLabel::Make(tag), n));
  }
  static TagVal *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    Assert(p == INVALID_POINTER ||
	   p->GetLabel() >= AliceLabel::ToBlockLabel(AliceLabel::MIN_LABEL) &&
	   p->GetLabel() <= AliceLabel::ToBlockLabel(AliceLabel::MAX_LABEL));
    return static_cast<TagVal *>(p);
  }

  u_int GetTag() {
    return static_cast<int>(GetLabel());
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
    return static_cast<Tuple *>(Store::AllocBlock(AliceLabel::ToBlockLabel(AliceLabel::Tuple), n));
  }
  static Tuple *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == AliceLabel::ToBlockLabel(AliceLabel::Tuple));
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

class Vector: private Block {
public:
  using Block::ToWord;

  static Vector *New(u_int length) {
    return static_cast<Vector *>(Store::AllocBlock(AliceLabel::ToBlockLabel(AliceLabel::Vector), length));
  }
  static Vector *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == AliceLabel::ToBlockLabel(AliceLabel::Vector) ||
	   b->GetLabel() == AliceLabel::ToBlockLabel(AliceLabel::VectorZero));
    return static_cast<Vector *>(b);
  }

  u_int GetLength() {
    if (HeaderOp::DecodeLabel(this) == AliceLabel::ToBlockLabel(AliceLabel::VectorZero)) {
      return 0;
    } else {
      return GetSize();
    }
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
  static const int LEN_POS = 1;
public:
  using Block::ToWord;

  static WideString *New(w_char *str, int len) {
    Block *b = Store::AllocChunk((sizeof(w_char) * len + 2 * sizeof(word) - 1) / sizeof(word));
    b->InitArg(LEN_POS, Store::IntToWord(len));
    memcpy(reinterpret_cast<char *>(b->GetBase() + 1), str, len * sizeof(w_char));
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

class Closure: private Block {
private:
  static const int SIZE = 2;
  static const int CONCRETE_CODE_POS = 1;
  static const int GLOBAL_ENV_POS = 2;
public:
  using Block::ToWord;

  static Closure *New(ConcreteCode *concreteCode, Vector *globalEnv) {
    Block *b = Store::AllocBlock(AliceLabel::ToBlockLabel(AliceLabel::Closure), SIZE);
    b->InitArg(CONCRETE_CODE_POS, concreteCode->ToWord());
    b->InitArg(GLOBAL_ENV_POS, globalEnv->ToWord());
    return static_cast<Closure *>(b);
  }
  static Closure *FromWord(word x) {
    Block *b = Store::WordToBlock(x);
    Assert(b == INVALID_POINTER ||
	   b->GetLabel() == AliceLabel::ToBlockLabel(AliceLabel::Closure));
    return static_cast<Closure *>(b);
  }

  ConcreteCode *GetConcreteCode() {
    return ConcreteCode::FromWord(GetArg(CONCRETE_CODE_POS));
  }
  Vector *GetGlobalEnv() {
    return Vector::FromWord(GetArg(GLOBAL_ENV_POS));
  }
  void PushCall(TaskStack *taskStack) {
    GetConcreteCode()->GetInterpreter()->PushCall(taskStack, ToWord());
  }
};

#endif
