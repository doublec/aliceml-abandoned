//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
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

#include "store.hh"

typedef unsigned short w_char;


class AliceLabel {
public:
  typedef enum {
    MIN_LABEL   = 0,
    MAX_LABEL   = (MAX_LSIZE - 14),
    Array       = (MAX_LSIZE - 13),
    ArrayZero   = (MAX_LSIZE - 12),
    Builtin     = (MAX_LSIZE - 11),
    Cell        = (MAX_LSIZE - 10),
    Constructor = (MAX_LSIZE - 9),
    ConVal      = (MAX_LSIZE - 8),
    Real        = (MAX_LSIZE - 7),
    Record      = (MAX_LSIZE - 6),
    String      = (MAX_LSIZE - 5),
    Tuple       = (MAX_LSIZE - 4),
    Vector      = (MAX_LSIZE - 3),
    VectorZero  = (MAX_LSIZE - 2),
    WideString  = (MAX_LSIZE - 1)
  } AliceDataLabel;
  
  static BlockLabel Make(int tag) {
    AliceDataLabel l = (AliceDataLabel) tag;

    Assert(l >= MIN_LABEL);
    Assert(l <= MAX_LABEL);
    return (BlockLabel) l;
  }
  static BlockLabel ToBlockLabel(AliceDataLabel l) {
    return (BlockLabel) l;
  }
};

class Array; // defined behind Vector
class Builtin; // defined behind String
class Cell;
class Constructor;
class ConVal;
class Real;
class Record;
class StreamWrapper;
class String;
class TagVal;
class Tuple;
class Vector;
class WideString;

class Cell : private Block {
private:
  static const int SIZE    = 1;
  static const int VAL_POS = 1;
public:
  using Block::ToWord;

  word GetValue()           { return GetArg(VAL_POS); }
  void SetValue(word value) { ReplaceArg(VAL_POS, value); }
  word Exchange(word value) { word val = GetArg(VAL_POS); ReplaceArg(VAL_POS, value); return val; }

  static Cell *New() {
    Cell *c = (Cell *) Store::AllocBlock(AliceLabel::ToBlockLabel(AliceLabel::Cell), SIZE);
    
    c->InitArg(VAL_POS, Store::IntToWord(0));
    return c;
  }
  static Cell *New(word value) {
    Cell *c = (Cell *) Store::AllocBlock(AliceLabel::ToBlockLabel(AliceLabel::Cell), SIZE);

    c->InitArg(VAL_POS, value);
    return c;
  }
  static Cell *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) ||
	   (p->GetLabel() == AliceLabel::ToBlockLabel(AliceLabel::Cell)));
    return (Cell *) p;
  }
};

class Constructor : private Block {
public:
  using Block::GetLabel;
  using Block::GetSize;
  using Block::ToWord;
  using Block::GetArg;
  using Block::InitArg;

  static Constructor *New() {
    Block *b = Store::AllocBlock(AliceLabel::ToBlockLabel(AliceLabel::Constructor), 1);
    // to be determined
    b->InitArg(1, Store::IntToWord(0));
    return (Constructor *) b;
  }
  static Constructor *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) ||
	   (p->GetLabel() == AliceLabel::ToBlockLabel(AliceLabel::Constructor)));
    return (Constructor *) p;
  }
};

class ConVal : private Block {
private:
  static const int CON_POS = 1;
public:
  using Block::GetLabel;
  using Block::ToWord;

  u_int GetSize() {
    return (((Block *) this)->GetSize() - 1);
  }
  void InitArg(u_int f, word v) {
    ((Block *) this)->InitArg((f + 1), v);
  }
  word GetArg(u_int f) {
    return ((Block *) this)->GetArg(f + 1);
  }

  // to be determined
  static ConVal *New(word cons, u_int n) {
    Block *b = Store::AllocBlock(AliceLabel::ToBlockLabel(AliceLabel::ConVal), (n + 1));

    b->InitArg(CON_POS, cons);
    return (ConVal *) b;
  }
  static ConVal *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) ||
	   (p->GetLabel() == AliceLabel::ToBlockLabel(AliceLabel::ConVal)));
    return (ConVal *) p;
  }
};

class Real : private Block {
private:
  static const int SIZE = 2;
public:
  using Block::ToWord;

  double GetValue() { return ((double *) (ar + 1))[0]; }

  static Real *New(double v) {
    Block *b = Store::AllocChunk(SIZE);
    word *ar = b->GetBase();

    ((double *) (ar + 1))[0] = v;
    return (Real *) b;
  }
  static Real *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == CHUNK));
    return (Real *) p;
  }
};

class Record : private Block {
public:
  using Block::GetLabel;
  using Block::GetSize;
  using Block::ToWord;
  using Block::GetArg;
  using Block::InitArg;

  static Record *New(u_int s) {
    return (Record *) Store::AllocBlock(AliceLabel::ToBlockLabel(AliceLabel::Record), s);
  }
  static Record *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) ||
	   (p->GetLabel() == AliceLabel::ToBlockLabel(AliceLabel::Record)));
    return (Record *) p;
  }
};

class String : private Block {
private:
  static const int LEN_POS = 1;
public:
  using Block::ToWord;

  char *GetValue() { return (char *) (ar + 2); }
  int GetLength()  { return Store::WordToInt(GetArg(LEN_POS)); }

  static String *New(int len) {
    Block *b = Store::AllocChunk((len + 2 * sizeof(word) - 1) / sizeof(word));

    b->InitArg(LEN_POS, Store::IntToWord(len));
    return (String *) b;
  }
  static String *New(char *str) {
    int len  = strlen(str);
    Block *b = Store::AllocChunk((len + 2 * sizeof(word) - 1) / sizeof(word));
    word *ar = b->GetBase();

    b->InitArg(LEN_POS, Store::IntToWord(len));
    memcpy((char *) (ar + 1), str, len);
    return (String *) b;
  }
  static String *New(char *str, int len) {
    Block *b = Store::AllocChunk((len + 2 * sizeof(word) - 1) / sizeof(word));
    word *ar = b->GetBase();

    b->InitArg(LEN_POS, Store::IntToWord(len));
    memcpy((char *) (ar + 1), str, len);
    return (String *) b;
  }
  static String *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == CHUNK));
    return (String *) p;
  }
};

class Builtin : private Block {
private:
  static const int SIZE     = 2;
  static const int PROC_POS = 1;
  static const int NAME_POS = 2;
public:
  int GetProc()     { return Store::WordToInt(GetArg(PROC_POS)); }
  String *GetName() { return (String *) Store::WordToBlock(GetArg(NAME_POS)); }

  static Builtin *New(int proc, char *name) {
    Block *b = Store::AllocBlock(AliceLabel::ToBlockLabel(AliceLabel::Builtin), SIZE);
    
    b->InitArg(PROC_POS, Store::IntToWord(proc));
    b->InitArg(NAME_POS, String::New(name)->ToWord());
    return (Builtin *) b;
  }
  static Builtin *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) ||
	   (p->GetLabel() == AliceLabel::ToBlockLabel(AliceLabel::Builtin)));
    return (Builtin *) p;
  }
};

class TagVal : private Block {
public:
  using Block::GetLabel;
  using Block::GetSize;
  using Block::ToWord;
  using Block::GetArg;
  using Block::InitArg;

  static TagVal *New(int tag, u_int n) {
    return (TagVal *) Store::AllocBlock(AliceLabel::Make(tag), n);
  }
  static TagVal *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) ||
	   ((p->GetLabel() >= AliceLabel::ToBlockLabel(AliceLabel::MIN_LABEL)) &&
	    (p->GetLabel() <= AliceLabel::ToBlockLabel(AliceLabel::MAX_LABEL))));
    return (TagVal *) p;
  }
};

class Tuple : private Block {
public:
  using Block::GetSize;
  using Block::ToWord;
  using Block::GetArg;
  using Block::InitArg;

  static Tuple *New(u_int s) {
    return (Tuple *) Store::AllocBlock(AliceLabel::ToBlockLabel(AliceLabel::Tuple), s);
  }
  static Tuple *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) ||
	   (p->GetLabel() == AliceLabel::ToBlockLabel(AliceLabel::Tuple)));
    return (Tuple *) p;
  }
};

class Vector : private Block {
protected:
  using Block::ReplaceArg;

  void CheckIndex(int i) {
    if ((u_int) i > GetSize()) {} //to be determined
  }
public:
  using Block::InitArg;
  using Block::ToWord;

  u_int GetSize() {
    if (HeaderOp::DecodeLabel(this) == AliceLabel::ToBlockLabel(AliceLabel::VectorZero)) {
      return 0;
    }
    else {
      return ((Block *) this)->GetSize();
    }
  }

  word GetArg(int i) { CheckIndex(i); return Block::GetArg((u_int) i); }

  static Vector *New(u_int s) {
    return (Vector *) Store::AllocBlock(AliceLabel::ToBlockLabel(AliceLabel::Vector), s);
  }
  static Vector *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) ||
	   ((p->GetLabel() == AliceLabel::ToBlockLabel(AliceLabel::Vector)) ||
	    (p->GetLabel() == AliceLabel::ToBlockLabel(AliceLabel::VectorZero))));
    return (Vector *) p;
  }
};

class Array : private Block {
protected:
  using Block::ReplaceArg;

  void CheckIndex(int i) {
    if ((u_int) i > GetSize()) {} //to be determined
  }
public:
  using Block::InitArg;
  using Block::ToWord;

  u_int GetSize() {
    if (HeaderOp::DecodeLabel(this) == AliceLabel::ToBlockLabel(AliceLabel::ArrayZero)) {
      return 0;
    }
    else {
      return ((Block *) this)->GetSize();
    }
  }

  word GetArg(int i) { CheckIndex(i); return Block::GetArg((u_int) i); }
  void SetArg(int i, word value) { CheckIndex(i); ReplaceArg((u_int) i, value); }

  static Array *New(u_int s) {
    if (s == 0) {
      return (Array *) Store::AllocBlock(AliceLabel::ToBlockLabel(AliceLabel::ArrayZero), 1);
    }
    else {
      return (Array *) Store::AllocBlock(AliceLabel::ToBlockLabel(AliceLabel::Array), s);
    }
  }
  static Array *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) ||
	   ((p->GetLabel() == AliceLabel::ToBlockLabel(AliceLabel::Array))
	    || (p->GetLabel() == AliceLabel::ToBlockLabel(AliceLabel::ArrayZero))));
    return (Array *) p;
  }
};

class WideString : private Block {
private:
  static const int LEN_POS = 1;
public:
  w_char *GetValue() { return (w_char *) (ar + 2); }

  static WideString *New(w_char *str, int len) {
    Block *b = Store::AllocChunk((sizeof(w_char) * len + 2 * sizeof(word) - 1) / sizeof(word));
    word *ar = b->GetBase();

    b->InitArg(LEN_POS, Store::IntToWord(len));
    memcpy((char *) (ar + 2), str, (len << 1));
    return (WideString *) b;
  }
  static WideString *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == CHUNK));
    return (WideString *) p;
  }
};

#endif
