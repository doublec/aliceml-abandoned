#ifndef __alicedata_hh__
#define __alicedata_hh__

#if defined(INTERFACE)
#pragma interface
#endif

#include "store.hh"

typedef unsigned short w_char;

class AliceLabel : private BlockLabel {
public:
  static t_label MIN_LABEL;
  static t_label MAX_LABEL;

  static t_label Array;
  static t_label Builtin;
  static t_label Cell;
  static t_label Constructor;
  static t_label ConVal;
  static t_label Real;
  static t_label Record;
  static t_label String;
  static t_label Tuple;
  static t_label Vector;
  static t_label WideString;

  static void CreateAliceLabel();

  static t_label Make(int tag) {
    t_label l = (t_label) tag;

    Assert(l >= MIN_LABEL);
    Assert(l <= MAX_LABEL);
    return l;
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
    Cell *c = (Cell *) Store::AllocBlock(AliceLabel::Cell, SIZE);
    
    c->InitArg(VAL_POS, Store::IntToWord(0));
    return c;
  }
  static Cell *New(word value) {
    Cell *c = (Cell *) Store::AllocBlock(AliceLabel::Cell, SIZE);

    c->InitArg(VAL_POS, value);
    return c;
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
    Block *b = Store::AllocBlock(AliceLabel::Constructor, 1);
    // to be determined
    b->InitArg(1, Store::IntToWord(0));
    return (Constructor *) b;
  }
};

class ConVal : private Block {
private:
  static const int CON_POS = 1;
public:
  using Block::GetLabel;
  using Block::GetSize;
  using Block::ToWord;
  using Block::GetArg;
  using Block::InitArg;

  // to be determined
  static ConVal *New(word cons, u_int n) {
    Block *b = Store::AllocBlock(AliceLabel::ConVal, (n + 1));

    b->InitArg(CON_POS, cons);
    return (ConVal *) b;
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
};

class Record : private Block {
public:
  using Block::GetLabel;
  using Block::GetSize;
  using Block::ToWord;
  using Block::GetArg;
  using Block::InitArg;

  static Record *New(u_int s) {
    return (Record *) Store::AllocBlock(AliceLabel::Record, s);
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
    Block *b = Store::AllocChunk(2 + (len / sizeof(word)));

    b->InitArg(LEN_POS, Store::IntToWord(len));
    return (String *) b;
  }
  static String *New(char *str) {
    int len  = strlen(str);
    Block *b = Store::AllocChunk(2 + (len / sizeof(word)));
    word *ar = b->GetBase();

    b->InitArg(LEN_POS, Store::IntToWord(len));
    memcpy((char *) (ar + 2), str, len);
    return (String *) b;
  }
  static String *New(char *str, int len) {
    Block *b = Store::AllocChunk(2 + (len / sizeof(word)));
    word *ar = b->GetBase();

    b->InitArg(LEN_POS, Store::IntToWord(len));
    memcpy((char *) (ar + 2), str, len);
    return (String *) b;
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
    Block *b = Store::AllocBlock(AliceLabel::Builtin, SIZE);
    
    b->InitArg(PROC_POS, Store::IntToWord(proc));
    b->InitArg(NAME_POS, String::New(name)->ToWord());
    return (Builtin *) b;
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
};

class Tuple : private Block {
public:
  using Block::GetSize;
  using Block::ToWord;
  using Block::GetArg;
  using Block::InitArg;

  static Tuple *New(u_int s) {
    return (Tuple *) Store::AllocBlock(AliceLabel::Tuple, s);
  }
};

class Vector : private Block {
protected:
  using Block::ReplaceArg;

  void CheckIndex(int i) {
    if ((i < 1) || (i > GetSize())) {} //to be determined
  }
public:
  using Block::GetSize;
  using Block::ToWord;

  word GetValue(int i) { CheckIndex(i); return GetValue(i); }

  static Vector *New(u_int s) {
    return (Vector *) Store::AllocBlock(AliceLabel::Vector, s);
  }
};

class Array : public Vector {
public:
  void SetValue(int i, word value) { CheckIndex(i); ReplaceArg(i, value); }

  static Array *New(u_int s) {
    return (Array *) Store::AllocBlock(AliceLabel::Array, s);
  }
};

class WideString : private Block {
private:
  static const int LEN_POS = 1;
public:
  w_char *GetValue() { return (w_char *) (ar + 2); }

  static WideString *New(w_char *str, int len) {
    Block *b = Store::AllocChunk(2 + (len / sizeof(word)));
    word *ar = b->GetBase();

    b->InitArg(LEN_POS, Store::IntToWord(len));
    memcpy((char *) (ar + 2), str, (len << 1));
    return (WideString *) b;
  }
};

#endif
