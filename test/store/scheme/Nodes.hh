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
#ifndef __TEST__STORE__SCHEME__NODES_HH__
#define __TEST__STORE__SCHEME__NODES_HH__

#include "store/Store.hh"
#include "adt/Stack.hh"

// Bison Node Types

typedef enum {
  T_INT,
  T_STRING,
  T_ID,
  T_IF,
  T_DEFINE,
  T_LAMBDA,
  T_APPLICATION,
  T_APPLY,
  T_LET,
  T_DECLARR,
  T_EXPRARR,
  T_IDARR,
  T_CONS,
  T_NIL,
  T_SELECTION,
  T_REMOVE,
  T_ASSIGN,
  T_PRIMOP
} NodeType;

// String Helper Node (extracted from AdL)

class String : private Block {
private:
  static const u_int LEN_POS = 1;
public:
  using Block::ToWord;

  char *GetValue() { return (char *) (GetBase() + 1); }
  int Length()     { return Store::WordToInt(GetArg(LEN_POS)); }

  static String *New(char *str) {
    int len  = strlen(str);
    Block *b = Store::AllocChunk((len + 2 * sizeof(word)) / sizeof(word));
    word *ar = b->GetBase();

    b->InitArg(LEN_POS, Store::IntToWord(len));
    std::memcpy((char *) (ar + 1), str, len + 1);
    return (String *) b;
  }
  static String *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == CHUNK_LABEL));
    return (String *) p;
  }
};

// Bison Tree Nodes

class IntNode : private Block {
private:
  static const u_int SIZE    = 1;
  static const u_int INT_POS = 1;
public:
  using Block::ToWord;

  int GetInt() {
    return Store::WordToInt(GetArg(INT_POS));
  }
  
  static Block *New(int i) {
    Block *p = Store::AllocBlock((BlockLabel) T_INT, SIZE);

    p->InitArg(INT_POS, Store::IntToWord(i));

    return p;
  }
  static IntNode *FromBlock(Block *x) {
    return (IntNode *) x;
  }
  static IntNode *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_INT));
    return FromBlock(p);
  }
};

class StringNode : private Block {
private:
  static const u_int SIZE       = 1;
  static const u_int STRING_POS = 1;
public:
  using Block::ToWord;

  char *GetString() {
    return ((String *) Store::WordToBlock(GetArg(STRING_POS)))->GetValue();
  }

  static Block *New(char *s) {
    Block *p = Store::AllocBlock((BlockLabel) T_STRING, SIZE);

    p->InitArg(STRING_POS, String::New(s)->ToWord());
    return p;
  }
  static StringNode *FromBlock(Block *x) {
    return (StringNode *) x;
  }
  static StringNode *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_STRING));
    return FromBlock(p);
  }
};

class IdNode : private Block {
private:
  static const u_int SIZE       = 2;
  static const u_int STRING_POS = 1;
  static const u_int VAL_POS    = 2;
public:
  using Block::ToWord;
  
  char *GetString() {
    return ((String *) Store::WordToBlock(GetArg(STRING_POS)))->GetValue();
  }
  word GetValue() {
    return GetArg(VAL_POS);
  }
  void SetValue(word value) {
    ReplaceArg(VAL_POS, value);
  }
  static Block *New(char *s) {
    Block *p = Store::AllocBlock((BlockLabel) T_ID, SIZE);

    p->InitArg(STRING_POS, String::New(s)->ToWord());
    p->InitArg(VAL_POS, Store::IntToWord(0));

    return p;
  }
  static Block *New(char *s, word value) {
    Block *p = Store::AllocBlock((BlockLabel) T_ID, SIZE);

    p->InitArg(STRING_POS, String::New(s)->ToWord());
    p->InitArg(VAL_POS, value);

    return p;
  }
  static IdNode *FromBlock(Block *x) {
    return (IdNode *) x;
  }
  static IdNode *Clone(IdNode *x) {
    Block *lp = (Block *) x;
    Block *p  = Store::AllocBlock((BlockLabel) T_ID, SIZE);

    p->InitArg(STRING_POS, lp->GetArg(STRING_POS));
    p->InitArg(VAL_POS, lp->GetArg(VAL_POS));

    return FromBlock(p);
  }
  static IdNode *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_ID));
    return FromBlock(p);
  }
};

class SelectionNode : private Block {
private:
  static const u_int SIZE     = 2;
  static const u_int THEN_POS = 1;
  static const u_int ELSE_POS = 2;
public:
  using Block::ToWord;

  word GetThen() {
    return GetArg(THEN_POS);
  }
  word GetElse() {
    return GetArg(ELSE_POS);
  }
  
  static Block *New(word thn, word els) {
    Block *p = Store::AllocBlock((BlockLabel) T_SELECTION, SIZE);

    p->InitArg(THEN_POS, thn);
    p->InitArg(ELSE_POS, els);

    return p;
  }
  static SelectionNode *FromBlock(Block *x) {
    return (SelectionNode *) x;
  }
  static SelectionNode *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_SELECTION));
    return FromBlock(p);
  }
};

class IfNode : private Block {
private:
  static const u_int SIZE       = 2;
  static const u_int COND_POS   = 1;
  static const u_int SELECT_POS = 2;
public:
  using Block::ToWord;

  word GetCond() {
    return GetArg(COND_POS);
  }
  word GetSelect() {
    return GetArg(SELECT_POS);
  }
  static Block *New(word cd, word sel) {
    Block *p = Store::AllocBlock((BlockLabel) T_IF, SIZE);

    p->InitArg(COND_POS, cd);
    p->InitArg(SELECT_POS, sel);

    return p;
  }
  static IfNode *FromBlock(Block *x) {
    return (IfNode *) x;
  }
  static IfNode *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_IF));
    return FromBlock(p);
  }
};

class DefineNode : private Block {
private:
  static const u_int SIZE     = 2;
  static const u_int ID_POS   = 1;
  static const u_int EXPR_POS = 2;
public:
  IdNode *GetId() {
    return IdNode::FromWord(GetArg(ID_POS));
  }
  word GetExpr() {
    return GetArg(EXPR_POS);
  }

  static Block *New(word id, word expr) {
    Block *p = Store::AllocBlock((BlockLabel) T_DEFINE, SIZE);

    p->InitArg(ID_POS, id);
    p->InitArg(EXPR_POS, expr);

    return p;
  }
  static DefineNode *FromBlock(Block *x) {
    return (DefineNode *) x;
  }
  static DefineNode *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_DEFINE));
    return FromBlock(p);
  }
};

class LambdaNode : private Block {
private:
  static const u_int SIZE     = 2;
  static const u_int ARG_POS  = 1;
  static const u_int BODY_POS = 2;
public:
  using Block::ToWord;

  word GetArgList() {
    return GetArg(ARG_POS);
  }
  word GetBody() {
    return GetArg(BODY_POS);
  }

  static Block *New(word arg, word body) {
    Block *p = Store::AllocBlock((BlockLabel) T_LAMBDA, SIZE);

    p->InitArg(ARG_POS, arg);
    p->InitArg(BODY_POS, body);

    return p;
  }
  static LambdaNode *FromBlock(Block *x) {
    return (LambdaNode *) x;
  }
  static LambdaNode *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_LAMBDA));
    return FromBlock(p);
  }
};

class ApplicationNode : private Block {
private:
  static const u_int SIZE        = 1;
  static const u_int EXPRARR_POS = 1;
public:
  word GetExprArr() {
    return GetArg(EXPRARR_POS);
  }
  
  static Block *New(word exprarr) {
    Block *p = Store::AllocBlock((BlockLabel) T_APPLICATION, SIZE);

    p->InitArg(EXPRARR_POS, exprarr);

    return p;
  }
  static ApplicationNode *FromBlock(Block *x) {
    return (ApplicationNode *) x;
  }
  static ApplicationNode *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_APPLICATION));
    return FromBlock(p);
  }
};

class ApplyNode : private Block {
private:
  static const u_int SIZE      = 1;
  static const u_int DUMMY_POS = 1;
public:
  static Block *New() {
    Block *p = Store::AllocBlock((BlockLabel) T_APPLY, SIZE);

    p->InitArg(DUMMY_POS, Store::IntToWord(0));

    return p;
  }
  static ApplyNode *FromBlock(Block *x) {
    return (ApplyNode *) x;
  }
  static ApplyNode *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_APPLY));
    return FromBlock(p);
  }
};

class LetNode : private Block {
private:
  static const u_int SIZE     = 3;
  static const u_int ID_POS   = 1;
  static const u_int EXPR_POS = 2;
  static const u_int BODY_POS = 3;
public:
  using Block::ToWord;

  IdNode *GetId() {
    return IdNode::FromWord(GetArg(ID_POS));
  }
  word GetExpr() {
    return GetArg(EXPR_POS);
  }
  word GetBody() {
    return GetArg(BODY_POS);
  }

  static Block *New(word id, word expr, word body) {
    Block *p = Store::AllocBlock((BlockLabel) T_LET, SIZE);

    p->InitArg(ID_POS, id);
    p->InitArg(EXPR_POS, expr);
    p->InitArg(BODY_POS, body);

    return p;
  }
  static LetNode *FromBlock(Block *x) {
    return (LetNode *) x;
  }
  static LetNode *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_LET));
    return FromBlock(p);
  }
};

class ConsCell : private Block {
private:
  static const u_int SIZE    = 2;
  static const u_int CAR_POS = 1;
  static const u_int CDR_POS = 2;
public:
  using Block::ToWord;

  word Car() {
    return GetArg(CAR_POS);
  }
  word Cdr() {
    return GetArg(CDR_POS);
  }
  static ConsCell *FromBlock(Block *x) {
    return (ConsCell *) x;
  }
  static ConsCell *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_CONS));
    return FromBlock(p);
  }
  Block *ToArray(NodeType t) {
    ConsCell *c = this;
    u_int size  = 0;
    Block *p    = INVALID_POINTER;
    
    while (c != INVALID_POINTER) {
      size++;
      c = FromWord(c->Cdr());
    }
    c    = this;
    p    = Store::AllocBlock((BlockLabel) t, size);
    size = 1;

    while (c != INVALID_POINTER) {
      p->InitArg(size++, c->Car());
      c = FromWord(c->Cdr());
    }

    return p;
  }

  static Block *New(word car, word cdr) {
    Block *p = Store::AllocBlock((BlockLabel) T_CONS, SIZE);

    p->InitArg(CAR_POS, car);
    p->InitArg(CDR_POS, cdr);

    return p;
  }
};

class NilNode : private Block {
private:
  static const u_int SIZE      = 1;
  static const u_int DUMMY_POS = 1;
public:
  using Block::ToWord;

  static Block *New() {
    Block *p = Store::AllocBlock((BlockLabel) T_NIL, SIZE);

    p->InitArg(DUMMY_POS, Store::IntToWord(0));

    return p;
  }
  static NilNode *FromBlock(Block *x) {
    return (NilNode *) x;
  }
  static NilNode *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_CONS));
    return FromBlock(p);
  }
};

// Interpreter Helper Nodes

class RemoveNode : private Block {
private:
  static const int SIZE      = 1;
  static const int COUNT_POS = 1;
public:
  using Block::ToWord;

  u_int GetCount() {
    return (u_int) Store::WordToInt(GetArg(COUNT_POS));
  }

  static Block *New(u_int count) {
    Block *p = Store::AllocBlock((BlockLabel) T_REMOVE, SIZE);

    p->InitArg(COUNT_POS, Store::IntToWord((int) count));

    return p;
  }
  static RemoveNode *FromBlock(Block *x) {
    return (RemoveNode *) x;
  }
  static RemoveNode *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_REMOVE));
    return FromBlock(p);
  }
};

class AssignNode : private Block {
private:
  static const u_int SIZE   = 1;
  static const u_int ID_POS = 1;
public:
  using Block::ToWord;

  u_int GetId() {
    return (u_int) Store::WordToInt(GetArg(ID_POS));
  }

  static Block *New(u_int id) {
    Block *p = Store::AllocBlock((BlockLabel) T_ASSIGN, SIZE);

    p->InitArg(ID_POS, Store::IntToWord((int) id));

    return p;
  }
  static AssignNode *FromBlock(Block *x) {
    return (AssignNode *) x;
  }
  static AssignNode *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_ASSIGN));
    return FromBlock(p);
  }
};

class PrimOpNode : private Block {
private:
  static const u_int SIZE   = 1;
  static const u_int ID_POS = 1;
public:
  using Block::ToWord;

  char *GetString() {
    return ((String *) Store::WordToBlock(GetArg(ID_POS)))->GetValue();
  }

  static Block *New(char *id) {
    Block *p = Store::AllocBlock((BlockLabel) T_PRIMOP, SIZE);

    p->InitArg(ID_POS, String::New(id)->ToWord());

    return p;
  }
  static PrimOpNode *FromBlock(Block *x) {
    return (PrimOpNode *) x;
  }
  static PrimOpNode *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_PRIMOP));
    return FromBlock(p);
  }
};

typedef Block * BlockPtr;

#endif __TEST__STORE__SCHEME__NODES_HH__
