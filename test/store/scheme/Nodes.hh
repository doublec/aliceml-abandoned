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

// External Helper Functions
extern u_int RegisterAtom(const char *s);
extern char *AtomToString(u_int name);
extern u_int GlobalAlloc(u_int name);
extern int SearchGlobal(u_int name);

// Bison Node Types
typedef enum {
  T_ENLARGEARR,
  T_ATOMDICT,
  T_GLBENV,
  T_FRAME,
  T_CLOSURE,
  T_CLOSUREARR,
  T_INT,
  T_STRING,
  T_ID,
  T_IF,
  T_DEFINE,
  T_LAMBDA,
  T_APPLICATION,
  T_APPLY,
  T_TAILAPPLY,
  T_LET,
  T_DECLARR,
  T_EXPRARR,
  T_IDARR,
  T_CONS,
  T_NIL,
  T_SELECTION,
  T_REMOVE,
  T_ASSIGN,
  T_PRIMOP,
  T_BEGIN,
  T_TIME,
  T_SETQ,
  T_SETCAR,
  T_SETCDR
} NodeType;

// Variable Types
typedef enum {
  T_GLOBAL_VAR,
  T_LOCAL_VAR,
  T_SEMI_LOCAL_VAR
} VarType;

// Interpreter Primitive Operations
typedef enum {
  OP_PLUS,
  OP_MINUS,
  OP_MUL,
  OP_DIV,
  OP_LESS,
  OP_GREATER,
  OP_EQUAL,
  OP_EQ,
  OP_USE,
  OP_CONS,
  OP_CAR,
  OP_CDR,
  OP_SHOW,
  OP_TIME,
  OP_GC,
  OP_GENGC,
  OP_SETGC,
  OP_SHOWLIST,
  OP_EXIT,
  OP_MEMSTAT,
  OP_SKIP,
  OP_SETCAR,
  OP_SETCDR,
  OP_KILLTOP
} PrimType;

// Global Helper Nodes
class EnlargableArray : public Block {
private:
  static const u_int SIZE    = 2;
  static const u_int ARR_POS = 0;
  static const u_int TOP_POS = 1;
protected:
  Block *Enlarge(Block *sarr) {
    u_int ssize = sarr->GetSize();
    u_int nsize = ((ssize * 3) >> 1);
    Block *narr = Store::AllocBlock((BlockLabel) T_ENLARGEARR, nsize);
    
    for (u_int i = ssize; i--;) {
      narr->InitArg(i, sarr->GetArg(i));
    }
    return narr;
  }
public:
  Block *GetArray() {
    return Store::DirectWordToBlock(GetArg(ARR_POS));
  }
  void SetArray(Block *arr) {
    ReplaceArg(ARR_POS, arr->ToWord());
  }
  u_int GetTop() {
    return Store::DirectWordToInt(GetArg(TOP_POS));
  }
  void SetTop(u_int top) {
    InitArg(TOP_POS, top);
  }
  u_int AllocSlot() {
    Block *arr = GetArray();
    u_int top  = GetTop();

    if (top >= arr->GetSize()) {
      arr = Enlarge(arr);
      SetArray(arr);
    }
    SetTop((top + 1));
    return top;
  }
  word Sub(u_int i) {
    return GetArray()->GetArg(i);
  }
  void Update(u_int i, word value) {
    return GetArray()->ReplaceArg(i, value);
  }

  static EnlargableArray *New(BlockLabel t, u_int size) {
    Block *p = Store::AllocBlock(t, SIZE);

    p->InitArg(ARR_POS, Store::AllocBlock((BlockLabel) T_ENLARGEARR, size)->ToWord());
    p->InitArg(TOP_POS, 0);
    return (EnlargableArray *) p;
  }
  static EnlargableArray *FromBlock(Block *x) {
    return (EnlargableArray *) x;
  }
  static EnlargableArray *FromWord(word x) {
    return FromBlock(Store::DirectWordToBlock(x));
  }
};

// Bison Tree Nodes
class IntNode : private Block {
private:
  static const u_int SIZE    = 1;
  static const u_int INT_POS = 0;
public:
  using Block::ToWord;

  int GetInt() {
    return Store::DirectWordToInt(GetArg(INT_POS));
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
    Block *p = Store::DirectWordToBlock(x);

    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_INT));
    return FromBlock(p);
  }
};

class StringCoreNode : private Chunk {
public:
  using Chunk::ToWord;

  char *GetString() { return GetBase(); }
  u_int Length()    { return GetSize(); }

  static StringCoreNode *New(const char *str) {
    int len  = (strlen(str) + 1);
    Chunk *b = Store::AllocChunk(len);
    char *ar = b->GetBase();

    std::memcpy(ar, str, len);
    return (StringCoreNode *) b;
  }
  static StringCoreNode *FromBlock(Block *x) {
    return (StringCoreNode *) x;
  }
  static StringCoreNode *FromWord(word x) {
    return FromBlock((Block *) Store::DirectWordToChunk(x));
  }
};

class StringNode : private Block {
private:
  static const u_int SIZE       = 1;
  static const u_int STRING_POS = 0;
public:
  using Block::ToWord;

  char *GetString() { return StringCoreNode::FromWord(GetArg(STRING_POS))->GetString(); }
  u_int Length()    { return StringCoreNode::FromWord(GetArg(STRING_POS))->Length(); }

  static StringNode *New(const char *str) {
    Block *p = Store::AllocBlock((BlockLabel) T_STRING, SIZE);

    p->InitArg(STRING_POS, StringCoreNode::New(str)->ToWord());
    return (StringNode *) p;
  }
  static StringNode *FromBlock(Block *x) {
    return (StringNode *) x;
  }
  static StringNode *FromWord(word x) {
    Block *p = Store::DirectWordToBlock(x);

    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_STRING));
    return FromBlock(p);
  }
};

class IdNode : private Block {
private:
  static const u_int SIZE      = 4;
  static const u_int NAME_POS  = 0;
  static const u_int TYPE_POS  = 1;
  static const u_int INDEX_POS = 2;
  static const u_int FRAME_POS = 3;
public:
  using Block::ToWord;
  
  u_int GetName() {
    return Store::DirectWordToInt(GetArg(NAME_POS));
  }
  VarType GetType() {
    return (VarType) Store::DirectWordToInt(GetArg(TYPE_POS));
  }
  void SetType(VarType type) {
    InitArg(TYPE_POS, Store::IntToWord(type));
  }
  u_int GetIndex() {
    return Store::DirectWordToInt(GetArg(INDEX_POS));
  }
  void SetIndex(u_int index) {
    InitArg(INDEX_POS, Store::IntToWord(index));
  }
  u_int GetFrame() {
    return Store::DirectWordToInt(GetArg(FRAME_POS));
  }
  void SetFrame(u_int pos) {
    InitArg(FRAME_POS, pos);
  }
  IdNode *Clone() {
    Block *p = Store::AllocBlock((BlockLabel) T_ID, SIZE);
    
    p->InitArg(NAME_POS, GetArg(NAME_POS));
    return (IdNode *) p;
  }

  static Block *New(const char *s) {
    Block *p = Store::AllocBlock((BlockLabel) T_ID, SIZE);

    p->InitArg(NAME_POS, RegisterAtom(s));
    p->InitArg(TYPE_POS, 0);
    p->InitArg(INDEX_POS, 0);
    return p;
  }
  static Block *New(u_int name) {
    Block *p = Store::AllocBlock((BlockLabel) T_ID, SIZE);

    p->InitArg(NAME_POS, name);
    return p;
  }
  static Block *New(u_int name, u_int index, int dist) {
    Block *p = Store::AllocBlock((BlockLabel) T_ID, SIZE);

    p->InitArg(NAME_POS, name);
    p->InitArg(INDEX_POS, index);
    p->InitArg(FRAME_POS, dist);
    return p;
  }

  static IdNode *FromBlock(Block *x) {
    return (IdNode *) x;
  }
  static IdNode *FromWord(word x) {
    Block *p = Store::DirectWordToBlock(x);

    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_ID));
    return FromBlock(p);
  }
};

class SelectionNode : private Block {
private:
  static const u_int SIZE     = 2;
  static const u_int THEN_POS = 0;
  static const u_int ELSE_POS = 1;
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
    Block *p = Store::DirectWordToBlock(x);

    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_SELECTION));
    return FromBlock(p);
  }
};

class IfNode : private Block {
private:
  static const u_int SIZE       = 2;
  static const u_int COND_POS   = 0;
  static const u_int SELECT_POS = 1;
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
    Block *p = Store::DirectWordToBlock(x);

    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_IF));
    return FromBlock(p);
  }
};

class DefineNode : private Block {
private:
  static const u_int SIZE     = 2;
  static const u_int ID_POS   = 0;
  static const u_int EXPR_POS = 1;
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
    Block *p = Store::DirectWordToBlock(x);

    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_DEFINE));
    return FromBlock(p);
  }
};

class ClosureNode : private Block {
private:
  static const u_int SIZE        = 4;
  static const u_int ARG_POS     = 0;
  static const u_int BODY_POS    = 1;
  static const u_int FRAME_POS   = 2;
  static const u_int CLOSURE_POS = 3;
public:
  using Block::ToWord;

  word GetArgList() {
    return GetArg(ARG_POS);
  }
  word GetBody() {
    return GetArg(BODY_POS);
  }
  u_int GetFrameSize() {
    return Store::DirectWordToInt(GetArg(FRAME_POS));
  }
  word GetEnv() {
    return GetArg(CLOSURE_POS);
  }

  static Block *New(word arg, word body, u_int frame, word closure) {
    Block *p = Store::AllocBlock((BlockLabel) T_CLOSURE, SIZE);

    p->ReplaceArg(ARG_POS, arg);
    p->ReplaceArg(BODY_POS, body);
    p->InitArg(FRAME_POS, frame);
    p->InitArg(CLOSURE_POS, closure);
    return p;
  }
  static ClosureNode *FromBlock(Block *x) {
    return (ClosureNode *) x;
  }
  static ClosureNode *FromWord(word x) {
    Block *p = Store::DirectWordToBlock(x);

    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_CLOSURE));
    return FromBlock(p);
  }
};

class LambdaNode : private Block {
private:
  static const u_int SIZE        = 5;
  static const u_int ARG_POS     = 0;
  static const u_int BODY_POS    = 1;
  static const u_int FRAME_POS   = 2;
  static const u_int ENV_POS     = 3;
  static const u_int ENV_LEN_POS = 4;
public:
  using Block::ToWord;

  word GetArgList() {
    return GetArg(ARG_POS);
  }
  word GetBody() {
    return GetArg(BODY_POS);
  }
  u_int GetFrameSize() {
    return Store::DirectWordToInt(GetArg(FRAME_POS));
  }
  void SetFrameSize(u_int size) {
    InitArg(FRAME_POS, size);
  }
  word GetEnv() {
    return GetArg(ENV_POS);
  }
  void SetEnv(word env) {
    ReplaceArg(ENV_POS, env);
  }
  u_int GetEnvSize() {
    return Store::DirectWordToInt(GetArg(ENV_LEN_POS));
  }
  void SetEnvSize(u_int size) {
    InitArg(ENV_LEN_POS, size);
  }
  Block *MakeClosure(Block *arr) {
    return ClosureNode::New(GetArgList(), GetBody(), GetFrameSize(), arr->ToWord());
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
    Block *p = Store::DirectWordToBlock(x);

    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_LAMBDA));
    return FromBlock(p);
  }
};

class ApplicationNode : private Block {
private:
  static const u_int SIZE        = 2;
  static const u_int EXPRARR_POS = 0;
  static const u_int TAIL_POS    = 1;
public:
  word GetExprArr() {
    return GetArg(EXPRARR_POS);
  }
  u_int IsTail() {
    return Store::DirectWordToInt(GetArg(TAIL_POS));
  }

  static Block *New(word exprarr, u_int tail) {
    Block *p = Store::AllocBlock((BlockLabel) T_APPLICATION, SIZE);

    p->InitArg(EXPRARR_POS, exprarr);
    p->InitArg(TAIL_POS, tail);
    return p;
  }
  static Block *FromLambda(word lamba) {
    Block *arr = Store::AllocBlock((BlockLabel) T_EXPRARR, 1);

    arr->ReplaceArg(0, lamba);
    return New(arr->ToWord(), 0);
  }
  static ApplicationNode *FromBlock(Block *x) {
    return (ApplicationNode *) x;
  }
  static ApplicationNode *FromWord(word x) {
    Block *p = Store::DirectWordToBlock(x);

    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_APPLICATION));
    return FromBlock(p);
  }
};

class LetNode : private Block {
private:
  static const u_int SIZE     = 3;
  static const u_int ID_POS   = 0;
  static const u_int EXPR_POS = 1;
  static const u_int BODY_POS = 2;
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
    Block *p = Store::DirectWordToBlock(x);

    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_LET));
    return FromBlock(p);
  }
};

class ConsCell : private Block {
private:
  static const u_int SIZE    = 2;
  static const u_int CAR_POS = 0;
  static const u_int CDR_POS = 1;
public:
  using Block::ToWord;

  word Car() {
    return GetArg(CAR_POS);
  }
  word Cdr() {
    return GetArg(CDR_POS);
  }
  void SetCar(word car) {
    ReplaceArg(CAR_POS, car);
  }
  void SetCdr(word cdr) {
    ReplaceArg(CDR_POS, cdr);
  }
  static ConsCell *FromBlock(Block *x) {
    return (ConsCell *) x;
  }
  static ConsCell *FromWord(word x) {
    Block *p = Store::DirectWordToBlock(x);

    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_CONS));
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
    size = 0;

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
  static const u_int DUMMY_POS = 0;
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
    Block *p = Store::DirectWordToBlock(x);

    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_CONS));
    return FromBlock(p);
  }
};

// Interpreter Helper Nodes
class AssignNode : private Block {
private:
  static const u_int SIZE   = 1;
  static const u_int ID_POS = 0;
public:
  using Block::ToWord;

  IdNode *GetId() {
    return IdNode::FromWord(GetArg(ID_POS));
  }

  static Block *New(IdNode *id) {
    Block *p = Store::AllocBlock((BlockLabel) T_ASSIGN, SIZE);

    p->ReplaceArg(ID_POS, id->ToWord());
    return p;
  }
  static AssignNode *FromBlock(Block *x) {
    return (AssignNode *) x;
  }
  static AssignNode *FromWord(word x) {
    Block *p = Store::DirectWordToBlock(x);

    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_ASSIGN));
    return FromBlock(p);
  }
};

class PrimOpNode : private Block {
private:
  static const u_int SIZE     = 1;
  static const u_int TYPE_POS = 0;
public:
  using Block::ToWord;

  PrimType GetType() {
    return (PrimType) Store::DirectWordToInt(GetArg(TYPE_POS));
  }

  static Block *New(PrimType type) {
    Block *p = Store::AllocBlock((BlockLabel) T_PRIMOP, SIZE);

    p->InitArg(TYPE_POS, static_cast<u_int>(type));
    return p;
  }
  static PrimOpNode *FromBlock(Block *x) {
    return (PrimOpNode *) x;
  }
  static PrimOpNode *FromWord(word x) {
    Block *p = Store::DirectWordToBlock(x);

    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_PRIMOP));
    return FromBlock(p);
  }
};

class BeginNode : private Block {
private:
  static const u_int SIZE        = 1;
  static const u_int EXPRARR_POS = 0;
public:
  using Block::ToWord;

  word GetExprArr() {
    return GetArg(EXPRARR_POS);
  }

  static Block *New(word exprarr) {
    Block *p = Store::AllocBlock((BlockLabel) T_BEGIN, SIZE);

    p->InitArg(EXPRARR_POS, exprarr);
    return p;
  }
  static BeginNode *FromBlock(Block *x) {
    return (BeginNode *) x;
  }
  static BeginNode *FromWord(word x) {
    Block *p = Store::DirectWordToBlock(x);

    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_BEGIN));
    return FromBlock(p);
  }
};

class TimeNode : private Block {
private:
  static const u_int SIZE = 0;
public:
  using Block::ToWord;

  static Block *New() {
    return Store::AllocBlock((BlockLabel) T_TIME, SIZE);
  }
  static TimeNode *FromBlock(Block *x) {
    return (TimeNode *) x;
  }
  static TimeNode *FromWord(word x) {
    Block *p = Store::DirectWordToBlock(x);

    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_TIME));
    return FromBlock(p);
  }
};

class SetQNode : private Block {
private:
  static const u_int SIZE     = 2;
  static const u_int ID_POS   = 0;
  static const u_int EXPR_POS = 1;
public:
  using Block::ToWord;

  IdNode *GetId() {
    return IdNode::FromWord(GetArg(ID_POS));
  }
  word GetExpr() {
    return GetArg(EXPR_POS);
  }

  static Block *New(word id, word expr) {
    Block *p = Store::AllocBlock((BlockLabel) T_SETQ, SIZE);

    p->InitArg(ID_POS, id);
    p->InitArg(EXPR_POS, expr);
    return p;
  }
  static SetQNode *FromBlock(Block *x) {
    return (SetQNode *) x;
  }
  static SetQNode *FromWord(word x) {
    Block *p = Store::DirectWordToBlock(x);

    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_SETQ));
    return FromBlock(p);
  }
};

class SetCxrNode : private Block {
private:
  static const u_int SIZE     = 2;
  static const u_int CELL_POS = 0;
  static const u_int EXPR_POS = 1;
public:
  using Block::ToWord;

  word GetCell() {
    return GetArg(CELL_POS);
  }
  word GetExpr() {
    return GetArg(EXPR_POS);
  }

  static Block *New(NodeType type, word id, word expr) {
    Block *p = Store::AllocBlock((BlockLabel) type, SIZE);

    p->InitArg(CELL_POS, id);
    p->InitArg(EXPR_POS, expr);
    return p;
  }
  static SetCxrNode *FromBlock(Block *x) {
    return (SetCxrNode *) x;
  }
  static SetCxrNode *FromWord(word x) {
    Block *p = Store::DirectWordToBlock(x);

    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_SETCAR));
    return FromBlock(p);
  }
};

typedef Block * BlockPtr;

// Atom Dictionary
class AtomDictionary : private EnlargableArray {
public:
  using Block::ToWord;

  u_int FromString(const char *s) {
    Block *arr = GetArray();

    for (int i = GetTop(); i--;) {
      if (!std::strcmp(s, StringNode::FromWord(arr->GetArg(i))->GetString())) {
	return i;
      }
    }
    u_int pos = AllocSlot();
    arr->ReplaceArg(pos, StringNode::New(s)->ToWord());
    return pos;
  }
  char *ToString(u_int a) {
    if (a < GetTop()) {
      return StringNode::FromWord(GetArray()->GetArg(a))->GetString();
    }
    return INVALID_POINTER;
  }

  static Block *New(int i) {
    return (Block *) EnlargableArray::New((BlockLabel) T_ATOMDICT, i);
  }
  static AtomDictionary *FromBlock(Block *x) {
    return (AtomDictionary *) x;
  }
  static AtomDictionary *FromWord(word x) {
    Block *p = Store::DirectWordToBlock(x);

    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) T_ATOMDICT));
    return FromBlock(p);
  }
};

#endif __TEST__STORE__SCHEME__NODES_HH__
