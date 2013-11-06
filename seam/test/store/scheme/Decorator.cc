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
#include <cstdio>
#include "Nodes.hh"
#include "Decorator.hh"

// Internal Decorator Member Functions
inline void Decorator::DecorateDeclArr(Block *instr) {
  u_int size = instr->GetSize();

  for (u_int i = 0; i < size; i++) {
    Decorate(instr->GetArg(i));
  }
}

inline void Decorator::DecorateDefine(Block *instr) {
  DefineNode *node = DefineNode::FromBlock(instr);
  IdNode *id       = node->GetId();
  
  id->SetType(T_GLOBAL_VAR);
  id->SetIndex(GlobalAlloc(id->GetName()));
  Decorate(node->GetExpr());
}

inline void Decorator::DecorateIf(Block *instr) {
  IfNode *node = IfNode::FromBlock(instr);

  Decorate(node->GetCond());
  SelectionNode *sel = SelectionNode::FromWord(node->GetSelect());
  Decorate(sel->GetThen());
  Decorate(sel->GetElse());
}

inline void Decorator::DecorateLambda(Block *instr) {
  LambdaNode *node = LambdaNode::FromBlock(instr);
  Block *arr       = Store::DirectWordToBlock(node->GetArgList());
  word list        = Store::IntToWord(0);

  for (int i = arr->GetSize(); i--;) {
    list = ConsCell::New(arr->GetArg(i), list)->ToWord();
  }
  top++;
  closures[top]    = list;
  semi_locals[top] = Store::IntToWord(0);
  Decorate(node->GetBody());

  word l    = closures[top];
  u_int len = Length(l);
  node->SetFrameSize(len);
  for (u_int i = len; i--;) {
    ConsCell *cell = ConsCell::FromWord(l);
    IdNode *node   = IdNode::FromWord(cell->Car());

    node->SetType(T_LOCAL_VAR);
    node->SetIndex(i);
    l = cell->Cdr();
  }
  node->SetEnv(semi_locals[top]);
  node->SetEnvSize(Length(semi_locals[top]));
  top--;
}

inline void Decorator::DecorateId(Block *instr) {
  IdNode *node = IdNode::FromBlock(instr);
  VarType type;
  u_int id;

  SearchId(node->GetName(), &type, &id);
  node->SetType(type);
  node->SetIndex(id);
}

inline void Decorator::DecorateLet(Block *instr) {
  LetNode *node = LetNode::FromBlock(instr);

  Decorate(node->GetExpr());
  closures[top] = ConsCell::New(node->GetId()->ToWord(), closures[top])->ToWord();
  Decorate(node->GetBody());
}

inline void Decorator::DecorateApplication(Block *instr) {
  Block *arr = Store::DirectWordToBlock(ApplicationNode::FromBlock(instr)->GetExprArr());

  for (int i = arr->GetSize(); i--;) {
    Decorate(arr->GetArg(i));
  }
}

inline void Decorator::DecorateBegin(Block *instr) {
  Block *arr = Store::DirectWordToBlock(BeginNode::FromBlock(instr)->GetExprArr());

  for (int i = arr->GetSize(); i--;) {
    Decorate(arr->GetArg(i));
  }
}

inline void Decorator::DecorateSetQ(Block *instr) {
  SetQNode *node = SetQNode::FromBlock(instr);

  Decorate(node->GetId()->ToWord());
  Decorate(node->GetExpr());
}

inline void Decorator::DecorateSetCxr(Block *instr) {
  SetCxrNode *node = SetCxrNode::FromBlock(instr);

  Decorate(node->GetCell());
  Decorate(node->GetExpr());
}

// Public Decorator Methods
void Decorator::Decorate(word tree) {
  Block *instr = Store::DirectWordToBlock(tree);

  switch ((NodeType) instr->GetLabel()) {
  case T_DECLARR:
    DecorateDeclArr(instr); break;
  case T_DEFINE:
    DecorateDefine(instr); break;
  case T_IF:
    DecorateIf(instr); break;
  case T_INT:
  case T_STRING:
  case T_NIL:
    break;
  case T_LAMBDA:
    DecorateLambda(instr); break;
  case T_ID:
    DecorateId(instr); break;
  case T_LET:
    DecorateLet(instr); break;
  case T_APPLICATION:
    DecorateApplication(instr); break;
  case T_BEGIN:
    DecorateBegin(instr); break;
  case T_SETQ:
    DecorateSetQ(instr); break;
  default:
    break;
  }
}
