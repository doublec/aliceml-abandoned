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
#include "Interpreter.hh"

// Internal Class Variables

word Interpreter::root;

// Internal Helper Methods

int Interpreter::HaveTask() {
  return !Stack::FromWord(Store::WordToBlock(root)->GetArg(TASK_STACK_POS))->IsEmpty();
}

void Interpreter::PushTask(word task) {
  Stack::FromWord(Store::WordToBlock(root)->GetArg(TASK_STACK_POS))->SlowPush(task);
}

word Interpreter::PopTask() {
  return Stack::FromWord(Store::WordToBlock(root)->GetArg(TASK_STACK_POS))->Pop();
}

u_int Interpreter::PushId(IdNode *id) {
  IdNode *idc = IdNode::Clone(id);
  word top;

  Stack::FromWord(Store::WordToBlock(root)->GetArg(ENV_STACK_POS))->SlowPush(idc->ToWord());
  top = ((Block *) Stack::FromWord(Store::WordToBlock(root)->GetArg(ENV_STACK_POS)))->GetArg(1);

  return (u_int) (Store::WordToInt(top) - 1);
}

void Interpreter::PopId() {
  Stack::FromWord(Store::WordToBlock(root)->GetArg(ENV_STACK_POS))->Pop();
}

void Interpreter::AssignId(u_int id) {
  Block *p   = (Block *) Stack::FromWord(Store::WordToBlock(root)->GetArg(ENV_STACK_POS));
  Block *arr = Store::WordToBlock(p->GetArg(2));

  IdNode::FromWord(arr->GetArg(id))->SetValue(PopValue());
}

word Interpreter::LookUp(char *s) {
  Block *p   = (Block *) Stack::FromWord(Store::WordToBlock(root)->GetArg(ENV_STACK_POS));
  Block *arr = Store::WordToBlock(p->GetArg(2));
  u_int size = (u_int) (Store::WordToInt(p->GetArg(1)) - 1);

  for (u_int i = size; i >= 1; i--) {
    IdNode *id = IdNode::FromWord(arr->GetArg(i));

    if (!strcmp(id->GetString(), s)) {
      return id->GetValue();
    }
  }
  fprintf(stderr, "Interpreter::LookUp: unable to find `%s'\n", s);
  exit(0);

  return Store::IntToWord(0);
}

void Interpreter::PushValue(word value) {
  Stack::FromWord(Store::WordToBlock(root)->GetArg(EVAL_STACK_POS))->SlowPush(value);
}

word Interpreter::PopValue() {
  return Stack::FromWord(Store::WordToBlock(root)->GetArg(EVAL_STACK_POS))->Pop();
}

static IdNode *CreateId(const char *s) {
  word op = PrimOpNode::New(s)->ToWord();
  return IdNode::FromBlock(IdNode::New(s, op));
}

static int BothInt(word a, word b, int & ai, int & bi) {
  IntNode *an = IntNode::FromWord(a);
  IntNode *bn = IntNode::FromWord(b);

  if ((an != INVALID_POINTER) && (bn != INVALID_POINTER)) {
    ai = an->GetInt();
    bi = bn->GetInt();

    return 1;
  }
  return 0;
}

static int BothNil(word a, word b) {
  Block *an = Store::WordToBlock(a);
  Block *bn = Store::WordToBlock(b);

  if ((an != INVALID_POINTER) && (bn != INVALID_POINTER)) {
    return ((an->GetLabel() == (BlockLabel) T_NIL) &&
	    (bn->GetLabel() == (BlockLabel) T_NIL));
  }
  else {
    return 0;
  }
}

void Interpreter::CreateEnvironment() {
  PushId(CreateId("+"));
  PushId(CreateId("-"));
  PushId(CreateId("*"));
  PushId(CreateId("/"));
  PushId(CreateId("<"));
  PushId(CreateId(">"));
  PushId(CreateId("="));
  PushId(CreateId("eq?"));
  PushId(CreateId("show"));
  PushId(CreateId("exit"));
  PushId(CreateId("use"));
  PushId(CreateId("cons"));
  PushId(CreateId("car"));
  PushId(CreateId("cdr"));
  PushId(CreateId("gc"));
}

// Public Methods

void Interpreter::Init() {
  Block *p = Store::AllocBlock(MIN_DATA_LABEL, ROOT_SIZE);

  p->InitArg(TASK_STACK_POS, Stack::New(STACK_SIZE)->ToWord());
  p->InitArg(EVAL_STACK_POS, Stack::New(STACK_SIZE)->ToWord());
  p->InitArg(ENV_STACK_POS, Stack::New(STACK_SIZE)->ToWord());

  root = p->ToWord();

  CreateEnvironment();
}

char *Interpreter::Interpret(word tree) {
  PushTask(tree);
  
  while (HaveTask()) {
    Block *instr = Store::WordToBlock(PopTask());

    switch (instr->GetLabel()) {
    case T_DECLARR: {
      u_int size = instr->GetSize();
      
      for (u_int i = size; i >= 1; i--) {
	PushTask(instr->GetArg(i));
      }
      if (Store::NeedGC()) {
	//root = Store::DoGC(root);
      }
      break;
    }
    case T_DEFINE: {
      DefineNode *lp = DefineNode::FromBlock(instr);
      
      PushTask(AssignNode::New(PushId(lp->GetId()))->ToWord());
      PushTask(lp->GetExpr());
      break;
    }
    case T_ASSIGN: {
      AssignNode *lp = AssignNode::FromBlock(instr);

      AssignId(lp->GetId());
      break;
    }
    case T_REMOVE: {
      RemoveNode *lp = RemoveNode::FromBlock(instr);

      for (u_int i = lp->GetCount(); i > 0; i--) {
	PopId();
      }
      break;
    }
    case T_IF: {
      IfNode *lp = IfNode::FromBlock(instr);
      
      PushTask(lp->GetSelect());
      PushTask(lp->GetCond());
      break;
    }
    case T_SELECTION: {
      SelectionNode *lp = SelectionNode::FromBlock(instr);

      if (IntNode::FromWord(PopValue())->GetInt()) {
	PushTask(lp->GetThen());
      }
      else {
	PushTask(lp->GetElse());
      }
      break;
    }
    case T_INT:
    case T_STRING:
    case T_LAMBDA:
    case T_PRIMOP:
    case T_NIL:
      PushValue(instr->ToWord());
      break;
    case T_ID: {
      IdNode *lp = IdNode::FromBlock(instr);

      PushValue(LookUp(lp->GetString()));
      break;
    }
    case T_LET: {
      LetNode *lp = LetNode::FromBlock(instr);
      u_int id    = PushId(lp->GetId());

      PushTask(RemoveNode::New(1)->ToWord());
      PushTask(lp->GetBody());
      PushTask(AssignNode::New(id)->ToWord());
      PushTask(lp->GetExpr());
      break;
    }
    case T_APPLICATION: {
      ApplicationNode *lp = ApplicationNode::FromBlock(instr);
      Block *arr          = Store::WordToBlock(lp->GetExprArr());
      u_int size          = arr->GetSize();

      PushTask(ApplyNode::New()->ToWord());
      
      for (u_int i = 1; i <= size; i++) {
	PushTask(arr->GetArg(i));
      }
      break;
    }
    case T_APPLY: {
      word value = PopValue(); 
      Block *p   = Store::WordToBlock(value);

      if (p->GetLabel() == (BlockLabel) T_LAMBDA) {
	LambdaNode *abs = LambdaNode::FromBlock(p);
	Block *arr      = Store::WordToBlock(abs->GetArgList());
	u_int size      = arr->GetSize();
	
	PushTask(RemoveNode::New((size))->ToWord());
	PushTask(abs->GetBody());
	
	for (u_int i = size; i >= 1; i--) {
	  u_int id = PushId(IdNode::FromWord(arr->GetArg(i)));
	  
	  PushTask(AssignNode::New(id)->ToWord());
	}
      }
      else {
	char *op = PrimOpNode::FromBlock(p)->GetString();

	if (!std::strcmp(op, "+")) {
	  int a, b;

	  if (BothInt(PopValue(), PopValue(), b, a)) {
	    PushValue(IntNode::New(a + b)->ToWord());
	  }
	  else {
	    fprintf(stderr, "Interpret::Interpret: evaluation of `+' needs int arguments\n");
	    exit(0);
	  }
	}
	else if (!std::strcmp(op, "-")) {
	  int a, b;

	  if (BothInt(PopValue(), PopValue(), b, a)) {
	    PushValue(IntNode::New(a - b)->ToWord());
	  }
	  else {
	    fprintf(stderr, "Interpret::Interpret: evaluation of `-' needs int arguments\n");
	    exit(0);
	  }
	}
	else if (!std::strcmp(op, "*")) {
	  int a, b;

	  if (BothInt(PopValue(), PopValue(), b, a)) {
	    PushValue(IntNode::New(a * b)->ToWord());
	  }
	  else {
	    fprintf(stderr, "Interpret::Interpret: evaluation of `*' needs int arguments\n");
	    exit(0);
	  }
	}
	else if (!std::strcmp(op, "/")) {
	  int a, b;

	  if (BothInt(PopValue(), PopValue(), b, a)) {
	    PushValue(IntNode::New(a / b)->ToWord());
	  }
	  else {
	    fprintf(stderr, "Interpret::Interpret: evaluation of `/' needs int arguments\n");
	    exit(0);
	  }
	}
	else if (!std::strcmp(op, "<")) {
	  int a, b;

	  if (BothInt(PopValue(), PopValue(), b, a)) {
	    PushValue(IntNode::New(a < b)->ToWord());
	  }
	  else {
	    fprintf(stderr, "Interpret::Interpret: evaluation of `<' needs int arguments\n");
	    exit(0);
	  }
	}
	else if (!std::strcmp(op, ">")) {
	  int a, b;

	  if (BothInt(PopValue(), PopValue(), b, a)) {
	    PushValue(IntNode::New(a > b)->ToWord());
	  }
	  else {
	    fprintf(stderr, "Interpret::Interpret: evaluation of `>' needs int arguments\n");
	    exit(0);
	  }
	}
	else if (!std::strcmp(op, "=")) {
	  int a, b;

	  if (BothInt(PopValue(), PopValue(), b, a)) {
	    PushValue(IntNode::New(a == b)->ToWord());
	  }
	  else {
	    PushValue(IntNode::New(0)->ToWord());
	  }
	}
	else if (!std::strcmp(op, "eq?")) {
	  word a = PopValue();
	  word b = PopValue();

	  if (BothNil(a, b)) {
	    PushValue(IntNode::New(1)->ToWord());
	  }
	  else {
	    PushValue(IntNode::New(a == b)->ToWord());
	  }
	}
	else if (!std::strcmp(op, "use")) {
	  StringNode *a = StringNode::FromWord(PopValue());

	  if (a != INVALID_POINTER) {
	    return a->GetString();
	  }
	  else {
	    fprintf(stderr, "Interpret::Interpret: evaluation of `use' needs string argument\n");
	    exit(0);
	  }
	}
	else if (!std::strcmp(op, "cons")) {
	  word a = PopValue();
	  word b = PopValue();

	  PushValue(ConsCell::New(a, b)->ToWord());
	}
	else if (!std::strcmp(op, "car")) {
	  ConsCell *c = ConsCell::FromWord(PopValue());

	  PushValue(c->Car());
	}
	else if (!std::strcmp(op, "cdr")) {
	  ConsCell *c = ConsCell::FromWord(PopValue());

	  PushValue(c->Cdr());
	}
	else if (!std::strcmp(op, "show")) {
	  Block *p = Store::WordToBlock(PopValue());

	  switch ((NodeType) p->GetLabel()) {
	  case T_INT:
	    printf("%d\n", IntNode::FromBlock(p)->GetInt());
	    break;
	  case T_STRING:
	    printf("%s\n", StringNode::FromBlock(p)->GetString());
	    break;
	  default:
	    break;
	  }
	}
	else if (!std::strcmp(op, "gc")) {
#if defined(DEBUG_CHECK)
	  int gen = IntNode::FromWord(PopValue())->GetInt();

	  Store::ForceGCGen(gen);
	  root = Store::DoGC(root);
#else
	  PopValue();
	  if (Store::NeedGC()) {
	    // root = Store::DoGC(root);
	  }
#endif
	}
	else if (!std::strcmp(op, "exit")) {
	  exit(0);
	}
      }
      
      if (Store::NeedGC()) {
	// root = Store::DoGC(root);
      }
      break;
    }
    default:
      break;
    }
  }

  return NULL;
}
