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
#include <sys/time.h>
#include <unistd.h>
#include "Nodes.hh"
#include "Decorator.hh"
#include "Interpreter.hh"

// Internal Global Variables
static struct timeval start_t, end_t;

// Internal Functions
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
  Block *an = Store::DirectWordToBlock(a);
  Block *bn = Store::DirectWordToBlock(b);

  if ((an != INVALID_POINTER) && (bn != INVALID_POINTER)) {
    return ((an->GetLabel() == (BlockLabel) T_NIL) &&
	    (bn->GetLabel() == (BlockLabel) T_NIL));
  }
  else {
    return 0;
  }
}

static u_int StackSize(word s) {
  return Store::DirectWordToBlock(Store::DirectWordToBlock(s)->GetArg(1))->GetSize();
}

static void ShowList(Block *p) {
  u_int ende = 0;
  
  std::printf(" (");
  while (!ende) {
    if (p->GetLabel() == (BlockLabel) T_NIL) {
      std::printf(")");
      ende = 1;
    }
    else {
      ConsCell *c = ConsCell::FromBlock(p);
      Block *car  = Store::DirectWordToBlock(c->Car());
      
      switch ((NodeType) car->GetLabel()) {
      case T_INT:
	std::printf(" %d", IntNode::FromBlock(car)->GetInt());
	break;
      case T_STRING:
	std::printf(" %s", StringNode::FromBlock(car)->GetString());
	break;
      case T_CONS:
	ShowList(car);
	break;
      case T_NIL:
	std::printf(" ()");
	break;
      default:
	break;
      }
      p = Store::DirectWordToBlock(c->Cdr());
    }
  }
}

// Internal Class Variables
word Interpreter::root;
u_int Interpreter::allowGC;

// Internal Class Methods
inline word Interpreter::GetRoot(u_int pos) {
  return Store::DirectWordToBlock(root)->GetArg(pos);
}

// Handle Interpreter Tasks
inline int Interpreter::HaveTask() {
  return !Stack::FromWordDirect(GetRoot(TASK_STACK))->IsEmpty();
}

inline void Interpreter::PushTask(word task) {
  Stack::FromWordDirect(GetRoot(TASK_STACK))->SlowPush(task);
}

inline word Interpreter::PopTask() {
  return Stack::FromWordDirect(GetRoot(TASK_STACK))->SlowPop();
}

// Push and Pop from EVAL_STACK
inline void Interpreter::PushValue(word value) {
  Stack::FromWordDirect(GetRoot(EVAL_STACK))->SlowPush(value);
}

inline word Interpreter::PopValue() {
  return Stack::FromWordDirect(GetRoot(EVAL_STACK))->SlowPop();
}

// Push and Pop Frame
inline void Interpreter::PushFrame(u_int size) {
  Stack *s    = Stack::FromWordDirect(GetRoot(FRAME_STACK));
  u_int rsize = (size + 1);

  s->AllocArgFrame(rsize);
  s->PutFrameArg(0, rsize);
}

inline void Interpreter::PushTailFrame(u_int size) {
  Stack *s    = Stack::FromWordDirect(GetRoot(FRAME_STACK));
  u_int rsize = Store::DirectWordToInt(s->GetFrameArg(0));

  for (u_int i = rsize; i--;) {
    s->PutFrameArg(i, 0);
  }
  s->ClearArgFrameZero(rsize);
  rsize = (size + 1);
  s->AllocArgFrame(rsize);
  s->PutFrameArg(0, rsize);
}

inline void Interpreter::PopFrame() {
  Stack *s   = Stack::FromWordDirect(GetRoot(FRAME_STACK));
  u_int size = Store::DirectWordToInt(s->GetFrameArg(0));

  for (u_int i = size; i--;) {
    s->PutFrameArg(i, 0);
  }
  s->ClearArgFrameZero(size);
}

// Push and Pop Closures
inline void Interpreter::PushClosure(word closure) {
  Stack::FromWordDirect(GetRoot(CLOSURE_STACK))->SlowPush(closure);
}

inline void Interpreter::PushTailClosure(word closure) {
  Stack::FromWordDirect(GetRoot(CLOSURE_STACK))->PutFrameArg(0, closure);
}

inline void Interpreter::PopClosure() {
  Stack::FromWordDirect(GetRoot(CLOSURE_STACK))->SlowPop();
}

// Push elem i of current FRAME/CLOSURE/GLOBAL to EVAL_STACK
inline void Interpreter::PushFrameArg(u_int i) {
  PushValue(Stack::FromWordDirect(GetRoot(FRAME_STACK))->GetFrameArg((1 + i)));
}

inline void Interpreter::PushClosureArg(u_int i) {
  PushValue(Store::DirectWordToBlock(Stack::FromWordDirect(GetRoot(CLOSURE_STACK))->Top())
	    ->GetArg(i));
}

inline void Interpreter::PushGlobalArg(u_int i) {
  PushValue(EnlargableArray::FromWord(GetRoot(GLB_ENV))->Sub(i));
}

// Pop from EVAL_STACK and write to elem i of current FRAME/CLOSURE/GLOBAL
inline void Interpreter::AssignFrameArg(u_int i) {
  Stack::FromWordDirect(GetRoot(FRAME_STACK))->PutFrameArg((1 + i), PopValue());
}

inline void Interpreter::AssignClosureArg(u_int i) {
  Store::DirectWordToBlock(Stack::FromWordDirect(GetRoot(CLOSURE_STACK))->Top())
    ->ReplaceArg(i, PopValue());
}

inline void Interpreter::AssignGlobalArg(u_int i) {
  EnlargableArray::FromWord(GetRoot(GLB_ENV))->Update(i, PopValue());
}

inline word Interpreter::FetchSemiValue(IdNode *node) {
  Stack *s     = Stack::FromWordDirect(GetRoot(FRAME_STACK));
  u_int offset = 0;
  
  for (s_int delta = (node->GetFrame() - 1); delta--;) {
    offset += Store::DirectWordToInt(s->GetFrameArg(offset));
  }
  return s->GetFrameArg((1 + offset + node->GetIndex()));
}

inline void Interpreter::CreateId(u_int i, PrimType type) {
  word op = PrimOpNode::New(type)->ToWord();
  EnlargableArray::FromWord(GetRoot(GLB_ENV))->Update(i, op);
}

void Interpreter::CreateDefaultEnv() {
  CreateId(GlobalAlloc("+"), OP_PLUS);
  CreateId(GlobalAlloc("-"), OP_MINUS);
  CreateId(GlobalAlloc("*"), OP_MUL);
  CreateId(GlobalAlloc("/"), OP_DIV);
  CreateId(GlobalAlloc("<"), OP_LESS);
  CreateId(GlobalAlloc(">"), OP_GREATER);
  CreateId(GlobalAlloc("="), OP_EQUAL);
  CreateId(GlobalAlloc("eq?"), OP_EQ);
  CreateId(GlobalAlloc("use"), OP_USE);
  CreateId(GlobalAlloc("cons"), OP_CONS);
  CreateId(GlobalAlloc("car"), OP_CAR);
  CreateId(GlobalAlloc("cdr"), OP_CDR);
  CreateId(GlobalAlloc("show"), OP_SHOW);
  CreateId(GlobalAlloc("time"), OP_TIME);
  CreateId(GlobalAlloc("gengc"), OP_GENGC);
  CreateId(GlobalAlloc("setgc"), OP_SETGC);
  CreateId(GlobalAlloc("show_list"), OP_SHOWLIST);
  CreateId(GlobalAlloc("exit"), OP_EXIT);
  CreateId(GlobalAlloc("memstat"), OP_MEMSTAT);
  CreateId(GlobalAlloc("skip"), OP_SKIP);
  CreateId(GlobalAlloc("set_car!"), OP_SETCAR);
  CreateId(GlobalAlloc("set_cdr!"), OP_SETCDR);
  CreateId(GlobalAlloc("killtop"), OP_KILLTOP);
}

inline void Interpreter::InterpretDeclArr(Block *instr) {
  u_int size = instr->GetSize();
  
  for (int i = size; i--;) {
    PushTask(instr->GetArg(i));
  }
  if (allowGC && Store::NeedGC()) {
    Store::DoGC(root);
#if defined(STORE_DEBUG)
    Store::MemStat();
    std::printf("TASK_STACK size %d\n", StackSize(GetRoot(TASK_STACK)));
    std::printf("EVAL_STACK size %d\n", StackSize(GetRoot(EVAL_STACK)));
    std::printf("FRAME_STACK size %d\n", StackSize(GetRoot(FRAME_STACK)));
    std::printf("CLOSURE_STACK size %d\n", StackSize(GetRoot(CLOSURE_STACK)));
#endif
  }
}

inline void Interpreter::InterpretDefine(Block *instr) {
  DefineNode *node = DefineNode::FromBlock(instr);
  
  PushTask(AssignNode::New(node->GetId())->ToWord());
  PushTask(node->GetExpr());
}

inline void Interpreter::InterpretAssign(Block *instr) {
  IdNode *node = AssignNode::FromBlock(instr)->GetId();
  
  switch (node->GetType()) {
  case T_LOCAL_VAR:
    AssignFrameArg(node->GetIndex()); break;
  case T_SEMI_LOCAL_VAR:
    AssignClosureArg(node->GetIndex()); break;
  case T_GLOBAL_VAR:
    AssignGlobalArg(node->GetIndex()); break;
  }
}

inline void Interpreter::InterpretIf(Block *instr) {
  IfNode *node = IfNode::FromBlock(instr);

  PushTask(node->GetSelect());
  PushTask(node->GetCond());
}

inline void Interpreter::InterpretSelection(Block *instr) {
  SelectionNode *node = SelectionNode::FromBlock(instr);
  
  PushTask((IntNode::FromWord(PopValue())->GetInt() ? node->GetThen() : node->GetElse()));
}

inline void Interpreter::InterpretValue(Block *instr) {
  PushValue(instr->ToWord());
}

inline void Interpreter::InterpretId(Block *instr) {
  IdNode *node = IdNode::FromBlock(instr);

  switch (node->GetType()) {
  case T_LOCAL_VAR:
    PushFrameArg(node->GetIndex()); break;
  case T_SEMI_LOCAL_VAR:
    PushClosureArg(node->GetIndex()); break;
  case T_GLOBAL_VAR:
    PushGlobalArg(node->GetIndex()); break;
  }
}

inline void Interpreter::InterpretLet(Block *instr) {
  LetNode *node  = LetNode::FromBlock(instr);
  Block *abs     = LambdaNode::New(node->GetId()->ToWord(), node->GetBody());
  Block *applist = ConsCell::New(node->GetExpr(),
				  ConsCell::New(abs->ToWord(), Store::IntToWord(0))->ToWord());
  word exarr     = ConsCell::FromBlock(applist)->ToArray(T_EXPRARR)->ToWord();
  Block *app     = ApplicationNode::New(exarr, 0);

  PushTask(app->ToWord());
}

inline void Interpreter::InterpretLambda(Block *instr) {
  LambdaNode *node = LambdaNode::FromBlock(instr);
  u_int len        = node->GetEnvSize();
  Block *arr       = Store::AllocBlock((BlockLabel) T_CLOSUREARR, len);

  word l = node->GetEnv();
  for (u_int i = len; i--;) {
    ConsCell *cell = ConsCell::FromWord(l);

    arr->ReplaceArg(i, FetchSemiValue(IdNode::FromWord(cell->Car())));
    l = cell->Cdr();
  }
  PushValue(node->MakeClosure(arr)->ToWord());
}

inline void Interpreter::InterpretApplication(Block *instr) {
  ApplicationNode *node = ApplicationNode::FromBlock(instr);
  Block *arr            = Store::DirectWordToBlock(node->GetExprArr());
  u_int size            = arr->GetSize();

  PushTask(Store::IntToWord(((node->IsTail()) ? T_TAILAPPLY : T_APPLY)));
  for (u_int i = 0; i < size; i++) {
    PushTask(arr->GetArg(i));
  }
}

inline void Interpreter::InterpretBegin(Block *instr) {
  BeginNode *node = BeginNode::FromBlock(instr);
  Block *arr      = Store::DirectWordToBlock(node->GetExprArr());
  u_int size      = arr->GetSize();
  
  for (u_int i = size; i--;) {
    PushTask(arr->GetArg(i));
  }
}

inline void Interpreter::InterpretSetQ(Block *instr) {
  SetQNode *node = SetQNode::FromBlock(instr);

  PushTask(AssignNode::New(node->GetId())->ToWord());
  PushTask(node->GetExpr());
}

inline void Interpreter::InterpretSetCxr(NodeType type, Block *instr) {
  SetCxrNode *node = SetCxrNode::FromBlock(instr);
  
  PushTask(Store::IntToWord((int) type));
  PushTask(node->GetCell());
  PushTask(node->GetExpr());
}

inline void Interpreter::InterpretRemove() {
  PopFrame();
  PopClosure();
}

inline void Interpreter::InterpretTime() {
  gettimeofday(&end_t, INVALID_POINTER);
#if (defined(STORE_DEBUG) || defined(STORE_PROFILE))
  struct timeval *sum_t = Store::ReadTime();
  long total_sec        = (end_t.tv_sec - start_t.tv_sec);
  long total_usec       = (end_t.tv_usec - start_t.tv_usec);

  long calc_time   = (((total_sec - sum_t->tv_sec) * 1000) +
		      ((total_usec - sum_t->tv_usec) / 1000));
  long gc_time     = ((sum_t->tv_sec * 1000) + (sum_t->tv_usec / 1000));
  long total_time  = ((total_sec * 1000) + (total_usec / 1000));
  double  weight   = (0.0 + gc_time) / (0.0 + total_time) * 100;
  std::printf("CC: %ld ms; GC: %ld ms; ALL: %ld ms; GW: %g percent\n",
	      calc_time, gc_time, total_time, weight);
  if (total_time == 0) {
    double all_mem = ((0.0 + Store::totalMem) / (1024 * 1024));
    std::printf("AT %g MB; ", all_mem);
  }
  else {
    double all_mem   = ((0.0 + Store::totalMem) / (1024 * 1024));
    double all_ratio = (all_mem / total_time * 1000);
    std::printf("AT %g MB at %g MB/s; ", all_mem, all_ratio);
  }
  if (gc_time == 0) {
    double gc_mem =  (0.0 + Store::gcLiveMem) / (1024 * 1024);
    std::printf("GCT: %g MB\n", gc_mem);
  }
  else {
    double gc_mem   =  (0.0 + Store::gcLiveMem) / (1024 * 1024);
    double gc_ratio = (gc_mem / gc_time * 1000); 
    std::printf("GCT %g MB at %g MB/s\n", gc_mem, gc_ratio);
  }
  std::printf("TASK_STACK size %d\n", StackSize(GetRoot(TASK_STACK)));
  std::printf("EVAL_STACK size %d\n", StackSize(GetRoot(EVAL_STACK)));
  std::printf("FRAME_STACK size %d\n", StackSize(GetRoot(FRAME_STACK)));
  std::printf("CLOSURE_STACK size %d\n", StackSize(GetRoot(CLOSURE_STACK)));
  Store::MemStat();
#endif
  std::fflush(stdout);
}

char *Interpreter::InterpretOp(Block *p) {
  switch (PrimOpNode::FromBlock(p)->GetType()) {
  case OP_PLUS: {
    int a, b;
    
    if (BothInt(PopValue(), PopValue(), b, a)) {
      PushValue(IntNode::New(a + b)->ToWord());
    }
    else {
      std::fprintf(stderr, "Interpreter::Interpret: evaluation of `+' needs int arguments\n");
      exit(0);
    }
    break;
  }
  case OP_MINUS: {
    int a, b;
    
    if (BothInt(PopValue(), PopValue(), b, a)) {
      PushValue(IntNode::New(a - b)->ToWord());
    }
    else {
      std::fprintf(stderr, "Interpret::Interpret: evaluation of `-' needs int arguments\n");
      exit(0);
    }
    break;
  }
  case OP_MUL: {
    int a, b;
    
    if (BothInt(PopValue(), PopValue(), b, a)) {
      PushValue(IntNode::New(a * b)->ToWord());
    }
    else {
      std::fprintf(stderr, "Interpret::Interpret: evaluation of `*' needs int arguments\n");
      exit(0);
    }
    break;
  }
  case OP_DIV: {
    int a, b;
    
    if (BothInt(PopValue(), PopValue(), b, a)) {
      PushValue(IntNode::New(a / b)->ToWord());
    }
    else {
      std::fprintf(stderr, "Interpret::Interpret: evaluation of `/' needs int arguments\n");
      exit(0);
    }
    break;
  }
  case OP_LESS: {
    int a, b;
    
    if (BothInt(PopValue(), PopValue(), b, a)) {
      PushValue(IntNode::New(a < b)->ToWord());
    }
    else {
      std::fprintf(stderr, "Interpret::Interpret: evaluation of `<' needs int arguments\n");
      exit(0);
    }
    break;
  }
  case OP_GREATER: {
    int a, b;
    
    if (BothInt(PopValue(), PopValue(), b, a)) {
      PushValue(IntNode::New(a > b)->ToWord());
    }
    else {
      std::fprintf(stderr, "Interpret::Interpret: evaluation of `>' needs int arguments\n");
      exit(0);
    }
    break;
  }
  case OP_EQUAL: {
    int a, b;
    
    if (BothInt(PopValue(), PopValue(), b, a)) {
      PushValue(IntNode::New(a == b)->ToWord());
    }
    else {
      PushValue(IntNode::New(0)->ToWord());
    }
    break;
  }
  case OP_EQ: {
    word a = PopValue();
    word b = PopValue();
    
    if (BothNil(a, b)) {
      PushValue(IntNode::New(1)->ToWord());
    }
    else {
      PushValue(IntNode::New(a == b)->ToWord());
    }
    break;
  }
  case OP_USE: {
    StringNode *a = StringNode::FromWord(PopValue());
    
    if (a != INVALID_POINTER) {
      return a->GetString();
    }
    else {
      std::fprintf(stderr, "Interpret::Interpret: evaluation of `use' needs string argument\n");
      exit(0);
    }
    break;
  }
  case OP_CONS: {
    word a = PopValue();
    word b = PopValue();
    
    PushValue(ConsCell::New(a, b)->ToWord());
    break;
  }
  case OP_CAR: {
    ConsCell *c = ConsCell::FromWord(PopValue());
    PushValue(c->Car());
    break;
  }
  case OP_CDR: {
    ConsCell *c = ConsCell::FromWord(PopValue());
    
    PushValue(c->Cdr());
    break;
  }
  case OP_SHOW: {
    Block *p = Store::DirectWordToBlock(PopValue());
    
    switch ((NodeType) p->GetLabel()) {
    case T_INT:
      std::printf("%d\n", IntNode::FromBlock(p)->GetInt());
      break;
    case T_STRING:
      std::printf("%s\n", StringNode::FromBlock(p)->GetString());
      break;
    default:
      break;
    }
    break;
  }
  case OP_TIME: {
    PushTask(Store::IntToWord(T_TIME));
    PushTask(ApplicationNode::FromLambda(PopValue())->ToWord());
#if (defined(STORE_DEBUG) || defined(STORE_PROFILE))
    Store::ResetTime();
#endif
    gettimeofday(&start_t, INVALID_POINTER);
    break;
  }
  case OP_GENGC: {
    std::printf("TASK_STACK size %d\n", StackSize(GetRoot(TASK_STACK)));
    std::printf("EVAL_STACK size %d\n", StackSize(GetRoot(EVAL_STACK)));
    std::printf("FRAME_STACK size %d\n", StackSize(GetRoot(FRAME_STACK)));
    std::printf("CLOSURE_STACK size %d\n", StackSize(GetRoot(CLOSURE_STACK)));
    Store::DoGC(root);
#if defined(STORE_DEBUG) || defined(STORE_PROFILE)
    Store::MemStat();
#endif
    break;
  }
  case OP_SETGC: {
    static const char *val[]= {"off", "on" };

    allowGC = IntNode::FromWord(PopValue())->GetInt();
    std::printf("Automatic Garbage Collection is %s.\n", val[allowGC]);
    break;
  }
  case OP_SHOWLIST:
    ShowList(Store::DirectWordToBlock(PopValue()));
    std::printf("\n");
    std::fflush(stdout);
    break;
  case OP_EXIT: {
    exit(0);
    break;
  }
  case OP_MEMSTAT: {
#if defined(STORE_DEBUG) || defined(STORE_PROFILE)
    std::printf("TASK_STACK size %d\n", StackSize(GetRoot(TASK_STACK)));
    std::printf("EVAL_STACK size %d\n", StackSize(GetRoot(EVAL_STACK)));
    std::printf("FRAME_STACK size %d\n", StackSize(GetRoot(FRAME_STACK)));
    std::printf("CLOSURE_STACK size %d\n", StackSize(GetRoot(CLOSURE_STACK)));
    Store::MemStat();
#endif
    break;
  }
  case OP_SKIP:
    break;
  case OP_SETCAR: {
    ConsCell *cell = ConsCell::FromWord(PopValue());
    word expr      = PopValue();
    cell->SetCar(expr);
    break;
  }
  case OP_SETCDR: {
    ConsCell *cell = ConsCell::FromWord(PopValue());
    word expr      = PopValue();
    cell->SetCdr(expr);
    break;
  }
  case OP_KILLTOP:
    PopValue();
    break;
  default:
    break;
  }

  return INVALID_POINTER;
}

// Public Methods
void Interpreter::Init() {
  Block *p = Store::AllocBlock(MIN_DATA_LABEL, ROOTSET_SIZE);

  p->InitArg(TASK_STACK, Stack::New(STACK_SIZE)->ToWord());
  p->InitArg(EVAL_STACK, Stack::New(STACK_SIZE)->ToWord());
  p->InitArg(FRAME_STACK, Stack::New(STACK_SIZE)->ToWord());
  p->InitArg(CLOSURE_STACK, Stack::New(STACK_SIZE)->ToWord());
  p->InitArg(GLB_ENV_LIST, Store::IntToWord(0));
  p->InitArg(GLB_ENV, EnlargableArray::New((BlockLabel) T_GLBENV, STACK_SIZE)->ToWord());
  p->InitArg(ATOM_DICT, AtomDictionary::New(DICT_SIZE)->ToWord());
  root    = p->ToWord();
  allowGC = 0;
  CreateDefaultEnv();
}

// Public Interpreter Methods
u_int Interpreter::RegisterAtom(const char *s) {
  return AtomDictionary::FromWord(GetRoot(ATOM_DICT))->FromString(s);
}

char *Interpreter::AtomToString(u_int name) {
  return AtomDictionary::FromWord(GetRoot(ATOM_DICT))->ToString(name);
}

u_int Interpreter::GlobalAlloc(u_int name) {
  word frame = GetRoot(GLB_ENV_LIST);
  int l;

  if ((l = Environment::SearchFrame(frame, name)) >= 0) {
    return l;
  }
  else {
    frame = ConsCell::New(IdNode::New(name)->ToWord(), frame)->ToWord();
    Store::DirectWordToBlock(root)->InitArg(GLB_ENV_LIST, frame);
    return EnlargableArray::FromWord(GetRoot(GLB_ENV))->AllocSlot();
  }
}

u_int Interpreter::GlobalAlloc(const char *s) {
  return GlobalAlloc(RegisterAtom(s));
}

int Interpreter::SearchGlobal(u_int name) {
  return Environment::SearchFrame(GetRoot(GLB_ENV_LIST), name);
}

char *Interpreter::Interpret(word tree) {
  char *fn = INVALID_POINTER;
  PushTask(tree);
  
  while (HaveTask()) {
    word task = PopTask();

    if (PointerOp::IsInt(task)) {
      switch ((NodeType) Store::DirectWordToInt(task)) {
      case T_TIME:
	InterpretTime(); break;
      case T_REMOVE:
	InterpretRemove(); break;
      case T_TAILAPPLY: {
	fn       = NULL;
	Block *p = Store::DirectWordToBlock(PopValue());
	if (p->GetLabel() == (BlockLabel) T_CLOSURE) {
	  ClosureNode *abs = ClosureNode::FromBlock(p);
	
	  PushTask(abs->GetBody());
	  PushTailFrame(abs->GetFrameSize());
	  PushTailClosure(abs->GetEnv());

	  Block *arr = Store::DirectWordToBlock(abs->GetArgList());
	  for (u_int i = arr->GetSize(); i--;) {
	    PushTask(AssignNode::New(IdNode::FromWord(arr->GetArg(i)))->ToWord());
	  }
	}
	else {
	  fn = InterpretOp(p);
	}
  
	if (allowGC && Store::NeedGC()) {
	  Store::DoGC(root);
#if defined(STORE_DEBUG)
	  Store::MemStat();
	  std::printf("TASK_STACK size %d\n", StackSize(GetRoot(TASK_STACK)));
	  std::printf("EVAL_STACK size %d\n", StackSize(GetRoot(EVAL_STACK)));
	  std::printf("FRAME_STACK size %d\n", StackSize(GetRoot(FRAME_STACK)));
	  std::printf("CLOSURE_STACK size %d\n", StackSize(GetRoot(CLOSURE_STACK)));
#endif
	}
	break;
      }
      case T_APPLY: {
	fn       = NULL;
	Block *p = Store::DirectWordToBlock(PopValue());
	if (p->GetLabel() == (BlockLabel) T_CLOSURE) {
	  ClosureNode *abs = ClosureNode::FromBlock(p);
	
	  PushTask(Store::IntToWord(T_REMOVE));
	  PushTask(abs->GetBody());
	  PushFrame(abs->GetFrameSize());
	  PushClosure(abs->GetEnv());

	  Block *arr = Store::DirectWordToBlock(abs->GetArgList());
	  for (u_int i = arr->GetSize(); i--;) {
	    PushTask(AssignNode::New(IdNode::FromWord(arr->GetArg(i)))->ToWord());
	  }
	}
	else {
	  fn = InterpretOp(p);
	}
  
	if (allowGC && Store::NeedGC()) {
	  Store::DoGC(root);
#if defined(STORE_DEBUG)
	  Store::MemStat();
	  std::printf("TASK_STACK size %d\n", StackSize(GetRoot(TASK_STACK)));
	  std::printf("EVAL_STACK size %d\n", StackSize(GetRoot(EVAL_STACK)));
	  std::printf("FRAME_STACK size %d\n", StackSize(GetRoot(FRAME_STACK)));
	  std::printf("CLOSURE_STACK size %d\n", StackSize(GetRoot(CLOSURE_STACK)));
#endif
	}
	break;
      }
      default:
	break;
      }
    }
    else {
      Block *instr = Store::DirectWordToBlock(task);
    
      switch ((NodeType) instr->GetLabel()) {
      case T_DECLARR:
	InterpretDeclArr(instr); break;
      case T_DEFINE:
	InterpretDefine(instr); break;
      case T_ASSIGN:
	InterpretAssign(instr); break;
      case T_IF:
	InterpretIf(instr); break;
      case T_SELECTION:
	InterpretSelection(instr); break;
      case T_INT:
      case T_STRING:
      case T_PRIMOP:
      case T_CLOSURE:
      case T_NIL:
	InterpretValue(instr); break;
      case T_ID:
	InterpretId(instr); break;
      case T_LET:
	InterpretLet(instr); break;
      case T_LAMBDA:
	InterpretLambda(instr); break;
      case T_APPLICATION:
	InterpretApplication(instr); break;
      case T_BEGIN:
	InterpretBegin(instr); break;
      case T_SETQ:
	InterpretSetQ(instr); break;
      case T_SETCAR:
	InterpretSetCxr(T_SETCAR, instr); break;
      case T_SETCDR:
	InterpretSetCxr(T_SETCDR, instr); break;
      default:
	break;
      }
    }
  }
  return fn;
}
