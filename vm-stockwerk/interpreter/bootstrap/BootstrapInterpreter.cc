//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "datalayer/alicedata.hh"
#include "scheduler/scheduler.hh"

#include "Pickle.hh"

#include "interpreter/bootstrap/Environment.hh"

//
// Helper Classes
//

class Closure: private Tuple { //--** should move to alicedata.hh
private:
  static const int PC_POS = 1;
  static const int GLOBAL_ENV_POS = 2;
  static const int FORMAL_ARGS_POS = 3;
  static const u_int CLOSURE_SIZE = 3;
public:
  using Block::ToWord;

  static Closure *New(TagVal *pc, Vector *globalEnv, TagVal *formalArgs) {
    Closure *closure = static_cast<Closure *>(Tuple::New(CLOSURE_SIZE));
    closure->InitArg(PC_POS, pc->ToWord());
    closure->InitArg(GLOBAL_ENV_POS, globalEnv->ToWord());
    closure->InitArg(FORMAL_ARGS_POS, formalArgs->ToWord());
    return closure;
  }
  static Closure *FromWord(word w) {
    return static_cast<Closure *>(Tuple::FromWord(w));
  }
  TagVal *GetPC() {
    return TagVal::FromWord(GetArg(PC_POS));
  }
  Vector *GetGlobalEnv() {
    return Vector::FromWord(GetArg(GLOBAL_ENV_POS));
  }
  TagVal *GetFormalArgs() {
    return TagVal::FromWord(GetArg(FORMAL_ARGS_POS));
  }
};

//
// Scheduler Interface
//

typedef enum {
  EXCEPTION, // of stack * exception
  PREEMPT,   // of stack
  SUSPEND,   // of stack * future
  TERMINATE  // (nullary)
} interpreter_result_label;

static word mkException(StackFrame *stack, word exception) {
  TagVal *tagVal = TagVal::New(EXCEPTION, 2);
  tagVal->InitArg(1, stack->ToWord());
  tagVal->InitArg(2, exception);
  return tagVal->ToWord();
}

static word mkPreempt(StackFrame *stack) {
  TagVal *tagVal = TagVal::New(PREEMPT, 1);
  tagVal->InitArg(1, stack->ToWord());
  return tagVal->ToWord();
}

static word mkSuspend(TagVal *pc, Vector *globalEnv, Environment *localEnv,
		      StackFrame *stack, word future) {
  //--** make new stack frame
  TagVal *tagVal = TagVal::New(SUSPEND, 2);
  tagVal->InitArg(1, stack->ToWord());
  tagVal->InitArg(2, future);
  return tagVal->ToWord();
}

static word mkTerminate() {
  return Store::IntToWord(TERMINATE);
}

//
// The Interpreter Proper
//

#define combineArgs(args1, args2) ((args1) * 2 + (args2))

//--** reading operands: FromWord tests for transients, but we don't

Scheduler::action interpret(int nargs, TaskStack *&stack, word &data) {
  if (nargs < 0) { // return
  } else if (nargs > 0) { // call
  } else { // nargs == 0: start
  }

  word suspendWord = Store::IntToWord(0);
  TagVal *pc = stack->GetPC();
  Vector *globalEnv = stack->GetGlobalEnv();
  Environment *localEnv = stack->GetEnvironment();
  while (!Store::NeedGC()) { //--** test preemption flag
    switch (static_cast<Pickle::instr_label>(pc->GetLabel())) {
    case Pickle::PutConst: // of id * value * instr
      {
	localEnv->Add(pc->GetArg(1), pc->GetArg(2));
	pc = TagVal::FromWord(pc->GetArg(3));
      }
      break;
    case Pickle::PutNew: // of id * instr
      {
	localEnv->Add(pc->GetArg(1), Constructor::New()->ToWord());
	pc = TagVal::FromWord(pc->GetArg(2));
      }
      break;
    case Pickle::PutGlobal: // of id * int * instr
      {
	localEnv->Add(pc->GetArg(1),
		      globalEnv->GetArg(Store::WordToInt(pc->GetArg(2)) + 1));
	pc = TagVal::FromWord(pc->GetArg(3));
      }
      break;
    case Pickle::PutTag: // of id * int * id vector * instr
      {
	Vector *ids = Vector::FromWord(pc->GetArg(3));
	u_int nargs = ids->GetSize();
	TagVal *tagVal = TagVal::New(Store::WordToInt(pc->GetArg(2)), nargs);
	for (u_int i = 1; i <= nargs; i++)
	  tagVal->InitArg(i, localEnv->Lookup(ids->GetArg(i)));
	localEnv->Add(pc->GetArg(1), tagVal->ToWord());
	pc = TagVal::FromWord(pc->GetArg(4));
      }
      break;
    case Pickle::PutCon: // of id * con * id vector * instr
      {
	Vector *ids = Vector::FromWord(pc->GetArg(3));
	u_int nargs = ids->GetSize();
	Constructor *constructor;
	TagVal *conBlock = TagVal::FromWord(pc->GetArg(2));
	switch (static_cast<Pickle::con_label>(conBlock->GetLabel())) {
	case Pickle::Con:
	  suspendWord = localEnv->Lookup(conBlock->GetArg(1));
	  constructor = Constructor::FromWord(suspendWord);
	  if (constructor == INVALID_POINTER)
	    return mkSuspend(pc, globalEnv, localEnv, stack, suspendWord);
	  break;
	case Pickle::StaticCon:
	  constructor = Constructor::FromWord(conBlock->GetArg(1));
	  break;
	}
	ConVal *conVal = ConVal::New(constructor, nargs);
	for (u_int i = 1; i <= nargs; i++)
	  conVal->InitArg(i, localEnv->Lookup(ids->GetArg(i)));
	localEnv->Add(pc->GetArg(1), conVal->ToWord());
	pc = TagVal::FromWord(pc->GetArg(4));
      }
      break;
    case Pickle::PutRef: // of id * id * instr
      {
	word contents = localEnv->Lookup(pc->GetArg(2));
	Cell *cell = Cell::New(contents);
	localEnv->Add(pc->GetArg(1), cell->ToWord());
	pc = TagVal::FromWord(pc->GetArg(3));
      }
      break;
    case Pickle::PutTup: // of id * id vector * instr
      {
	Vector *ids = Vector::FromWord(pc->GetArg(2));
	u_int nargs = ids->GetSize();
	Tuple *tuple = Tuple::New(nargs);
	for (u_int i = 1; i <= nargs; i++)
	  tuple->InitArg(i, localEnv->Lookup(ids->GetArg(i)));
	localEnv->Add(pc->GetArg(1), tuple->ToWord());
	pc = TagVal::FromWord(pc->GetArg(3));
      }
      break;
    case Pickle::PutSel: // of id * int * id * instr
      {
	suspendWord = localEnv->Lookup(pc->GetArg(3));
	Tuple *tuple = Tuple::FromWord(suspendWord);
	if (tuple == INVALID_POINTER)
	  return mkSuspend(pc, globalEnv, localEnv, stack, suspendWord);
	word value = tuple->GetArg(Store::WordToInt(pc->GetArg(2)));
	localEnv->Add(pc->GetArg(1), value);
	pc = TagVal::FromWord(pc->GetArg(4));
      }
      break;
    case Pickle::PutVec: // of id * id vector * instr
      {
	Vector *ids = Vector::FromWord(pc->GetArg(2));
	u_int nargs = ids->GetSize();
	Vector *vector = Vector::New(nargs);
	for (u_int i = 1; i <= nargs; i++)
	  vector->InitArg(i, localEnv->Lookup(ids->GetArg(i)));
	pc = TagVal::FromWord(pc->GetArg(3));
      }
      break;
    case Pickle::PutFun: // of id * id vector * idDef args * instr * instr
      {
	Vector *ids = Vector::FromWord(pc->GetArg(2));
	u_int nglobals = ids->GetSize();
	Vector *newGlobalEnv = Vector::New(nglobals);
	for (u_int i = 1; i <= nglobals; i++) {
	  word value = localEnv->Lookup(ids->GetArg(i));
	  newGlobalEnv->InitArg(i, value);
	}
	TagVal *newPC = TagVal::FromWord(pc->GetArg(4));
	TagVal *formalArgs = TagVal::FromWord(pc->GetArg(3));
	Closure *closure = Closure::New(newPC, newGlobalEnv, formalArgs);
	localEnv->Add(pc->GetArg(1), closure->ToWord());
	pc = TagVal::FromWord(pc->GetArg(5));
      }
      break;
    case Pickle::Kill: // of id vector * instr
      {
	Vector *kills = Vector::FromWord(pc->GetArg(1));
	u_int nkills = kills->GetSize();
	for (u_int i = 1; i <= nkills; i++)
	  localEnv->Kill(kills->GetArg(i));
	pc = TagVal::FromWord(pc->GetArg(2));
      }
      break;
    case Pickle::AppPrim: // of id * string * id vector * instr
      {
	Vector *ids = Vector::FromWord(pc->GetArg(3));
	void (*functionPtr)(); //--** obtain from builtin table
	word result;
	switch (ids->GetSize()) {
	case 0:
	  {
	    word (*builtin)() = reinterpret_cast<word (*)()>(functionPtr);
	    result = builtin();
	  }
	  break;
	case 1:
	  {
	    word (*builtin)(word) =
	      reinterpret_cast<word (*)(word)>(functionPtr);
	    result = builtin(localEnv->Lookup(ids->GetArg(1)));
	  }
	  break;
	case 2:
	  {
	    word (*builtin)(word, word) =
	      reinterpret_cast<word (*)(word, word)>(functionPtr);
	    result = builtin(localEnv->Lookup(ids->GetArg(1)),
			     localEnv->Lookup(ids->GetArg(2)));
	  }
	  break;
	case 3:
	  {
	    word (*builtin)(word, word, word) =
	      reinterpret_cast<word (*)(word, word, word)>(functionPtr);
	    result = builtin(localEnv->Lookup(ids->GetArg(1)),
			     localEnv->Lookup(ids->GetArg(2)),
			     localEnv->Lookup(ids->GetArg(3)));
	  }
	  break;
	default:
	  Assert(0);
	  break;
	}
	localEnv->Add(pc->GetArg(1), result);
	pc = TagVal::FromWord(pc->GetArg(4));
      }
      break;
    case Pickle::AppVar: // of id * id * id args * instr //--** id vector
      {
	suspendWord = pc->GetArg(2);
	Closure *closure = Closure::FromWord(suspendWord);
	if (closure == INVALID_POINTER)
	  return mkSuspend(pc, globalEnv, localEnv, stack, suspendWord);
	// Test for calling convention conversion:
	TagVal *actualArgs = TagVal::FromWord(pc->GetArg(3));
	TagVal *formalArgs = closure->GetFormalArgs();
	Environment *calleeEnv = Environment::New(envSize);
	switch (combineArgs(actualArgs->GetLabel(), formalArgs->GetLabel())) {
	case combineArgs(Pickle::OneArg, Pickle::OneArg):
	  {
	    TagVal formalIdDef = TagVal::FromWord(formalArgs->GetArg(1));
	    if (formalIdDef != INVALID_POINTER) { // SOME id
	      word actualArg = localEnv->Lookup(actualArgs->GetArg(1));
	      calleeEnv->Add(formalIdDef->GetArg(1), actualArg);
	  }
	  break;
	case combineArgs(Pickle::OneArg, Pickle::TupArgs): // deconstruct
	  {
	    TagVal *formals = TagVal::FromWord(formalArgs->GetArg(1));
	    suspendWord = localEnv->Lookup(actualArgs->GetArg(1));
	    Tuple *tuple = Tuple::FromWord(suspendWord);
	    if (tuple == INVALID_POINTER)
	      return mkSuspend(pc, globalEnv, localEnv, stack, suspendWord);
	    u_int nargs = tuple->GetSize();
	    for (u_int i = 1; i <= nargs; i++) {
	      TagVal formalIdDef = TagVal::FromWord(formals->GetArg(1));
	      if (formalIdDef != INVALID_POINTER) { // SOME id
		word actualArg = localEnv->Lookup(actualArgs->GetArg(1));
		calleeEnv->Add(formalIdDef->GetArg(1), tuple->GetArg(i));
	      }
	    }
	  }
	  break;
	case combineArgs(Pickle::TupArgs, Pickle::OneArg): // construct
	  {
	    TagVal formalIdDef = TagVal::FromWord(formalArgs->GetArg(1));
	    if (formalIdDef != INVALID_POINTER) { // SOME id
	      TagVal *actuals = TagVal::FromWord(actualArgs->GetArg(1));
	      u_int nargs = actuals->GetSize();
	      Tuple *tuple = Tuple::New(nargs);
	      for (u_int i = 1; i <= nargs; i++)
		tuple->InitArg(i, localEnv->Lookup(actuals->GetArg(i)));
	      calleeEnv->Add(formalId, tuple->ToWord());
	    }
	  }
	  break;
	case combineArgs(Pickle::TupArgs, Pickle::TupArgs):
	  {
	    TagVal *formals = TagVal::FromWord(formalArgs->GetArg(1));
	    TagVal *actuals = TagVal::FromWord(actualArgs->GetArg(1));
	    u_int nargs = formals->GetSize();
	    Assert(actuals->GetSize() == nargs);
	    for (u_int i = 1; i <= nargs; i++) {
	      TagVal formalIdDef = TagVal::FromWord(formals->GetArg(1));
	      if (formalIdDef != INVALID_POINTER) { // SOME id
		word actualArg = localEnv->Lookup(actuals->GetArg(i));
		calleeEnv->Add(formalIdDef->GetArg(i), actualArg);
	      }
	    }
	  }
	  break;
	}
	// Set up interpreter registers for callee:
	stack = stack->Push(TagVal::FromWord(pc->GetArg(4)), globalEnv,
			    localEnv, Store::WordToInt(pc->GetArg(1)));
	pc = closure->GetPC();
	globalEnv = closure->GetGlobalEnv();
	localEnv = calleeEnv;
      }
      break;
    case Pickle::GetTup: // of idDef vector * id * instr
      {
	suspendWord = pc->GetArg(2);
	Tuple *tuple = Tuple::FromWord(suspendWord);
	if (tuple == INVALID_POINTER)
	  return mkSuspend(pc, globalEnv, localEnv, stack, suspendWord);
	Vector *idDefs = Vector::FromWord(pc->GetArg(1));
	u_int nargs = idDefs->GetSize();
	for (u_int i = 1; i <= nargs; i++) {
	  TagVal *idDef = TagVal::FromWord(idDefs->GetArg(i));
	  if (idDef != INVALID_POINTER) // SOME id
	    localEnv->Add(idDef->GetArg(1), tuple->GetArg(i));
	}
	pc = TagVal::FromWord(pc->GetArg(3));
      }
      break;
    case Pickle::IntTest: // of id * (int * instr) vector * instr
      {
	suspendWord = localEnv->Lookup(pc->GetArg(1));
	int value = Store::WordToInt(suspendWord);
	//--** test for transients
	Vector *tests = Vector::FromWord(pc->GetArg(2));
	u_int ntests = tests->GetSize();
	for (u_int i = 1; i <= ntests; i++) {
	  Tuple *pair = Tuple::FromWord(tests->GetArg(i));
	  if (Store::WordToInt(pair->GetArg(1)) == value) {
	    pc = TagVal::FromWord(pair->GetArg(2));
	    goto loop;
	  }
	}
	pc = TagVal::FromWord(pc->GetArg(3));
      }
      break;
    case Pickle::RealTest: // of id * (real * instr) vector * instr
      {
	suspendWord = localEnv->Lookup(pc->GetArg(1));
	Real *real = Real::FromWord(suspendWord);
	if (real == INVALID_POINTER)
	  return mkSuspend(pc, globalEnv, localEnv, stack, suspendWord);
	double value = real->GetValue();
	Vector *tests = Vector::FromWord(pc->GetArg(2));
	u_int ntests = tests->GetSize();
	for (u_int i = 1; i <= ntests; i++) {
	  Tuple *pair = Tuple::FromWord(tests->GetArg(i));
	  if (Real::FromWord(pair->GetArg(1))->GetValue() == value) {
	    pc = TagVal::FromWord(pair->GetArg(2));
	    goto loop;
	  }
	}
	pc = TagVal::FromWord(pc->GetArg(3));
      }
      break;
    case Pickle::StringTest: // of id * (string * instr) vector * instr
      //--**
      break;
    case Pickle::TagTest:
      // of id * (int * instr) vector
      //       * (int * idDef vector * instr) vector * instr
      {
	suspendWord = localEnv->Lookup(pc->GetArg(1));
	TagVal *tagVal = TagVal::FromWord(suspendWord);
	if (tagVal == INVALID_POINTER) { // nullary constructor or transient
	  int label = Store::WordToInt(suspendWord);
	  //--** test for transients!
	  Vector *tests = Vector::FromWord(pc->GetArg(2));
	  u_int ntests = tests->GetSize();
	  for (u_int i = 1; i <= ntests; i++) {
	    Tuple *pair = Tuple::FromWord(tests->GetArg(i));
	    if (Store::WordToInt(pair->GetArg(1)) == label) {
	      pc = TagVal::FromWord(pair->GetArg(2));
	      goto loop;
	    }
	  }
	} else { // non-nullary constructor
	  int label = tagVal->GetLabel();
	  Vector *tests = Vector::FromWord(pc->GetArg(2));
	  u_int ntests = tests->GetSize();
	  for (u_int i = 1; i <= ntests; i++) {
	    Tuple *triple = Tuple::FromWord(tests->GetArg(i));
	    if (Store::WordToInt(triple->GetArg(1)) == label) {
	      Vector *idDefs = Vector::FromWord(triple->GetArg(2));
	      u_int nargs = idDefs->GetSize();
	      for (u_int i = 1; i <= nargs; i++)
		//--** can be NONE
		localEnv->Add(idDefs->GetArg(i), tagVal->GetArg(i));
	      pc = TagVal::FromWord(triple->GetArg(3));
	      goto loop;
	    }
	  }
	}
	pc = TagVal::FromWord(pc->GetArg(4));
      }
      break;
    case Pickle::ConTest:
      // of id * (con * instr) vector
      //       * (con * idDef vector * instr) vector * instr
      {
	suspendWord = localEnv->Lookup(pc->GetArg(1));
	ConVal *conVal = ConVal::FromWord(suspendWord);
	if (conVal == INVALID_POINTER)
	  return mkSuspend(pc, globalEnv, localEnv, stack, suspendWord);
	if (conVal->IsConVal()) { // non-nullary constructor
	  Constructor *constructor = conVal->GetConstructor();
	  Vector *tests = Vector::FromWord(pc->GetArg(3));
	  u_int ntests = tests->GetSize();
	  for (u_int i = 1; i <= ntests; i++) {
	    Tuple *triple = Tuple::FromWord(tests->GetArg(i));
	    TagVal *conBlock = TagVal::FromWord(triple->GetArg(1));
	    Constructor *testConstructor;
	    switch (static_cast<Pickle::con_label>(conBlock->GetLabel())) {
	    case Pickle::Con:
	      suspendWord = localEnv->Lookup(conBlock->GetArg(1));
	      testConstructor = Constructor::FromWord(suspendWord);
	      if (testConstructor == INVALID_POINTER)
		return mkSuspend(pc, globalEnv, localEnv, stack, suspendWord);
	      break;
	    case Pickle::StaticCon:
	      testConstructor = Constructor::FromWord(conBlock->GetArg(1));
	      break;
	    }
	    if (testConstructor == constructor) {
	      Vector *idDefs = Vector::FromWord(triple->GetArg(2));
	      u_int nargs = idDefs->GetSize();
	      for (u_int i = 1; i <= nargs; i++)
		//--** can be NONE
		localEnv->Add(idDefs->GetArg(i), conVal->GetArg(i));
	      pc = TagVal::FromWord(triple->GetArg(3));
	      goto loop;
	    }
	  }
	} else { // nullary constructor
	  Constructor *constructor = reinterpret_cast<Constructor *>(conVal);
	  Vector *tests = Vector::FromWord(pc->GetArg(2));
	  u_int ntests = tests->GetSize();
	  for (u_int i = 1; i <= ntests; i++) {
	    Tuple *pair = Tuple::FromWord(tests->GetArg(i));
	    TagVal *conBlock = TagVal::FromWord(pair->GetArg(1));
	    Constructor *testConstructor;
	    switch (static_cast<Pickle::con_label>(conBlock->GetLabel())) {
	    case Pickle::Con:
	      suspendWord = localEnv->Lookup(conBlock->GetArg(1));
	      testConstructor = Constructor::FromWord(suspendWord);
	      if (testConstructor == INVALID_POINTER)
		return mkSuspend(pc, globalEnv, localEnv, stack, suspendWord);
	      break;
	    case Pickle::StaticCon:
	      testConstructor = Constructor::FromWord(conBlock->GetArg(1));
	      break;
	    }
	    if (testConstructor == constructor) {
	      pc = TagVal::FromWord(pair->GetArg(2));
	      goto loop;
	    }
	  }
	}
	pc = TagVal::FromWord(pc->GetArg(4));
      }
      break;
    case Pickle::VecTest: // of id * (idDef vector * instr) vector * instr
      {
	suspendWord = localEnv->Lookup(pc->GetArg(1));
	Vector *vector = Vector::FromWord(suspendWord);
	if (vector == INVALID_POINTER)
	  return mkSuspend(pc, globalEnv, localEnv, stack, suspendWord);
	int value = vector->GetSize();
	Vector *tests = Vector::FromWord(pc->GetArg(2));
	u_int ntests = tests->GetSize();
	for (u_int i = 1; i <= ntests; i++) {
	  Tuple *pair = Tuple::FromWord(tests->GetArg(i));
	  Vector *idDefs = Vector::FromWord(pair->GetArg(1));
	  u_int nargs = idDefs->GetSize();
	  if (nargs == value) {
	    for (u_int i = 1; i <= nargs; i++)
	      //--** can be NONE
	      localEnv->Add(idDefs->GetArg(i), vector->GetArg(i));
	    pc = TagVal::FromWord(pair->GetArg(2));
	    goto loop;
	  }
	}
	pc = TagVal::FromWord(pc->GetArg(3));
      }
      break;
    case Pickle::Raise: // of id
    case Pickle::Reraise: // of id
      return mkException(stack, localEnv->Lookup(pc->GetArg(1)));
    case Pickle::Return: // of id //--** of id vector
      {
	if (stack->IsEmpty())
	  return mkTerminate();
	word returnValue = localEnv->Lookup(pc->GetArg(1));
	pc = stack->GetPC();
	globalEnv = stack->GetGlobalEnv();
	localEnv = stack->GetEnvironment();
	stack = stack->Pop(returnValue);
      }
      break;
    }
  loop:
    ;
  }
  return mkPreempt(stack);
}
