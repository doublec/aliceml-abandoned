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

#include "scheduler/Scheduler.hh"
#include "scheduler/TaskStack.hh"
#include "datalayer/alicedata.hh"
#include "builtins/Primitive.hh"
#include "builtins/GlobalPrimitives.hh"
#include "interpreter/Pickle.hh"
#include "interpreter/bootstrap/Environment.hh"
#include "interpreter/bootstrap/BootstrapInterpreter.hh"

//
// This interpreter's concrete code representation
//

ConcreteCode *BootstrapInterpreter::Prepare(word abstractCode) {
  Assert(TagVal::FromWord(abstractCode)->GetTag() == Pickle::Function &&
	 TagVal::FromWord(abstractCode)->GetWidth() == 3);
  //--** block on all transients
  return ConcreteCode::New(abstractCode, this, 0);
}

//
// This interpreter uses the following stack frame layout:
//
//    0 to nargs - 1: arguments
//    #args + INTERPRETER_POS: Interpreter *interpreter
//    #args + PC_POS: TagVal *pc
//    #args + GLOBAL_ENV_POS: Vector *globalEnv
//    #args + LOCAL_ENV_POS: Environment *localEnv
//    #args + FORMAL_ARGS_POS: id args (to receive arguments)
//

static const u_int FRAME_SIZE = 5;
static const u_int INTERPRETER_POS = 0;
static const u_int PC_POS = 1;
static const u_int GLOBAL_ENV_POS = 2;
static const u_int LOCAL_ENV_POS = 3;
static const u_int FORMAL_ARGS_POS = 4;

void BootstrapInterpreter::PushCall(TaskStack *taskStack, word w) {
  Closure *closure = Closure::FromWord(w);
  ConcreteCode *concreteCode = closure->GetConcreteCode();
  Assert(concreteCode->GetInterpreter() == this);
  TagVal *function = TagVal::FromWord(concreteCode->GetAbstractCode());
  // datatype function = Function of int * idDef args * instr
  Assert(Store::WordToInt(function->Sel(0)) ==
	 closure->GetGlobalEnv()->GetLength());
  taskStack->PushFrame(FRAME_SIZE);
  taskStack->PutUnmanagedPointer(INTERPRETER_POS, this);
  taskStack->PutWord(PC_POS, function->Sel(2));
  taskStack->PutWord(GLOBAL_ENV_POS, closure->GetGlobalEnv()->ToWord());
  taskStack->PutWord(LOCAL_ENV_POS, Environment::New()->ToWord());
  taskStack->PutWord(FORMAL_ARGS_POS, function->Sel(1));
}

void BootstrapInterpreter::PopFrame(TaskStack *taskStack) {
  taskStack->PopFrame(FRAME_SIZE);
}

inline void PushState(TaskStack *taskStack,
		      Interpreter *interpreter, TagVal *pc, Vector *globalEnv,
		      Environment *localEnv, TagVal *formalArgs) {
  taskStack->PushFrame(FRAME_SIZE);
  taskStack->PutUnmanagedPointer(INTERPRETER_POS, interpreter);
  taskStack->PutWord(PC_POS, pc->ToWord());
  taskStack->PutWord(GLOBAL_ENV_POS, globalEnv->ToWord());
  taskStack->PutWord(LOCAL_ENV_POS, localEnv->ToWord());
  taskStack->PutWord(FORMAL_ARGS_POS, formalArgs->ToWord());
}

inline void PushState(TaskStack *taskStack,
		      Interpreter *interpreter, TagVal *pc, Vector *globalEnv,
		      Environment *localEnv) {
  TagVal *formalArgs = TagVal::New(Pickle::TupArgs, 1);
  formalArgs->Init(0, Vector::New(0)->ToWord());
  PushState(taskStack, interpreter, pc, globalEnv, localEnv, formalArgs);
}

#define SUSPEND(w) {						\
  PushState(taskStack, this, pc, globalEnv, localEnv);		\
  taskStack->PushFrame(1);					\
  taskStack->PutWord(0, w);					\
  return Result(Result::REQUEST, 1);				\
}

//
// Execution
//

//--** reading operands: FromWord tests for transients, but we don't
Interpreter::Result
BootstrapInterpreter::Run(TaskStack *taskStack, int nargs) {
  u_int nslots = nargs == -1? 1: nargs;
  Assert(Store::WordToUnmanagedPointer(taskStack->GetWord(nslots + INTERPRETER_POS)) == this);
  TagVal *pc = TagVal::FromWord(taskStack->GetWord(nslots + PC_POS));
  Vector *globalEnv =
    Vector::FromWord(taskStack->GetWord(nslots + GLOBAL_ENV_POS));
  Environment *localEnv =
    Environment::FromWord(taskStack->GetWord(nslots + LOCAL_ENV_POS));
  TagVal *formalArgs =
    TagVal::FromWord(taskStack->GetWord(nslots + FORMAL_ARGS_POS));
  // Calling convention conversion:
  switch (Pickle::GetArgs(formalArgs)) {
  case Pickle::OneArg:
    {
      word formalId = formalArgs->Sel(0);
      if (nargs == ~1) {
	localEnv->Add(formalId, taskStack->GetWord(0));
      } else if (nargs == 0) {
	localEnv->Add(formalId, Store::IntToWord(0)); // unit
      } else { // construct
	Tuple *tuple = Tuple::New(nslots);
	for (u_int i = nslots; i--; )
	  tuple->Init(i, taskStack->GetWord(i));
	localEnv->Add(formalId, tuple->ToWord());
      }
    }
    break;
  case Pickle::TupArgs:
    {
      Vector *formalIds = Vector::FromWord(formalArgs->Sel(0));
      if (nargs == ~1) { // deconstruct
	word suspendWord = taskStack->GetWord(0);
	Tuple *tuple = Tuple::FromWord(suspendWord);
	if (tuple == INVALID_POINTER) {
	  taskStack->PopFrame(1);
	  Closure::FromWord(GlobalPrimitives::Future_await)->
	    PushCall(taskStack);
	  taskStack->PushFrame(1);
	  taskStack->PutWord(0, suspendWord);
	  return Result(Result::CONTINUE, 1);
	}
	Assert(tuple->GetWidth() == nargs);
	for (u_int i = nargs; i--; )
	  localEnv->Add(formalIds->Sub(i), tuple->Sel(i));
      } else {
	Assert(formalIds->GetLength() == nargs);
	for (u_int i = nargs; i--; )
	  localEnv->Add(formalIds->Sub(i), taskStack->GetWord(i));
      }
    }
    break;
  };
  taskStack->PopFrame(nslots + FRAME_SIZE);
  while (!(Scheduler::TestPreempt() || Store::NeedGC())) {
  loop:
    switch (Pickle::GetInstr(pc)) {
    case Pickle::PutConst: // of id * value * instr
      {
	localEnv->Add(pc->Sel(0), pc->Sel(1));
	pc = TagVal::FromWord(pc->Sel(2));
      }
      break;
    case Pickle::PutNew: // of id * instr
      {
	localEnv->Add(pc->Sel(0), Constructor::New()->ToWord());
	pc = TagVal::FromWord(pc->Sel(1));
      }
      break;
    case Pickle::PutGlobal: // of id * int * instr
      {
	localEnv->Add(pc->Sel(0),
		      globalEnv->Sub(Store::WordToInt(pc->Sel(1))));
	pc = TagVal::FromWord(pc->Sel(2));
      }
      break;
    case Pickle::PutTag: // of id * int * id vector * instr
      {
	Vector *ids = Vector::FromWord(pc->Sel(2));
	u_int nargs = ids->GetLength();
	TagVal *tagVal = TagVal::New(Store::WordToInt(pc->Sel(1)), nargs);
	for (u_int i = nargs; i--; )
	  tagVal->Init(i, localEnv->Lookup(ids->Sub(i)));
	localEnv->Add(pc->Sel(0), tagVal->ToWord());
	pc = TagVal::FromWord(pc->Sel(3));
      }
      break;
    case Pickle::PutCon: // of id * con * id vector * instr
      {
	Vector *ids = Vector::FromWord(pc->Sel(2));
	u_int nargs = ids->GetLength();
	TagVal *conBlock = TagVal::FromWord(pc->Sel(1));
	word suspendWord;
	switch (Pickle::GetCon(conBlock)) {
	case Pickle::Con:
	  suspendWord = localEnv->Lookup(conBlock->Sel(0));
	  break;
	case Pickle::StaticCon:
	  suspendWord = conBlock->Sel(0);
	  break;
	}
	Constructor *constructor = Constructor::FromWord(suspendWord);
	if (constructor == INVALID_POINTER) SUSPEND(suspendWord);
	ConVal *conVal = ConVal::New(constructor, nargs);
	for (u_int i = nargs; i--; )
	  conVal->Init(i, localEnv->Lookup(ids->Sub(i)));
	localEnv->Add(pc->Sel(0), conVal->ToWord());
	pc = TagVal::FromWord(pc->Sel(3));
      }
      break;
    case Pickle::PutRef: // of id * id * instr
      {
	word contents = localEnv->Lookup(pc->Sel(1));
	localEnv->Add(pc->Sel(0), Cell::New(contents)->ToWord());
	pc = TagVal::FromWord(pc->Sel(2));
      }
      break;
    case Pickle::PutTup: // of id * id vector * instr
      {
	Vector *ids = Vector::FromWord(pc->Sel(1));
	u_int nargs = ids->GetLength();
	Tuple *tuple = Tuple::New(nargs);
	for (u_int i = nargs; i--; )
	  tuple->Init(i, localEnv->Lookup(ids->Sub(i)));
	localEnv->Add(pc->Sel(0), tuple->ToWord());
	pc = TagVal::FromWord(pc->Sel(2));
      }
      break;
    case Pickle::PutSel: // of id * int * id * instr
      {
	word suspendWord = localEnv->Lookup(pc->Sel(2));
	Tuple *tuple = Tuple::FromWord(suspendWord);
	if (tuple == INVALID_POINTER) SUSPEND(suspendWord);
	localEnv->Add(pc->Sel(0), tuple->Sel(Store::WordToInt(pc->Sel(1))));
	pc = TagVal::FromWord(pc->Sel(3));
      }
      break;
    case Pickle::PutVec: // of id * id vector * instr
      {
	Vector *ids = Vector::FromWord(pc->Sel(1));
	u_int nargs = ids->GetLength();
	Vector *vector = Vector::New(nargs);
	for (u_int i = nargs; i--; )
	  vector->Init(i, localEnv->Lookup(ids->Sub(i)));
	localEnv->Add(pc->Sel(0), vector->ToWord());
	pc = TagVal::FromWord(pc->Sel(2));
      }
      break;
    case Pickle::PutFun: // of id * id vector * function * instr
      {
	Vector *ids = Vector::FromWord(pc->Sel(1));
	u_int nglobals = ids->GetLength();
	Vector *newGlobalEnv = Vector::New(nglobals);
	for (u_int i = nglobals; i--; )
	  newGlobalEnv->Init(i, localEnv->Lookup(ids->Sub(i)));
	Closure *closure = Closure::New(Prepare(pc->Sel(2)), newGlobalEnv);
	localEnv->Add(pc->Sel(0), closure->ToWord());
	pc = TagVal::FromWord(pc->Sel(3));
      }
      break;
    case Pickle::Kill: // of id vector * instr
      {
	Vector *kills = Vector::FromWord(pc->Sel(0));
	for (u_int i = kills->GetLength(); i--; )
	  localEnv->Kill(kills->Sub(i));
	pc = TagVal::FromWord(pc->Sel(1));
      }
      break;
    case Pickle::AppPrim: // of id * string * id vector * instr
      {
	// Save our state for return:
	Vector *formalIds = Vector::New(1);
	formalIds->Init(0, pc->Sel(0));
	TagVal *formalArgs = TagVal::New(Pickle::TupArgs, 1);
	formalArgs->Init(0, formalIds->ToWord());
	PushState(taskStack, this, TagVal::FromWord(pc->Sel(3)),
		  globalEnv, localEnv, formalArgs);
	// Push a call frame for the primitive:
	const char *name = String::FromWord(pc->Sel(1))->GetValue();
	Closure::FromWord(Primitive::Lookup(name))->PushCall(taskStack);
	Vector *actualIds = Vector::FromWord(pc->Sel(2));
	u_int nargs = actualIds->GetLength();
	taskStack->PushFrame(nargs);
	for (u_int i = nargs; i--; )
	  taskStack->PutWord(i, localEnv->Lookup(actualIds->Sub(i)));
	return Result(Result::CONTINUE, nargs);
      }
      break;
    case Pickle::AppVar: // of id args * id * id args * instr
      {
	word suspendWord = localEnv->Lookup(pc->Sel(1));
	Closure *closure = Closure::FromWord(suspendWord);
	if (closure == INVALID_POINTER) SUSPEND(suspendWord);
	PushState(taskStack, this, TagVal::FromWord(pc->Sel(3)),
		  globalEnv, localEnv, TagVal::FromWord(pc->Sel(0)));
	closure->PushCall(taskStack);
	TagVal *actualArgs = TagVal::FromWord(pc->Sel(2));
	switch (Pickle::GetArgs(actualArgs)) {
	case Pickle::OneArg:
	  taskStack->PushFrame(1);
	  taskStack->PutWord(0, localEnv->Lookup(actualArgs->Sel(0)));
	  return Result(Result::CONTINUE, -1);
	case Pickle::TupArgs:
	  {
	    Vector *actualIds = Vector::FromWord(actualArgs->Sel(0));
	    u_int nargs = actualIds->GetLength();
	    taskStack->PushFrame(nargs);
	    for (u_int i = nargs; i--; )
	      taskStack->PutWord(i, localEnv->Lookup(actualIds->Sub(i)));
	    return Result(Result::CONTINUE, nargs);
	  }
	}
      }
      break;
    case Pickle::GetTup: // of idDef vector * id * instr
      {
	word suspendWord = localEnv->Lookup(pc->Sel(1));
	Tuple *tuple = Tuple::FromWord(suspendWord);
	if (tuple == INVALID_POINTER) SUSPEND(suspendWord);
	Vector *idDefs = Vector::FromWord(pc->Sel(0));
	Assert(tuple->GetWidth() == idDefs->GetLength());
	for (u_int i = idDefs->GetLength(); i--; ) {
	  TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
	  if (idDef != INVALID_POINTER) // SOME id
	    localEnv->Add(idDef->Sel(0), tuple->Sel(i));
	}
	pc = TagVal::FromWord(pc->Sel(2));
      }
      break;
    case Pickle::IntTest: // of id * (int * instr) vector * instr
      {
	word suspendWord = localEnv->Lookup(pc->Sel(0));
	int value = Store::WordToInt(suspendWord);
	if (value == INVALID_INT) SUSPEND(suspendWord);
	Vector *tests = Vector::FromWord(pc->Sel(1));
	u_int ntests = tests->GetLength();
	for (u_int i = 0; i < ntests; i++) {
	  Tuple *pair = Tuple::FromWord(tests->Sub(i));
	  if (Store::WordToInt(pair->Sel(0)) == value) {
	    pc = TagVal::FromWord(pair->Sel(1));
	    goto loop;
	  }
	}
	pc = TagVal::FromWord(pc->Sel(2));
      }
      break;
    case Pickle::RealTest: // of id * (real * instr) vector * instr
      {
	word suspendWord = localEnv->Lookup(pc->Sel(0));
	Real *real = Real::FromWord(suspendWord);
	if (real == INVALID_POINTER) SUSPEND(suspendWord);
	double value = real->GetValue();
	Vector *tests = Vector::FromWord(pc->Sel(1));
	u_int ntests = tests->GetLength();
	for (u_int i = 0; i < ntests; i++) {
	  Tuple *pair = Tuple::FromWord(tests->Sub(i));
	  if (Real::FromWord(pair->Sel(0))->GetValue() == value) {
	    pc = TagVal::FromWord(pair->Sel(1));
	    goto loop;
	  }
	}
	pc = TagVal::FromWord(pc->Sel(2));
      }
      break;
    case Pickle::StringTest: // of id * (string * instr) vector * instr
      //--** StringTest not implemented
      break;
    case Pickle::TagTest:
      // of id * (int * instr) vector
      //       * (int * idDef vector * instr) vector * instr
      {
	word suspendWord = localEnv->Lookup(pc->Sel(0));
	TagVal *tagVal = TagVal::FromWord(suspendWord);
	if (tagVal == INVALID_POINTER) { // nullary constructor or transient
	  int tag = Store::WordToInt(suspendWord);
	  if (tag == INVALID_INT) SUSPEND(suspendWord);
	  Vector *tests = Vector::FromWord(pc->Sel(1));
	  u_int ntests = tests->GetLength();
	  for (u_int i = 0; i < ntests; i++) {
	    Tuple *pair = Tuple::FromWord(tests->Sub(i));
	    if (Store::WordToInt(pair->Sel(0)) == tag) {
	      pc = TagVal::FromWord(pair->Sel(1));
	      goto loop;
	    }
	  }
	} else { // non-nullary constructor
	  int tag = tagVal->GetTag();
	  Vector *tests = Vector::FromWord(pc->Sel(2));
	  u_int ntests = tests->GetLength();
	  for (u_int i = 0; i < ntests; i++) {
	    Tuple *triple = Tuple::FromWord(tests->Sub(i));
	    if (Store::WordToInt(triple->Sel(0)) == tag) {
	      Vector *idDefs = Vector::FromWord(triple->Sel(1));
	      Assert(tagVal->GetWidth() == idDefs->GetLength());
	      for (u_int i = idDefs->GetLength(); i--; ) {
		TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
		if (idDef != INVALID_POINTER) // SOME id
		  localEnv->Add(idDef->Sel(0), tagVal->Sel(i));
	      }
	      pc = TagVal::FromWord(triple->Sel(2));
	      goto loop;
	    }
	  }
	}
	pc = TagVal::FromWord(pc->Sel(3));
      }
      break;
    case Pickle::ConTest:
      // of id * (con * instr) vector
      //       * (con * idDef vector * instr) vector * instr
      {
	word suspendWord = localEnv->Lookup(pc->Sel(0));
	ConVal *conVal = ConVal::FromWord(suspendWord);
	if (conVal == INVALID_POINTER) SUSPEND(suspendWord);
	if (conVal->IsConVal()) { // non-nullary constructor
	  Constructor *constructor = conVal->GetConstructor();
	  Vector *tests = Vector::FromWord(pc->Sel(2));
	  u_int ntests = tests->GetLength();
	  for (u_int i = 0; i < ntests; i++) {
	    Tuple *triple = Tuple::FromWord(tests->Sub(i));
	    TagVal *conBlock = TagVal::FromWord(triple->Sel(0));
	    switch (Pickle::GetCon(conBlock)) {
	    case Pickle::Con:
	      suspendWord = localEnv->Lookup(conBlock->Sel(0));
	      break;
	    case Pickle::StaticCon:
	      suspendWord = conBlock->Sel(0);
	      break;
	    }
	    Constructor *testConstructor = Constructor::FromWord(suspendWord);
	    if (testConstructor == INVALID_POINTER) SUSPEND(suspendWord);
	    if (testConstructor == constructor) {
	      Vector *idDefs = Vector::FromWord(triple->Sel(1));
	      Assert(conVal->GetWidth() == idDefs->GetLength());
	      for (u_int i = idDefs->GetLength(); i--; ) {
		TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
		if (idDef != INVALID_POINTER) // SOME id
		  localEnv->Add(idDef->Sel(0), conVal->Sel(i));
	      }
	      pc = TagVal::FromWord(triple->Sel(2));
	      goto loop;
	    }
	  }
	} else { // nullary constructor
	  Constructor *constructor = reinterpret_cast<Constructor *>(conVal);
	  Vector *tests = Vector::FromWord(pc->Sel(1));
	  u_int ntests = tests->GetLength();
	  for (u_int i = 0; i < ntests; i++) {
	    Tuple *pair = Tuple::FromWord(tests->Sub(i));
	    TagVal *conBlock = TagVal::FromWord(pair->Sel(0));
	    switch (Pickle::GetCon(conBlock)) {
	    case Pickle::Con:
	      suspendWord = localEnv->Lookup(conBlock->Sel(0));
	      break;
	    case Pickle::StaticCon:
	      suspendWord = conBlock->Sel(0);
	      break;
	    }
	    Constructor *testConstructor = Constructor::FromWord(suspendWord);
	    if (testConstructor == INVALID_POINTER) SUSPEND(suspendWord);
	    if (testConstructor == constructor) {
	      pc = TagVal::FromWord(pair->Sel(1));
	      goto loop;
	    }
	  }
	}
	pc = TagVal::FromWord(pc->Sel(3));
      }
      break;
    case Pickle::VecTest: // of id * (idDef vector * instr) vector * instr
      {
	word suspendWord = localEnv->Lookup(pc->Sel(0));
	Vector *vector = Vector::FromWord(suspendWord);
	if (vector == INVALID_POINTER) SUSPEND(suspendWord);
	int value = vector->GetLength();
	Vector *tests = Vector::FromWord(pc->Sel(1));
	u_int ntests = tests->GetLength();
	for (u_int i = 0; i < ntests; i++) {
	  Tuple *pair = Tuple::FromWord(tests->Sub(i));
	  Vector *idDefs = Vector::FromWord(pair->Sel(0));
	  if (idDefs->GetLength() == value) {
	    for (u_int i = value; i--; ) {
	      TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
	      if (idDef != INVALID_POINTER) // SOME id
		localEnv->Add(idDef->Sel(0), vector->Sub(i));
	    }
	    pc = TagVal::FromWord(pair->Sel(1));
	    goto loop;
	  }
	}
	pc = TagVal::FromWord(pc->Sel(2));
      }
      break;
    case Pickle::Return: // of id args
      {
	TagVal *returnArgs = TagVal::FromWord(pc->Sel(0));
	switch (Pickle::GetArgs(returnArgs)) {
	case Pickle::OneArg:
	  taskStack->PushFrame(1);
	  taskStack->PutWord(0, localEnv->Lookup(returnArgs->Sel(0)));
	  return Result(Result::CONTINUE, -1);
	case Pickle::TupArgs:
	  {
	    Vector *returnIds = Vector::FromWord(returnArgs->Sel(0));
	    u_int nargs = returnIds->GetLength();
	    taskStack->PushFrame(nargs);
	    for (u_int i = nargs; i--; )
	      taskStack->PutWord(i, localEnv->Lookup(returnIds->Sub(i)));
	    return Result(Result::CONTINUE, nargs);
	  }
	}
      }
      break;
    }
  }
  PushState(taskStack, this, pc, globalEnv, localEnv);
  return Result(Result::PREEMPT, 0);
}
