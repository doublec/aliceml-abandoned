//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "emulator/TaskStack.hh"
#endif

#include <cstdio>
#include "emulator/RootSet.hh"
#include "emulator/TaskStack.hh"
#include "emulator/Interpreter.hh"
#include "emulator/PushCallInterpreter.hh"
#include "emulator/Scheduler.hh"
#include "emulator/ConcreteCode.hh"

typedef union {
  Transient *pt;
  Chunk *pc;
  Block *pb;
  int pi;
} word_data;

static const char *TransLabel(BlockLabel l) {
  switch (l) {
  case HOLE_LABEL:
    return "HOLE";
  case FUTURE_LABEL:
    return "FUTURE";
  case REF_LABEL:
    return "REF";
  case CANCELLED_LABEL:
    return "CANCELLED";
  case BYNEED_LABEL:
    return "BYNEED";
  case HASHTABLE_LABEL:
    return "HASHTABBLE";
  case QUEUE_LABEL:
    return "QUEUE";
  case STACK_LABEL:
    return "STACK";
  case THREAD_LABEL:
    return "THREAD";
  case TUPLE_LABEL:
    return "TUPLE";
  case EMPTYARG_LABEL:
    return "EMPTYARG";
  case ONEARG_LABEL:
    return "ONEARG_LABEL";
  case TUPARGS_LABEL:
    return "TUPARGS_LABEL";
  default:
    return "UNKNOWN";
  }
}

static void Print(Chunk *c) {
  fprintf(stderr, "'%.*s'\n", (int) c->GetSize(), c->GetBase());
}

static void PerformDump(word x, u_int index, u_int level) {
  word_data w;
  if ((w.pt = Store::WordToTransient(x)) != INVALID_POINTER) {
    fprintf(stderr, "%*c(%d):TRANSIENT(%s)\n", level, ' ',
	    index, TransLabel(w.pb->GetLabel()));
    level += 2;
    PerformDump(w.pb->GetArg(0), 0, level);
    level -= 2;
    fprintf(stderr, "%*cENDTRANSIENT\n", level , ' ');
  }
  else if ((w.pc = Store::WordToChunk(x)) != INVALID_POINTER) {
    fprintf(stderr, "%*c(%d):CHUNK(%d)=", level, ' ',
	    index, w.pc->GetSize());
    Print(w.pc);
  }
  else if ((w.pb = Store::WordToBlock(x)) != INVALID_POINTER) {
    u_int size = w.pb->GetSize();
    fprintf(stderr, "%*c(%d):BLOCK(%s=%d, %d)\n", level, ' ',
	    index, TransLabel(w.pb->GetLabel()),
	    w.pb->GetLabel(), size);
    level += 2;
    for (u_int i = 0; i < size; i++) {
      PerformDump(w.pb->GetArg(i), i, level);
    }
    level -= 2;
    fprintf(stderr, "%*cENDBLOCK\n", level , ' ');
  }
  // Assume Int
  else {
    w.pi = Store::WordToInt(x);
    fprintf(stderr, "%*c(%d):INT=%d\n", level, ' ', index, w.pi);
  }
}

void TaskStack::Dump(word x) {
  PerformDump(x, 0, 0);
}

// Empty Interpreter
class EmptyTaskInterpreter : public Interpreter {
public:
  // EmptyTaskInterpreter Constructor
  EmptyTaskInterpreter() : Interpreter() {}
  // Execution
  virtual Result Run(word, TaskStack *);
  virtual Result Handle(word, word, TaskStack *);
  // Debugging
  virtual const char *Identify();
  virtual const char *ToString(word, TaskStack *);
};

Interpreter::Result EmptyTaskInterpreter::Handle(word exn, word, TaskStack *) {
  //--** output information about the unhandled exception
  fprintf(stderr, "uncaught exception:\n");
  TaskStack::Dump(exn);
  return Interpreter::TERMINATE;
}

Interpreter::Result EmptyTaskInterpreter::Run(word, TaskStack *) {
  return Interpreter::TERMINATE;
}

const char *EmptyTaskInterpreter::Identify() {
  return "EmptyTaskInterpreter";
}

const char *EmptyTaskInterpreter::ToString(word, TaskStack *) {
  return "EmptyTaskInterpreter::ToString";
}

word TaskStack::emptyTask;

void TaskStack::Init() {
  Interpreter *interpreter = new EmptyTaskInterpreter();
  StackFrame *frame = StackFrame::New(PRIMITIVE_FRAME, interpreter);
  emptyTask = frame->ToWord();
  RootSet::Add(emptyTask);
}

// Core PushCall Function
Interpreter::Result TaskStack::PushCall(word closure) {
  Transient *transient = Store::WordToTransient(closure);
  // Found Closure
  if (transient == INVALID_POINTER) {
    Closure *cl = Closure::FromWord(closure);
    word code = cl->GetConcreteCode();
    transient = Store::WordToTransient(code);
    // Found Code Block
    if (transient == INVALID_POINTER) {
      ConcreteCode *cc = ConcreteCode::FromWord(code);
      cc->GetInterpreter()->PushCall(this, cl);
      return Interpreter::CONTINUE;
    }
    // Code not yet available
    else {
      // Create CallFrame on top
      PushCallInterpreter::PushFrame(this, closure);
      Scheduler::currentData = transient->ToWord();
      return Interpreter::REQUEST;
    }
  }
  // Need to wait for closure
  else {
    Scheduler::currentData = transient->ToWord();
    // Create CallFrame on top
    PushCallInterpreter::PushFrame(this, Scheduler::currentData);
    return Interpreter::REQUEST;
  }
}
