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

#include "store/store.hh"
#include "scheduler/TaskStack.hh"
#include "scheduler/Interpreter.hh"
#include "builtins/GlobalPrimitives.hh"

#define TASK_STACK_INITIAL_SIZE 8 /* words */

const BlockLabel taskStackLabel = Store::MakeLabel(0); //--** use sth sensible

TaskStack *TaskStack::New() {
  Block *b = Store::AllocBlock(taskStackLabel, TASK_STACK_INITIAL_SIZE);
  b->InitArg(1, Store::IntToWord(0));
  return static_cast<TaskStack *>(b);
}

//--** void TaskStack::PushFrame(u_int size);

//--** void TaskStack::PopFrame(u_int size);

//--** bool TaskStack::IsEmpty();

//--** void TaskStack::PutWord(u_int offset, word v);

//--** word TaskStack::GetWord(u_int offset);
