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

#ifndef __INTERPRETER_HH__
#define __INTERPRETER_HH__

#include "store/store.hh"

class Thread;
class TaskStack;

class Interpreter {
public:
  typedef enum {
    CONTINUE, EXCEPTION, PREEMPT, SUSPEND, TERMINATE
  } result;

  virtual result Run(int nargs, TaskStack *&taskStack, word &data) = 0;
  virtual Thread *NewThread(word code) = 0;
};

#endif __INTERPRETER_HH__
