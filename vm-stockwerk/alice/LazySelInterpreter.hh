//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __ALICE__LAZY_SEL_INTERPRETER_HH__
#define __ALICE__LAZY_SEL_INTERPRETER_HH__

#if defined(INTERFACE)
#pragma implementation "alice/LazySelInterpreter.hh"
#endif

#include "generic/Interpreter.hh"
#include "generic/Closure.hh"
#include "alice/Data.hh"

class LazySelInterpreter: public Interpreter {
public:
  // Exported LazySelInterpreter Instance
  static LazySelInterpreter *self;
  // LazySelInterpreter Constructor
  LazySelInterpreter(): Interpreter() {}
  // LazySelInterpreter Static Constructor
  static void Init() {
    self = new LazySelInterpreter();
  }
  // Frame Handling
  static void PushFrame(word record, UniqueString *label);
  virtual void PushCall(Closure *closure);
  // Execution
  virtual Result Run();
  // Debugging
  virtual const char *Identify();
  virtual void DumpFrame(word frame);
};

class LazySelClosure: public Closure {
public:
  static LazySelClosure *New(word tuple, UniqueString *label);
};

#endif
