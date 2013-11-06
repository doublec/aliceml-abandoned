//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __ALICE__LAZY_SEL_INTERPRETER_HH__
#define __ALICE__LAZY_SEL_INTERPRETER_HH__

#if defined(INTERFACE)
#pragma interface "alice/LazySelInterpreter.hh"
#endif

#include "alice/Base.hh"

class LazySelInterpreter: public Interpreter {
private:
  LazySelInterpreter(): Interpreter() {}
public:
  static LazySelInterpreter *self;
  static word concreteCode;

  static void Init();
  
  /**
   * Dereference the specified word (i.e. skip over reference-transients),
   * and if there are by-needs with LazySelClosure in the way, and they
   * are selecting from a non-transient Record, mutate the by-needs into
   * reference-transients and allow the dereference to pass through them.
   */
  static word Deref(word w);

  virtual u_int GetFrameSize(StackFrame *sFrame);
  virtual Result Run(StackFrame *sFrame);
  virtual u_int GetInArity(ConcreteCode *concreteCode);
  virtual u_int GetOutArity(ConcreteCode *concreteCode);
  virtual void PushCall(Closure *closure);
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame, std::ostream& out);

#if PROFILE
  virtual word GetProfileKey(StackFrame *);
  virtual word GetProfileKey(ConcreteCode *);
#endif
};

class LazySelClosure: public Closure {
public:
  enum { RECORD_POS, LABEL_POS, SIZE };

  static LazySelClosure *New(word record, UniqueString *label) {
    Closure *closure = Closure::New(LazySelInterpreter::concreteCode, SIZE);
    closure->Init(RECORD_POS, record);
    closure->Init(LABEL_POS, label->ToWord());
    return static_cast<LazySelClosure *>(closure);
  }

  word GetRecord() {
    return Sub(RECORD_POS);
  }
  UniqueString *GetLabel() {
    return UniqueString::FromWordDirect(Sub(LABEL_POS));
  }
};

#endif
