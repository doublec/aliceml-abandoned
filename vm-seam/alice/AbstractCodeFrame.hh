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

#ifndef __ALICE__ABSTRACT_CODE_FRAME_HH__
#define __ALICE__ABSTRACT_CODE_FRAME_HH__

#if defined(INTERFACE)
#pragma interface "alice/AbstractCodeFrame.hh"
#endif

#include "alice/Data.hh"
#include "alice/AbstractCode.hh"
#include "alice/AliceLanguageLayer.hh"

// AbstractCodeInterpreter StackFrames
class AbstractCodeFrame: public StackFrame {
protected:
  enum { PC_POS, CLOSURE_POS, LOCAL_ENV_POS, FORMAL_ARGS_POS, SIZE };
public:
  class Environment : private Array {
  public:
    using Array::ToWord;
    void Add(word id, word value);
#if DEBUGGER
    word LookupUnchecked(word id);
#endif
    word Lookup(word id);
    void Kill(word id, TagVal *pc, Closure *globalEnv);
    static Environment *New(u_int size);
    static Environment *FromWordDirect(word x);
  };
  // AbstractCodeFrame Accessors
  u_int GetSize() {
    return StackFrame::GetSize() + SIZE;
  }
  bool IsHandlerFrame() {
    return false; // to be done
  }
  TagVal *GetPC() {
    return TagVal::FromWordDirect(StackFrame::GetArg(PC_POS));
  }
  void SetPC(TagVal *pc) {
    StackFrame::ReplaceArg(PC_POS, pc->ToWord());
  }
  Closure *GetClosure() {
    return Closure::FromWordDirect(StackFrame::GetArg(CLOSURE_POS));
  }
  Environment *GetLocalEnv() {
    return Environment::FromWordDirect(StackFrame::GetArg(LOCAL_ENV_POS));
  }
  Vector *GetFormalArgs() {
    return Vector::FromWord(StackFrame::GetArg(FORMAL_ARGS_POS));
  }
  void SetFormalArgs(word formalArgs) {
    StackFrame::ReplaceArg(FORMAL_ARGS_POS, formalArgs);
  }
  // AbstractCodeFrame Constructor
  static AbstractCodeFrame *New(Interpreter *interpreter,
				word pc,
				Closure *closure,
				Environment *env,
				word formalArgs) {
    NEW_STACK_FRAME(frame, interpreter, SIZE);
    frame->InitArg(PC_POS, pc);
    frame->InitArg(CLOSURE_POS, closure->ToWord());
    frame->InitArg(LOCAL_ENV_POS, env->ToWord());
    frame->InitArg(FORMAL_ARGS_POS, formalArgs);
    return STATIC_CAST(AbstractCodeFrame *, frame);
  }
#ifdef DEBUG_CHECK
  static void Init();
#endif

};
#endif
