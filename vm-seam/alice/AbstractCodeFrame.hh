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

#ifdef DEBUG_CHECK
static word dead;
#endif

#ifdef LIVENESS_DEBUG
static const BlockLabel DEAD_LABEL = HASHNODE_LABEL;

static void DisassembleAlice(Closure *closure) {
  AliceConcreteCode *concreteCode =
    AliceConcreteCode::FromWord(closure->GetConcreteCode());
  concreteCode->Disassemble(stderr);
}
#endif

// AbstractCodeInterpreter StackFrames
class AbstractCodeFrame: public StackFrame {
protected:
  enum { PC_POS, CLOSURE_POS, LOCAL_ENV_POS, FORMAL_ARGS_POS, SIZE };
public:
  // Local Environment
  class Environment: private Array {
  public:
    using Array::ToWord;
    // Environment Accessors
    void Add(word id, word value) {
      Update(Store::WordToInt(id), value);
    }
    word Lookup(word id) {
      word value = Sub(Store::WordToInt(id));
#ifdef LIVENESS_DEBUG
      Block *p = Store::WordToBlock(value);
      if (p != INVALID_POINTER) {
	if (p->GetLabel() == DEAD_LABEL) {
	  std::fprintf(stderr, "### USING KILLED VALUE ###\n");
	  std::fprintf(stderr, "### killed as Local(%d)\n",
		       Store::DirectWordToInt(p->GetArg(0)));
	  std::fprintf(stderr, "### value before kill:\n");
	  Debug::Dump(p->GetArg(1));
	  std::fprintf(stderr, "### killed at pc=%p in function:\n",
		       TagVal::FromWordDirect(p->GetArg(2)));
	  DisassembleAlice(Closure::FromWordDirect(p->GetArg(3)));
	  return p->GetArg(1);
	}
      }
#else
      Assert(value != dead);
#endif
      return value;
    }
#ifdef LIVENESS_DEBUG
    void Kill(word id, TagVal *pc, Closure *globalEnv) {
      Block *dead = Store::AllocBlock(DEAD_LABEL, 4);
      dead->InitArg(0, id);
      dead->InitArg(1, Sub(Store::WordToInt(id)));
      dead->InitArg(2, pc->ToWord());
      dead->InitArg(3, globalEnv->ToWord());
      Update(Store::WordToInt(id), dead->ToWord());
    }
#else
    void Kill(word id, TagVal *, Closure *) {
#ifdef DEBUG_CHECK
      Update(Store::WordToInt(id), dead);
#else
      Update(Store::WordToInt(id), Store::IntToWord(0));
#endif
    }
#endif
    // Environment Constructor
    static Environment *New(u_int size) {
      Array *array = Array::New(size);
      for(int index = size; index--; ) {
	array->Init(index, AliceLanguageLayer::undefinedValue);
      }
      return STATIC_CAST(Environment *, array);
    }
    // Environment Untagging
    static Environment *FromWordDirect(word x) {
      return STATIC_CAST(Environment *, Array::FromWordDirect(x));
    }
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
  static void Init() {
    dead = String::New("UNINITIALIZED OR DEAD")->ToWord();
    RootSet::Add(dead);
  }
#endif

};
#endif
