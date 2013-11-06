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
#include "AliceConcreteCode.hh"

class AbstractCodeFrame: private StackFrame {
protected:
  enum { PC_POS, COORD_POS, CLOSURE_POS, FORMAL_ARGS_POS, SIZE_POS, BASE_SIZE };

public:
  using StackFrame::Clone;
  
  u_int GetSize() {
    return Store::DirectWordToInt(GetArg(SIZE_POS));
  }
  
  bool IsHandlerFrame() {
    return false; // to be done
  }
  
  TagVal *GetPC() {
    return TagVal::FromWordDirect(GetArg(PC_POS));
  }
  
  void SetPC(word pc) {
    ReplaceArg(PC_POS, pc);
  }
  
  void SetPC(TagVal *pc) {
    ReplaceArg(PC_POS, pc->ToWord());
  }
  
  word GetCoord() {
    return GetArg(COORD_POS);
  }

  /**
   * coord should be either a Tuple or Store::IntToWord(0)
   */
  void SetCoord(word coord) {
    ReplaceArg(COORD_POS, coord);
  }

  Closure *GetClosure() {
    return Closure::FromWordDirect(GetArg(CLOSURE_POS));
  }
  
  /**
   * formalArgs may be a single IdDef or an IdDef Vector
   */
  
  void SetFormalArgs(Vector *ids) {
    SetFormalArgs(ids->ToWord());
  }
  
  void SetFormalArgs(word formalArgs) {
    ReplaceArg(FORMAL_ARGS_POS, formalArgs);
  }
  
  word GetFormalArgs() {
    return GetArg(FORMAL_ARGS_POS);
  }
  
  word GetLocal(word id) {
    return GetLocal(Store::WordToInt(id));
  }
  
  word GetLocal(u_int id);
  
  void SetLocal(word id, word value) {
    SetLocal(Store::WordToInt(id), value);
  }
  
  void SetLocal(u_int id, word value) {
    Assert(id >= 0 && id < GetSize() - BASE_SIZE - StackFrame::GetBaseSize());
    ReplaceArg(BASE_SIZE + id, value);
  }
  
  void KillLocal(word id, TagVal *pc, Closure *closure) {
    KillLocal(Store::WordToInt(id), pc, closure);
  }
  
  void KillLocal(u_int id, TagVal *pc, Closure *closure);
  
  static AbstractCodeFrame *New(AliceConcreteCode *acc, Closure *closure) {
    TagVal *abstractCode = acc->GetAbstractCode();
    
    u_int nbLocals = acc->GetNLocals();
    u_int frSize = BASE_SIZE + nbLocals;
    NEW_STACK_FRAME(frame, AbstractCodeInterpreter::self, frSize);
    frame->InitArg(PC_POS, abstractCode->Sel(5));
    frame->InitArg(COORD_POS, Store::IntToWord(0));
    frame->InitArg(CLOSURE_POS, closure->ToWord());
    frame->InitArg(FORMAL_ARGS_POS, abstractCode->Sel(3));
    frame->InitArg(SIZE_POS, StackFrame::GetBaseSize() + frSize);
    frame->InitArgs(BASE_SIZE, nbLocals, AliceLanguageLayer::undefinedValue);
    return static_cast<AbstractCodeFrame *>(frame);
  }
  
  static AbstractCodeFrame *New(Closure *closure) {
    AbstractCodeFrame::New(AliceConcreteCode::FromWord(closure->GetConcreteCode()), closure);
  }
  
#ifdef DEBUG_CHECK
  static void Init();
#endif

};
#endif
