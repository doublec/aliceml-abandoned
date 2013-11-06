//
// Author:
//   Christian Mueller <cmueller@ps.uni-sb.de>
//
// Copyright:
//   Christian Mueller, 2005
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __ALICE_BYTE_CODE_FRAME__
#define __ALICE_BYTE_CODE_FRAME__

#if defined(INTERFACE)
#pragma interface "alice/ByteCodeFrame.hh"
#endif

#include "alice/Base.hh"
#include "alice/ByteCodeInterpreter.hh"
#include "alice/ByteConcreteCode.hh"


class AliceDll ByteCodeFrame : private StackFrame {
protected:
  enum { CONCRETE_CODE_POS, PC_POS, CP_POS, SIZE_POS, BASE_SIZE };
public:
  using StackFrame::Clone;

  u_int GetSize() {
    return Store::DirectWordToInt(StackFrame::GetArg(SIZE_POS));
  }
  
  u_int GetPC() {
    return Store::DirectWordToInt(StackFrame::GetArg(PC_POS));
  }
  
  Tuple *GetIP() {
    return GetConcreteCode()->GetImmediateArgs();
  }
  
  Closure *GetCP() {
    return Closure::FromWordDirect(StackFrame::GetArg(CP_POS));
  }
  
  ByteConcreteCode *GetConcreteCode() {
    return ByteConcreteCode::FromWordDirect(GetArg(CONCRETE_CODE_POS));
  }
  
  TagVal *GetAbstractCode() {
    return GetConcreteCode()->GetAbstractCode();
  }
  
  word GetLocal(u_int index) {
    Assert(index >= 0 && index < GetSize() - BASE_SIZE - StackFrame::GetBaseSize());
    return StackFrame::GetArg(BASE_SIZE + index);
  }
  
  void SetLocal(u_int index, word w) {
    Assert(index >= 0 && index < GetSize() - BASE_SIZE - StackFrame::GetBaseSize());
    StackFrame::ReplaceArg(BASE_SIZE + index, w);
  }
  
  void SavePC(u_int pc) {
    StackFrame::ReplaceArg(PC_POS, Store::IntToWord(pc));
  }
    
  static ByteCodeFrame *New(ByteConcreteCode *bcc, Closure *closure) {
    u_int nbLocals = bcc->GetNLocals();
    u_int frSize = BASE_SIZE + nbLocals;
    NEW_STACK_FRAME(frame, ByteCodeInterpreter::self, frSize);
    frame->InitArg(CONCRETE_CODE_POS, bcc->ToWord());
    frame->InitArg(PC_POS, Store::IntToWord(0));
    frame->InitArg(CP_POS, closure->ToWord());
    frame->InitArg(SIZE_POS, StackFrame::GetBaseSize() + frSize);
    frame->InitArgs(BASE_SIZE, nbLocals, Store::IntToWord(0));
    return static_cast<ByteCodeFrame *>(frame);
  }
  
  static ByteCodeFrame *New(Closure *closure) {
    return New(ByteConcreteCode::FromWord(closure->GetConcreteCode()), closure);
  }
};

#endif
