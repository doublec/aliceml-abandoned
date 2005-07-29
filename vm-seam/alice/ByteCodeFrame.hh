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

class AliceDll ByteCodeFrame : public StackFrame {
protected:
  enum { CODE_POS, PC_POS, CP_POS, IP_POS, SIZE_POS, BASE_SIZE };
public:
  // ByteCodeFrame Accessors
  
  void GetState(u_int *pc, 
	       Closure **cp, 
	       Tuple **ip) { 
    *pc = Store::DirectWordToInt(StackFrame::GetArg(PC_POS));
    *cp = Closure::FromWordDirect(StackFrame::GetArg(CP_POS));
    *ip = Tuple::FromWordDirect(StackFrame::GetArg(IP_POS));
  }
  u_int GetSize() {
    return Store::DirectWordToInt(StackFrame::GetArg(SIZE_POS));
  }
  Chunk *GetCode() {
    return Store::DirectWordToChunk(StackFrame::GetArg(CODE_POS));
  }
  u_int GetPC() {
    return Store::DirectWordToInt(StackFrame::GetArg(PC_POS));
  }
  Tuple* GetIP() {
    return Tuple::FromWordDirect(StackFrame::GetArg(IP_POS));
  }
  Closure* GetCP() {
    return Closure::FromWordDirect(StackFrame::GetArg(CP_POS));
  }
  word GetLocal(u_int index) {
    return StackFrame::GetArg(BASE_SIZE + index);
  }
  void SetCode(Chunk *code) {
    StackFrame::ReplaceArg(CODE_POS, code->ToWord());
  }
  void SetLocal(u_int index, word w) {
    StackFrame::ReplaceArg(BASE_SIZE + index, w);
  }
  word SaveState(u_int pc, Closure *cp, Tuple *ip) {
    StackFrame::ReplaceArg(PC_POS, Store::IntToWord(pc));
    StackFrame::ReplaceArg(CP_POS, cp->ToWord());
    StackFrame::ReplaceArg(IP_POS, ip->ToWord());
  }
  void SavePC(u_int pc) {
    StackFrame::ReplaceArg(PC_POS, Store::IntToWord(pc));
  }
  // ByteCodeFrame Constructor
  static ByteCodeFrame *New(Interpreter *interpreter,
			    Chunk *code,
			    u_int pc, 
			    Closure *cp,
			    Tuple *ip,
			    u_int nbLocals) {
    // initialize every slot, so that the GC will not be confused
    u_int frSize = BASE_SIZE + nbLocals;
    NEW_STACK_FRAME(frame, interpreter, frSize);
    frame->InitArg(CODE_POS, code->ToWord());
    frame->InitArg(PC_POS, pc);
    frame->InitArg(CP_POS, cp->ToWord());
    frame->InitArg(IP_POS, ip->ToWord());
    frame->InitArg(SIZE_POS, StackFrame::GetBaseSize() + frSize);
    word defaultValue = Store::IntToWord(0);
    for (u_int i = nbLocals; i--; )
      frame->InitArg(BASE_SIZE + i, defaultValue);
    return STATIC_CAST(ByteCodeFrame *, frame);
  }
};

#endif
