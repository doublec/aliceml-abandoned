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

#ifndef __EMULATOR_STACKFRAME_HH__
#define __EMULATOR_STACKFRAME_HH__

class Interpreter;

// Known StackFrame Types
typedef enum {
  MIN_STACK_FRAME       = MIN_DATA_LABEL,
  CALL_FRAME            = MIN_STACK_FRAME,
  BYNEED_FRAME          = (CALL_FRAME + 1),
  LAZY_SELECTION_FRAME  = (BYNEED_FRAME + 1),
  ABSTRACT_CODE_FRAME   = (LAZY_SELECTION_FRAME + 1),
  PRIMITIVE_FRAME       = (ABSTRACT_CODE_FRAME + 1),
  VECTOR_TABULATE_FRAME = (PRIMITIVE_FRAME + 1),
  // Pickling Frames
  INPUT_FRAME           = (VECTOR_TABULATE_FRAME + 1),
  TRANSFORM_FRAME       = (INPUT_FRAME + 1),
  UNPICKLE_FRAME        = (TRANSFORM_FRAME + 1),
  PICKLE_UNPACK_FRAME   = (UNPICKLE_FRAME + 1),
  PICKLE_LOAD_FRAME     = (PICKLE_UNPACK_FRAME + 1),
  // BootLinker Frames
  APPLY_FRAME           = (PICKLE_LOAD_FRAME + 1),
  ENTER_FRAME           = (APPLY_FRAME + 1),
  LINK_FRAME            = (ENTER_FRAME + 1),
  LOAD_FRAME            = (LINK_FRAME + 1),
  MAX_STACK_FRAME       = LOAD_FRAME
} FrameLabel;

class StackFrame : private Block {
private:
  static const u_int INTERPRETER_POS = 0;
public:
  using Block::ToWord;
  // StackFrame Accessors
  word GetArg(u_int pos) {
    return Block::GetArg(pos + 1);
  }
  void InitArg(u_int pos, word value) {
    Block::InitArg(pos, value);
  }
  void ReplaceArg(u_int pos, word value) {
    Block::ReplaceArg(pos, value);
  }
  Interpreter *GetInterpreter() {
    return (Interpreter *)
      Store::WordToUnmanagedPointer(Block::GetArg(INTERPRETER_POS));
  }
  // StackFrame Constructors
  static StackFrame *New(FrameLabel l, Interpreter *interpreter) {
    Block *p = Store::AllocBlock((BlockLabel) l, 1);
    Assert(p != INVALID_POINTER);
    p->InitArg(INTERPRETER_POS, Store::UnmanagedPointerToWord(interpreter));
    return (StackFrame *) p;
  }
  static StackFrame *New(FrameLabel l, Interpreter *interpreter, u_int size) {
    size +=1;
    Block *p = Store::AllocBlock((BlockLabel) l, size);
    Assert(p != INVALID_POINTER);
    p->InitArg(INTERPRETER_POS, Store::UnmanagedPointerToWord(interpreter));
    return (StackFrame *) p;
  }
  // StackFrame Untagging
  static StackFrame *FromWord(word frame) {
    Block *p = Store::WordToBlock(frame);
    Assert(p == INVALID_POINTER ||
	   ((p->GetLabel() >= (BlockLabel) MIN_STACK_FRAME) &&
	    (p->GetLabel() <= (BlockLabel) MAX_STACK_FRAME)));
    return (StackFrame *) p;
  }
  static StackFrame *FromWordDirect(word frame) {
    Block *p = Store::DirectWordToBlock(frame);
    Assert(p == INVALID_POINTER ||
	   ((p->GetLabel() >= (BlockLabel) MIN_STACK_FRAME) &&
	    (p->GetLabel() <= (BlockLabel) MAX_STACK_FRAME)));
    return (StackFrame *) p;
  }
};

#endif
