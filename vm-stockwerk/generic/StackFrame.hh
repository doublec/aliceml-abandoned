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
  MIN_STACK_FRAME             = MIN_DATA_LABEL,
  CALL_FRAME                  = MIN_STACK_FRAME,
  BYNEED_FRAME                = (CALL_FRAME + 1),
  LAZY_SELECTION_FRAME        = (BYNEED_FRAME + 1),
  // AbstractCode Frames
  ABSTRACT_CODE_FRAME         = (LAZY_SELECTION_FRAME + 1),
  ABSTRACT_CODE_HANDLER_FRAME = (ABSTRACT_CODE_FRAME + 1),
  // Primitive Frames
  PRIMITIVE_FRAME             = (ABSTRACT_CODE_HANDLER_FRAME + 1),
  VECTOR_TABULATE_FRAME       = (PRIMITIVE_FRAME + 1),
  RAISE_FRAME                 = (VECTOR_TABULATE_FRAME + 1),
  // Pickling Frames
  INPUT_FRAME                 = (RAISE_FRAME + 1),
  TRANSFORM_FRAME             = (INPUT_FRAME + 1),
  UNPICKLE_FRAME              = (TRANSFORM_FRAME + 1),
  PICKLE_UNPACK_FRAME         = (UNPICKLE_FRAME + 1),
  PICKLE_LOAD_FRAME           = (PICKLE_UNPACK_FRAME + 1),
  PICKLING_FRAME              = (PICKLE_LOAD_FRAME + 1),
  PICKLE_PACK_FRAME           = (PICKLING_FRAME + 1),
  PICKLE_SAVE_FRAME           = (PICKLE_PACK_FRAME + 1),
  // BootLinker Frames
  APPLY_FRAME                 = (PICKLE_SAVE_FRAME + 1),
  ENTER_FRAME                 = (APPLY_FRAME + 1),
  LINK_FRAME                  = (ENTER_FRAME + 1),
  LOAD_FRAME                  = (LINK_FRAME + 1),
  MAX_STACK_FRAME             = LOAD_FRAME
} FrameLabel;

class StackFrame : private Block {
private:
  static const u_int INTERPRETER_POS = 0;
public:
  using Block::ToWord;
  FrameLabel GetLabel() {
    return static_cast<FrameLabel>(static_cast<int>(Block::GetLabel()));
  }
  // StackFrame Accessors
  word GetArg(u_int pos) {
    return Block::GetArg(pos + 1);
  }
  void InitArg(u_int pos, word value) {
    Block::InitArg(pos + 1, value);
  }
  void ReplaceArg(u_int pos, word value) {
    Block::ReplaceArg(pos + 1, value);
  }
  Interpreter *GetInterpreter() {
    return static_cast<Interpreter *>
      (Store::WordToUnmanagedPointer(Block::GetArg(INTERPRETER_POS)));
  }
  // StackFrame Constructors
  static StackFrame *New(FrameLabel l, Interpreter *interpreter) {
    Block *p = Store::AllocBlock((BlockLabel) l, 1);
    Assert(p != INVALID_POINTER);
    p->InitArg(INTERPRETER_POS, Store::UnmanagedPointerToWord(interpreter));
    return static_cast<StackFrame *>(p);
  }
  static StackFrame *New(FrameLabel l, Interpreter *interpreter, u_int size) {
    Block *p = Store::AllocBlock((BlockLabel) l, size + 1);
    Assert(p != INVALID_POINTER);
    p->InitArg(INTERPRETER_POS, Store::UnmanagedPointerToWord(interpreter));
    return static_cast<StackFrame *>(p);
  }
  // StackFrame Untagging
  static StackFrame *FromWordDirect(word frame) {
    Block *p = Store::DirectWordToBlock(frame);
    Assert(p->GetLabel() >= (BlockLabel) MIN_STACK_FRAME &&
	   p->GetLabel() <= (BlockLabel) MAX_STACK_FRAME);
    return static_cast<StackFrame *>(p);
  }
};

#endif
