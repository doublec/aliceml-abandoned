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

#ifndef __GENERIC__STACK_FRAME_HH__
#define __GENERIC__STACK_FRAME_HH__

#if defined(INTERFACE)
#pragma interface "generic/StackFrame.hh"
#endif

#include "store/Store.hh"

class Interpreter;

// Known StackFrame Types
typedef enum {
  MIN_STACK_FRAME             = MIN_DATA_LABEL,
  // Primitive Frames
  PUSHCALL_FRAME              = MIN_STACK_FRAME,
  BYNEED_FRAME,
  BOTTOM_FRAME,
  PRIMITIVE_FRAME,
  // Pickling Frames
  INPUT_FRAME,
  TRANSFORM_FRAME,
  UNPICKLING_FRAME,
  PICKLE_UNPACK_FRAME,
  PICKLE_LOAD_FRAME,
  PICKLING_FRAME,
  PICKLE_PACK_FRAME,
  PICKLE_SAVE_FRAME,
  // BootLinker Frames
  APPLY_FRAME,
  ENTER_FRAME,
  LINK_FRAME,
  LOAD_FRAME,
  // Alice Frames //--** should be in the Alice Language Layer
  ABSTRACT_CODE_FRAME,
  ABSTRACT_CODE_HANDLER_FRAME,
  LAZY_SELECTION_FRAME,
  VECTOR_TABULATE_FRAME,
  RAISE_FRAME,
  REFMAP_ITERATOR_FRAME,
  // AliceNativeCode Frames
  NATIVE_CODE_FRAME,
  NATIVE_CODE_HANDLER_FRAME,
  LAZY_COMPILE_FRAME,
  // Alice Async IO Frames
  IO_FRAME
  // End of Frames
  MAX_STACK_FRAME = IO_FRAME
} FrameLabel;

class StackFrame: private Block {
protected:
  enum { INTERPRETER_POS, BASE_SIZE };
public:
  using Block::ToWord;

  // StackFrame Constructors
  static StackFrame *New(FrameLabel l, Interpreter *interpreter) {
    Block *p = Store::AllocBlock((BlockLabel) l, BASE_SIZE);
    p->InitArg(INTERPRETER_POS, Store::UnmanagedPointerToWord(interpreter));
    return static_cast<StackFrame *>(p);
  }
  static StackFrame *New(FrameLabel l, Interpreter *interpreter, u_int size) {
    Block *p = Store::AllocBlock((BlockLabel) l, BASE_SIZE + size);
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

  // StackFrame Accessors
  FrameLabel GetLabel() {
    return static_cast<FrameLabel>(static_cast<int>(Block::GetLabel()));
  }
  Interpreter *GetInterpreter() {
    return static_cast<Interpreter *>
      (Store::WordToUnmanagedPointer(Block::GetArg(INTERPRETER_POS)));
  }
  word GetArg(u_int pos) {
    return Block::GetArg(BASE_SIZE + pos);
  }
  void InitArg(u_int pos, int value) {
    Block::InitArg(BASE_SIZE + pos, value);
  }
  void InitArg(u_int pos, word value) {
    Block::InitArg(BASE_SIZE + pos, value);
  }
  void ReplaceArg(u_int pos, int value) {
    Block::ReplaceArg(BASE_SIZE + pos, value);
  }
  void ReplaceArg(u_int pos, word value) {
    Block::ReplaceArg(BASE_SIZE + pos, value);
  }
};

#endif
