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

class Worker;

// Known StackFrame Types
enum FrameLabel {
  MIN_STACK_FRAME = MIN_DATA_LABEL,
  // Primitive Frames
  PUSHCALL_FRAME = MIN_STACK_FRAME,
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
  BOOT_FRAME,
  // Lanugage specific Frames
  MIN_LANGUAGE_LAYER_FRAME = BOOT_FRAME + 1,
  MAX_STACK_FRAME = MAX_DATA_LABEL
};

class DllExport StackFrame: private Block {
protected:
  enum { WORKER_POS, BASE_SIZE };
public:
  using Block::ToWord;

  // StackFrame Constructors
  static StackFrame *New(FrameLabel l, Worker *worker) {
    Block *p = Store::AllocBlock((BlockLabel) l, BASE_SIZE);
    p->InitArg(WORKER_POS, Store::UnmanagedPointerToWord(worker));
    return static_cast<StackFrame *>(p);
  }
  static StackFrame *New(FrameLabel l, Worker *worker, u_int size) {
    Block *p = Store::AllocBlock((BlockLabel) l, BASE_SIZE + size);
    p->InitArg(WORKER_POS, Store::UnmanagedPointerToWord(worker));
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
  Worker *GetWorker() {
    return static_cast<Worker *>
      (Store::WordToUnmanagedPointer(Block::GetArg(WORKER_POS)));
  }
  word GetArg(u_int pos) {
    return Block::GetArg(BASE_SIZE + pos);
  }
  void InitArg(u_int pos, s_int value) {
    Block::InitArg(BASE_SIZE + pos, value);
  }
  void InitArg(u_int pos, word value) {
    Block::InitArg(BASE_SIZE + pos, value);
  }
  void ReplaceArg(u_int pos, s_int value) {
    Block::ReplaceArg(BASE_SIZE + pos, value);
  }
  void ReplaceArg(u_int pos, word value) {
    Block::ReplaceArg(BASE_SIZE + pos, value);
  }
};

#endif
