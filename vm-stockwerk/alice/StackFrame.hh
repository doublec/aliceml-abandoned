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

#ifndef __ALICE__STACK_FRAME_HH__
#define __ALICE__STACK_FRAME_HH__

#if defined(INTERFACE)
#pragma interface "alice/StackFrame.hh"
#endif
#include "generic/StackFrame.hh"

static const FrameLabel ABSTRACT_CODE_FRAME =
  (FrameLabel) (MIN_LANGUAGE_LAYER_FRAME + 1);
static const FrameLabel ABSTRACT_CODE_HANDLER_FRAME =
  (FrameLabel) (MIN_LANGUAGE_LAYER_FRAME + 2);
static const FrameLabel LAZY_SELECTION_FRAME =
  (FrameLabel) (MIN_LANGUAGE_LAYER_FRAME + 3);
static const FrameLabel VECTOR_TABULATE_FRAME =
  (FrameLabel) (MIN_LANGUAGE_LAYER_FRAME + 4);
static const FrameLabel RAISE_FRAME =
  (FrameLabel) (MIN_LANGUAGE_LAYER_FRAME + 5);
static const FrameLabel CELLMAP_INSERT_FRAME =
  (FrameLabel) (MIN_LANGUAGE_LAYER_FRAME + 6);
static const FrameLabel CELLMAP_ITERATOR_FRAME =
  (FrameLabel) (MIN_LANGUAGE_LAYER_FRAME + 7);
static const FrameLabel CELLMAP_FIND_FRAME =
  (FrameLabel) (MIN_LANGUAGE_LAYER_FRAME + 8);
static const FrameLabel IMPMAP_INSERT_FRAME =
  (FrameLabel) (MIN_LANGUAGE_LAYER_FRAME + 9);
static const FrameLabel IMPMAP_ITERATOR_FRAME =
  (FrameLabel) (MIN_LANGUAGE_LAYER_FRAME + 10);
static const FrameLabel IMPMAP_FIND_FRAME =
  (FrameLabel) (MIN_LANGUAGE_LAYER_FRAME + 11);

// AliceNativeCode Frames
static const FrameLabel NATIVE_CODE_FRAME =
  (FrameLabel) (MIN_LANGUAGE_LAYER_FRAME + 12);
static const FrameLabel NATIVE_CODE_HANDLER_FRAME =
  (FrameLabel) (MIN_LANGUAGE_LAYER_FRAME + 13);
static const FrameLabel LAZY_COMPILE_FRAME =
  (FrameLabel) (MIN_LANGUAGE_LAYER_FRAME + 14);

// Alice Async IO Frames
static const FrameLabel FD_INPUT_FRAME =
  (FrameLabel) (MIN_LANGUAGE_LAYER_FRAME + 15);

#endif
