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

#ifndef __JAVA__STACK_FRAME_HH__
#define __JAVA__STACK_FRAME_HH__

#if defined(INTERFACE)
#pragma interface "java/StackFrame.hh"
#endif
#include "generic/StackFrame.hh"

static const FrameLabel BUILD_CLASS_FRAME =
  (FrameLabel) (MIN_LANGUAGE_LAYER_FRAME + 1);
static const FrameLabel RESOLVE_CLASS_FRAME =
  (FrameLabel) (MIN_LANGUAGE_LAYER_FRAME + 2);
static const FrameLabel JAVA_BYTE_CODE_FRAME =
  (FrameLabel) (MIN_LANGUAGE_LAYER_FRAME + 3);
static const FrameLabel RUN_MAIN_FRAME =
  (FrameLabel) (MIN_LANGUAGE_LAYER_FRAME + 4);

#endif
