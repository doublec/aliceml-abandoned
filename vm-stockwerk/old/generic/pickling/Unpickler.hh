//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__PICKLING__UNPICKLER_HH__
#define __GENERIC__PICKLING__UNPICKLER_HH__

#if defined(INTERFACE)
#pragma interface "generic/pickling/Unpickler.hh"
#endif

#include "generic/TaskManager.hh"

class Unpickler: public TaskManager {
public:
  Unpickler();

  virtual void PushCall(TaskStack *taskStack, Closure *closure);
  virtual u_int PurgeFrame(TaskStack *taskStack, u_int offset);
  virtual Result Handle(TaskStack *taskStack);
  virtual Result Run(TaskStack *taskStack, int nargs);
};

#endif __GENERIC_PICKLING__UNPICKLER_HH__
