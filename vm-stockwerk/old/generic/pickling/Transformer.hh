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

#ifndef __GENERIC__PICKLING__TRANSFORMER_HH__
#define __GENERIC__PICKLING__TRANSFORMER_HH__

#if defined(INTERFACE)
#pragma interface "generic/pickling/Transformer.hh"
#endif

#include "generic/TaskManager.hh"

class Transformer: public TaskManager {
public:
  typedef Result (*transformer)(TaskStack *taskStack,
				word abstractRepresentation);
private:
  transformer function;
public:
  Transformer(transformer f);

  virtual void PushCall(TaskStack *taskStack, Closure *closure);
  virtual u_int PurgeFrame(TaskStack *taskStack, u_int offset);
  virtual Result Handle(TaskStack *taskStack);
  virtual Result Run(TaskStack *taskStack, int nargs);
};

#endif __GENERIC__PICKLING__TRANSFORMER_HH__
