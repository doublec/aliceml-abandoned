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

#ifndef __GENERIC__INTERNAL_TASKS_HH__
#define __GENERIC__INTERNAL_TASKS_HH__

#if defined(INTERFACE)
#pragma interface "generic/InternalTasks.hh"
#endif

#include "store/Store.hh"

class InternalTasks {
public:
  static word await; // calling-convention conversion
  static word defaultHandler; //--** this is never pushed?
  static word raise; //--** Thread.raiseIn; move to alice/primitives/Thread.cc
  static word terminate; //--** Future.concur

  static void Init();
};

#endif __GENERIC__INTERNAL_TASKS_HH__
