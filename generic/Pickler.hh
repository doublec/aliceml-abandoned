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

#ifndef __EMULATOR_PICKLER_HH__
#define __EMULATOR_PICKLER_HH__

#if defined(INTERFACE)
#pragma interface "emulator/Pickler.hh"
#endif

#include "emulator/Interpreter.hh"

class Pickler {
public:
  // Exceptions
  static word Sited;
  // Pickler Functions
  static Interpreter::Result Pack(word x, TaskStack *taskStack);
  static Interpreter::Result Save(Chunk *filename, word x,
				  TaskStack *taskStack);
  // Pickler Static Constructor
  static void Init();
};

#endif
