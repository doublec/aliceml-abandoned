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

#ifndef __GENERIC__UNPICKLER_HH__
#define __GENERIC__UNPICKLER_HH__

#if defined(INTERFACE)
#pragma interface "generic/Unpickler.hh"
#endif

#include "generic/Interpreter.hh"

class Unpickler {
public:
  // Exceptions
  static word Corrupt;

  // Unpickler Static Constructor
  static void Init();
  static void InitExceptions();

  typedef word (*handler)(word);
  static void RegisterHandler(Chunk *name, handler handler);

  // Unpickler Functions
  static Interpreter::Result Unpack(Chunk *string, TaskStack *taskStack);
  static Interpreter::Result Load(Chunk *filename, TaskStack *taskStack);
};

#endif
