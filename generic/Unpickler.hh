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

#ifndef __EMULATOR__UNPICKLER_HH__
#define __EMULATOR__UNPICKLER_HH__

#if defined(INTERFACE)
#pragma interface "emulator/Unpickler.hh"
#endif

#include "emulator/Interpreter.hh"

typedef unsigned char u_char;

class Unpickler {
public:
  // Exceptions
  static word Corrupt;
  // Unpickler Functions
  static Interpreter::Result Unpack(Chunk *s, TaskStack *taskStack);
  // Excpect c filename style
  static Interpreter::Result Load(Chunk *filename, TaskStack *taskStack);
  // Unpickler Static Constructor
  static void Init();
};

#endif
