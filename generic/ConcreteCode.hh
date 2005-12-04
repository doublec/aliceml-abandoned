//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__CONCRETE_CODE_HH__
#define __GENERIC__CONCRETE_CODE_HH__

#if defined(INTERFACE)
#pragma interface "generic/ConcreteCode.hh"
#endif

#include "generic/ConcreteRepresentation.hh"
#include "generic/Interpreter.hh"

class SeamDll ConcreteCode: private ConcreteRepresentation {
public:
  using ConcreteRepresentation::ToWord;
  using ConcreteRepresentation::Init;
  using ConcreteRepresentation::Get;
  using ConcreteRepresentation::Replace;

  // ConcreteCode Constructor
  static ConcreteCode *New(Interpreter *interpreter, u_int size) {
    return STATIC_CAST(ConcreteCode *, ConcreteRepresentation::New(interpreter, size));
  }
  // ConcreteCode Untagging
  static ConcreteCode *FromWord(word x) {
    return STATIC_CAST(ConcreteCode *, ConcreteRepresentation::FromWord(x));
  }
  static ConcreteCode *FromWordDirect(word x) {
    return STATIC_CAST(ConcreteCode *, ConcreteRepresentation::FromWordDirect(x));
  }

  // ConcreteCode Accessors
  Interpreter *GetInterpreter() {
    return STATIC_CAST(Interpreter *, GetHandler());
  }
  void ReplaceInterpreter(Interpreter *interpreter) {
    ReplaceHandler(interpreter);
  }
};

#endif
