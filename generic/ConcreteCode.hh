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

  // ConcreteCode Constructor
  static ConcreteCode *New(Interpreter *interpreter, u_int size) {
    return static_cast<ConcreteCode *>
      (ConcreteRepresentation::New(interpreter, size));
  }
  // ConcreteCode Untagging
  static ConcreteCode *FromWord(word x) {
    return static_cast<ConcreteCode *>
      (ConcreteRepresentation::FromWord(x));
  }
  static ConcreteCode *FromWordDirect(word x) {
    return static_cast<ConcreteCode *>
      (ConcreteRepresentation::FromWordDirect(x));
  }

  // ConcreteCode Accessors
  Interpreter *GetInterpreter() {
    return static_cast<Interpreter *>(GetHandler());
  }
};

#endif
