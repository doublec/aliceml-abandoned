//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__CONCRETE_REPRESENTATION_HANDLER_HH__
#define __GENERIC__CONCRETE_REPRESENTATION_HANDLER_HH__

#include "Base.hh"
#include "store/Types.hh"

class Block;
class Transform;
class ConcreteRepresentation;

class SeamDll ConcreteRepresentationHandler {
public:
  // returns INVALID_POINTER if there is none
  virtual Transform *GetAbstractRepresentation(ConcreteRepresentation *) = 0;
};

class SeamDll NullConcreteRepresentationHandler : public ConcreteRepresentationHandler {
public:
  static NullConcreteRepresentationHandler *GetInstance() {
    static NullConcreteRepresentationHandler instance;
    return &instance;
  }
  Transform *GetAbstractRepresentation(ConcreteRepresentation *) {
    return INVALID_POINTER;
  }
};

#endif
