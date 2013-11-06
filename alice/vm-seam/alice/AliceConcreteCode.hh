//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __ALICE__ALICE_CONCRETE_CODE_HH__
#define __ALICE__ALICE_CONCRETE_CODE_HH__

#if defined(INTERFACE)
#pragma interface "alice/AliceConcreteCode.hh"
#endif

#include "alice/Data.hh"
#include "alice/AbstractCode.hh"
#include "alice/AbstractCodeInterpreter.hh"


class AliceDll AliceConcreteCode: private ConcreteCode {
private:
  enum { ABSTRACT_CODE_POS, CLOSE_CONCRETE_CODES_POS, TRANSFORM_POS, SIZE };
public:
  using ConcreteCode::ToWord;

  static word New(TagVal *abstractCode);
  
  TagVal *GetAbstractCode() {
    return TagVal::FromWordDirect(Get(ABSTRACT_CODE_POS));
  }
  
  u_int GetNLocals() {
    return AbstractCode::GetNumberOfLocals(GetAbstractCode());
  }
  
  Transform *GetAbstractRepresentation() {
    return Transform::FromWordDirect(Get(TRANSFORM_POS));
  }
  
  /**
   * @return A map from Close instr to the ConcreteCode to use for it,
   *         with the subst inherited from this abstractCode. The map
   *         only contains entries for Close instructions that have
   *         already been evaluated at least once.
   */
  Map *GetCloseConcreteCodes() {
    return Map::FromWordDirect(Get(CLOSE_CONCRETE_CODES_POS));
  }
  
  void Disassemble(std::FILE *file);

  static AliceConcreteCode *FromWord(word x) {
    ConcreteCode *b = ConcreteCode::FromWord(x);
    Assert(b == INVALID_POINTER ||
	   b->GetInterpreter() == AbstractCodeInterpreter::self);
    return static_cast<AliceConcreteCode *>(b);
  }
  
  static AliceConcreteCode *FromWordDirect(word x) {
    ConcreteCode *b = ConcreteCode::FromWordDirect(x);
    Assert(b->GetInterpreter() == AbstractCodeInterpreter::self);
    return static_cast<AliceConcreteCode *>(b);
  }
};

#endif
