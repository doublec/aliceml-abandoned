//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//
#ifndef __TEST__STORE__SCHEME__DECORATOR_HH__
#define __TEST__STORE__SCHEME__DECORATOR_HH__

#include "Environment.hh"

class Decorator : public Environment {
protected:
  static void DecorateDeclArr(Block *instr);
  static void DecorateDefine(Block *instr);
  static void DecorateIf(Block *instr);
  static void DecorateLambda(Block *instr);
  static void DecorateId(Block *instr);
  static void DecorateLet(Block *instr);
  static void DecorateApplication(Block *instr);
  static void DecorateBegin(Block *instr);
  static void DecorateSetQ(Block *instr);
  static void DecorateSetCxr(Block *instr);
public:
  static void Decorate(word tree);
};

#endif __TEST__STORE__SCHEME__DECORATOR_HH__
