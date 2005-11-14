//
// Author:
//   Christian Mueller <cmueller@ps.uni-sb.de>
//
// Copyright:
//   Christian Mueller, 2005
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __ALICE_BYTECODE_CONST_PROP_HH__
#define __ALICE_BYTECODE_CONST_PROP_HH__

#if defined(INTERFACE)
#pragma interface "alice/ByteCodeConstProp.hh"
#endif

#include "alice/Data.hh"
#include "alice/ByteCodeInliner.hh"

class ByteCodeConstProp {
public:
  // the information that is returned can be used to eliminate tag
  // tests during compilation
  // The returned map maps (compact) tagtests to a pair 
  // (binding_args option, continuation)
  static Map *RunAnalysis(TagVal* abstractCode, InlineInfo *inlineInfo);
};

#endif
