//
// Author:
//   Jens Regenberg <jens@ps.uni-sb.de>
//
// Copyright:
//   Jens Regenberg, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if DEBUGGER
#ifndef __ALICE__DEBUG_ENVIRONMENT_HH__
#define __ALICE__DEBUG_ENVIRONMENT_HH__

#if defined(INTERFACE)
#pragma interface "alice/DebugEnvironment.hh"
#endif

#include "alice/AbstractCodeFrame.hh"
#include "alice/AbstractCodeInterpreter.hh"
#include "alice/Data.hh"

class DebugEnvironment : public Block {
private:
  enum { NAME_POS, VALUE_POS, TYPE_POS, TYPE_SCHEME_POS, BOUNDING_POS, SIZE };
public:
  static word EMPTY;
  static word INVALID_NAME;

  static DebugEnvironment *New(AbstractCodeFrame::Environment *localEnv,
			       Closure *globalEnvironment);

  static void Init() {
    EMPTY = Store::IntToWord(0);
    INVALID_NAME = Store::IntToWord(1);

    RootSet::Add(EMPTY);
    RootSet::Add(INVALID_NAME);
  }
  static DebugEnvironment *FromWord(word w);
  static DebugEnvironment *FromWordDirect(word w);
  
  word Lookup(String *name);
  TagVal *GetNameValueList();
};
#endif
#endif
