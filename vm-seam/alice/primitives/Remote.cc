//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "alice/Authoring.hh"

DEFINE2(Remote_dynamicCall) {
  if (AliceLanguageLayer::remoteCallback == Store::IntToWord(0)) {
    RAISE(PrimitiveTable::Hole_Hole); //--** to be done
  } else {
    Scheduler::SetNArgs(2);
    Scheduler::SetCurrentArg(0, x0);
    Scheduler::SetCurrentArg(1, x1);
    return Scheduler::PushCall(AliceLanguageLayer::remoteCallback);
  }
} END

void PrimitiveTable::RegisterRemote() {
  Register("Remote.dynamicCall", Remote_dynamicCall, 2);
}
