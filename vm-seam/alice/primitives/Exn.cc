//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Andreas Rossberg <rossberg@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus and Andreas Rossberg, 2004
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "alice/Authoring.hh"


DEFINE1(Exn_name) {
  DECLARE_BLOCK(conVal, x0);
  String *name;
 retry:
  switch (conVal->GetLabel()) {
  case UNIQUESTRING_LABEL:
    name = UniqueString::FromWordDirect(conVal->ToWord())->ToString();
    break;
  case Alice::ConVal:
    conVal = ConVal::FromWordDirect(conVal->ToWord())->GetConstructor();
    goto retry;
  default:
    name = Constructor::FromWordDirect(conVal->ToWord())->GetName();
    break;
  }
  //--** drop prefix
  RETURN(name->ToWord());
} END


DEFINE0(Exn_currentPacket) {
  Tuple *p = Scheduler::GetCurrentThread()->GetCurrentPackage();
  
  if (p == INVALID_POINTER) {
    RAISE(PrimitiveTable::Exn_NoCurrentPacket);
  }
  else {
    RETURN(p->ToWord());
  }
} END


DEFINE2(Exn_reraise) {
  word exn = x0;
  word backtrace = x1;
  Scheduler::SetNArgs(0);
  Scheduler::SetCurrentData(exn);
  Scheduler::SetCurrentBacktrace(Backtrace::FromWord(backtrace));
  return Worker::RERAISE;
} END


DEFINE2(Exn_trace) {
  DECLARE_BLOCKTYPE(Backtrace, backtrace, x1);
  RETURN(backtrace->DumpToString()->ToWord());
} END


DEFINE2(Exn_dumpTrace) {
  DECLARE_BLOCKTYPE(Backtrace, backtrace, x1);
  backtrace->Dump();
  RETURN_UNIT;
} END


void PrimitiveTable::RegisterExn() {
  PrimitiveTable::Exn_NoCurrentPacket =
    UniqueConstructor::New("NoCurrentPacket", "Exn.NoCurrentPacket")->ToWord();
  Register("Exn.NoCurrentPacket", PrimitiveTable::Exn_NoCurrentPacket);
  Register("Exn.currentPacket", Exn_currentPacket, 0);
  Register("Exn.trace", Exn_trace, 2);
  Register("Exn.dumpTrace", Exn_dumpTrace, 2);
  Register("Exn.name", Exn_name, 1);
  Register("Exn.reraise", Exn_reraise, 2);
}
