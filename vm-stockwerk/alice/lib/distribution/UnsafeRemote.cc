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

#include "emulator/Authoring.hh"
#include "emulator/RootSet.hh"
#include "emulator/Pickler.hh"
#include "emulator/Unpickler.hh"

static word callback;

DEFINE1(UnsafeRemote_setCallback) {
  callback = x0;
  RETURN_UNIT;
} END

DEFINE2(UnsafeRemote_dynamicCall) {
  if (callback == Store::IntToWord(0)) {
    RAISE(PrimitiveTable::Hole_Hole); //--** to be done
  } else {
    DECLARE_STRING(ticket, x0);
    Scheduler::nArgs = 2;
    Scheduler::currentArgs[0] = ticket->ToWord();
    Scheduler::currentArgs[1] = x1;
    return taskStack->PushCall(callback);
  }
} END

DEFINE1(UnsafeRemote_pack) {
  return Pickler::Pack(x0, taskStack);
} END

DEFINE1(UnsafeRemote_unpack) {
  DECLARE_STRING(packedValue, x0);
  return Unpickler::Unpack(static_cast<Chunk *>(packedValue), taskStack);
} END

word UnsafeRemote(void) {
  RootSet::Add(callback);
  callback = Store::IntToWord(0);

  Tuple *t = Tuple::New(4);
  t->Init(0, Primitive::MakeClosure("UnsafeRemote_dynamicCall",
				    UnsafeRemote_dynamicCall, 2, false));
  t->Init(1, Primitive::MakeClosure("UnsafeRemote_pack",
				    UnsafeRemote_pack, 1, true));
  t->Init(2, Primitive::MakeClosure("UnsafeRemote_setCallback",
				    UnsafeRemote_setCallback, 1, true));
  t->Init(3, Primitive::MakeClosure("UnsafeRemote_unpack",
				    UnsafeRemote_unpack, 1, true));
  RETURN_STRUCTURE(t);
}
