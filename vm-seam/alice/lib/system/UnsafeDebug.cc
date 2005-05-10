//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "alice/Authoring.hh"
#include "alice/AliceConcreteCode.hh"
#include "alice/NativeConcreteCode.hh"

DEFINE1(UnsafeDebug_print) {
  Debug::Dump(x0);
  RETURN_UNIT;
} END

DEFINE1(UnsafeDebug_unimplemented) {
  Error("UnsafeDebug: unimplemented");
} END

DEFINE1(UnsafeDebug_disassemble) {
  DECLARE_CLOSURE(closure, x0);
  word cc = closure->GetConcreteCode();
  ConcreteCode *b = ConcreteCode::FromWord(cc);
  if (b == INVALID_POINTER)
    REQUEST(cc);
  if (b->GetInterpreter() == AbstractCodeInterpreter::self) {
    AliceConcreteCode *acc = AliceConcreteCode::FromWord(cc);
    acc->Disassemble(stderr);
  } 
#if HAVE_LIGHTNING
  else if (b->GetInterpreter() == NativeCodeInterpreter::self) {
    NativeConcreteCode *acc = NativeConcreteCode::FromWord(cc);
    acc->Disassemble(stderr);
  }
#endif
  else 
    Error("UnsafeDebug_disassemble: unkown code interpreter found");
  
  RETURN_UNIT;
} END

AliceDll word UnsafeDebug() {
  Record *record = Record::New(10);
  INIT_STRUCTURE(record, "UnsafeDebug", "setPrintDepth",
		 UnsafeDebug_unimplemented, 1);
  INIT_STRUCTURE(record, "UnsafeDebug", "setPrintWidth",
		 UnsafeDebug_unimplemented, 1);
  INIT_STRUCTURE(record, "UnsafeDebug", "toString",
		 UnsafeDebug_unimplemented, 1);
  INIT_STRUCTURE(record, "UnsafeDebug", "print",
		 UnsafeDebug_print, 1);
  INIT_STRUCTURE(record, "UnsafeDebug", "inspect",
		 UnsafeDebug_unimplemented, 1);
  INIT_STRUCTURE(record, "UnsafeDebug", "disassemble",
		 UnsafeDebug_disassemble, 1);
  INIT_STRUCTURE(record, "UnsafeDebug", "Print$",
		 UnsafeDebug_unimplemented, 1);
  INIT_STRUCTURE(record, "UnsafeDebug", "Inspect$",
		 UnsafeDebug_unimplemented, 1);
  INIT_STRUCTURE(record, "UnsafeDebug", "InspectType$",
		 UnsafeDebug_unimplemented, 1);
  INIT_STRUCTURE(record, "UnsafeDebug", "InspectSig$",
		 UnsafeDebug_unimplemented, 1);
  RETURN_STRUCTURE("UnsafeDebug$", record);
}
