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
#include "alice/ByteConcreteCode.hh"
#include "alice/ByteCodeJitter.hh"

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
  if (b == INVALID_POINTER) {
    REQUEST(cc);
  }
  if (b->GetInterpreter() == AbstractCodeInterpreter::self) {
    AliceConcreteCode *acc = AliceConcreteCode::FromWord(cc);
    acc->Disassemble(stderr);
  } 
  else if (b->GetInterpreter() == ByteCodeInterpreter::self) {
    ByteConcreteCode *bcc = ByteConcreteCode::FromWord(cc);
    bcc->Disassemble(stderr);
  }
#if HAVE_LIGHTNING
    else if (b->GetInterpreter() == NativeCodeInterpreter::self) {
    NativeConcreteCode *ncc = NativeConcreteCode::FromWord(cc);
    ncc->Disassemble(stderr);
  }
#endif
    else {
      fprintf(stderr,"unkown interpreter\n");
    }

  RETURN_UNIT;
} END

DEFINE1(UnsafeDebug_byteCompile) {
  DECLARE_CLOSURE(closure, x0);
  word cc = closure->GetConcreteCode();
  ConcreteCode *b = ConcreteCode::FromWord(cc);
  if (b == INVALID_POINTER)
    REQUEST(cc);

  TagVal *abstractCode;

  if (b->GetInterpreter() == AbstractCodeInterpreter::self) {
    AliceConcreteCode *acc = AliceConcreteCode::FromWord(cc);
    abstractCode = acc->GetAbstractCode();
  } 
#if HAVE_LIGHTNING
  else if (b->GetInterpreter() == NativeCodeInterpreter::self) {
    NativeConcreteCode *ncc = NativeConcreteCode::FromWord(cc);
    Transform *transform =
      STATIC_CAST(Transform *, ncc->GetAbstractRepresentation());
    abstractCode = TagVal::FromWordDirect(transform->GetArgument());
  }
#endif
  else if (b->GetInterpreter() == ByteCodeInterpreter::self) {
    fprintf(stderr,"byte concrete code found, nothing to be done\n");
    RETURN_UNIT;
  }
  else {
    fprintf(stderr,"unkown interpreter, do nothing\n");
    RETURN_UNIT;
  }

  LazyByteCompileClosure *lbcClos = LazyByteCompileClosure::New(abstractCode);
  ByteCodeJitter jitter;
  fprintf(stderr,"start byte code jitter\n");
  word wConcreteCode = jitter.Compile(lbcClos);
  closure->SetConcreteCode(wConcreteCode);
  fprintf(stderr,"changed concrete code to byte code\n");

  RETURN_UNIT;
} END

DEFINE1(UnsafeDebug_lazyByteCompile) {
  DECLARE_CLOSURE(closure, x0);
  word cc = closure->GetConcreteCode();
  ConcreteCode *b = ConcreteCode::FromWord(cc);
  if (b == INVALID_POINTER)
    REQUEST(cc);

  TagVal *abstractCode;

  if (b->GetInterpreter() == AbstractCodeInterpreter::self) {
    fprintf(stderr,"is abstract code \n");
    AliceConcreteCode *acc = AliceConcreteCode::FromWord(cc);
    abstractCode = acc->GetAbstractCode();
  } 
#if HAVE_LIGHTNING
  else if (b->GetInterpreter() == NativeCodeInterpreter::self) {
    NativeConcreteCode *ncc = NativeConcreteCode::FromWord(cc);
    Transform *transform =
      STATIC_CAST(Transform *, ncc->GetAbstractRepresentation());
    abstractCode = TagVal::FromWordDirect(transform->GetArgument());
  }
#endif
  else if (b->GetInterpreter() == ByteCodeInterpreter::self) {
    fprintf(stderr,"byte concrete code found, nothing to be done\n");
    RETURN_UNIT;
  }
  else {
    fprintf(stderr,"unkown interpreter, do nothing\n");
    RETURN_UNIT;
  }

  fprintf(stderr,"lazy compile closure created\n");
  word byneed = ByteConcreteCode::New(abstractCode); 
  closure->SetConcreteCode(byneed);

  RETURN_UNIT;
} END


AliceDll word UnsafeDebug() {
  Record *record = Record::New(12);
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
  INIT_STRUCTURE(record, "UnsafeDebug", "byteCompile", 
		 UnsafeDebug_byteCompile, 1); 
  INIT_STRUCTURE(record, "UnsafeDebug", "lazyByteCompile", 
		 UnsafeDebug_lazyByteCompile, 1); 
  RETURN_STRUCTURE("UnsafeDebug$", record);
}
