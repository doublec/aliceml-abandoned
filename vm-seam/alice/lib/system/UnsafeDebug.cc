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
#include "alice/HotSpotConcreteCode.hh"

DEFINE1(UnsafeDebug_print) {
  Debug::Dump(x0);
  RETURN_UNIT;
} END

DEFINE1(UnsafeDebug_unimplemented) {
  Error("UnsafeDebug: unimplemented");
} END

void PrintLiveness(TagVal *abstractCode) {
  Vector *liveness = Vector::FromWordDirect(abstractCode->Sel(6));
  u_int size = liveness->GetLength();
  fprintf(stderr,"print liveness of size %"U_INTF":\n", size/3);
  for(u_int i = 0, j = 1; i<size; i+=3, j++) {
    u_int index = Store::DirectWordToInt(liveness->Sub(i));
    u_int start = Store::DirectWordToInt(liveness->Sub(i+1));
    u_int end   = Store::DirectWordToInt(liveness->Sub(i+2));
    fprintf(stderr, "%"U_INTF". %"U_INTF" -> [%"U_INTF", %"U_INTF"]\n", j, index, start, end);
  }
}

DEFINE1(UnsafeDebug_disassemble) {
  DECLARE_CLOSURE(closure, x0);
  word cc = closure->GetConcreteCode();
  ConcreteCode *b = ConcreteCode::FromWord(cc);
  if (b == INVALID_POINTER) {
    REQUEST(cc);
  }
 
  Interpreter *interpreter = b->GetInterpreter();
  if (interpreter == AbstractCodeInterpreter::self) {
    AliceConcreteCode *acc = AliceConcreteCode::FromWord(cc);
    TagVal *abstractCode = acc->GetAbstractCode();
    
    fprintf(stderr, "AliceConcreteCode:\n");
    PrintLiveness(abstractCode);
    acc->Disassemble(stderr);
  } 
  else if (interpreter == HotSpotInterpreter::self) {
    HotSpotConcreteCode *hscc = HotSpotConcreteCode::FromWord(cc);
    Transform *transform = hscc->GetAbstractRepresentation();
    TagVal *abstractCode = TagVal::FromWordDirect(transform->GetArgument());
    
    fprintf(stderr, "HotSpotConcreteCode (counter = %"U_INTF"):\n", hscc->GetCounter());
    AbstractCode::Disassemble(stderr,
			      TagVal::FromWordDirect(abstractCode->Sel(5)));
  }
  else if (interpreter == ByteCodeInterpreter::self) {
    ByteConcreteCode *bcc = ByteConcreteCode::FromWord(cc);
    Transform *transform = bcc->GetAbstractRepresentation();
    TagVal *abstractCode = TagVal::FromWordDirect(transform->GetArgument());
    
    fprintf(stderr, "ByteConcreteCode:\n");
    PrintLiveness(abstractCode);
    bcc->Disassemble(stderr);
  }
#if HAVE_LIGHTNING
  else if (interpreter == NativeCodeInterpreter::self) {
    NativeConcreteCode *ncc = NativeConcreteCode::FromWord(cc);
    Transform *transform = ncc->GetAbstractRepresentation();
    TagVal *abstractCode = TagVal::FromWordDirect(transform->GetArgument());
    
    fprintf(stderr, "NativeConcreteCode:\n");
    PrintLiveness(abstractCode);
    ncc->Disassemble(stderr);
  }
#endif
  else {
    fprintf(stderr,"unknown interpreter: %s\n",interpreter->Identify());
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
    Transform *transform = ncc->GetAbstractRepresentation();
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

  word w = HotSpotConcreteCode::New(abstractCode);
  HotSpotConcreteCode *hsc = HotSpotConcreteCode::FromWordDirect(w);
  ByteCodeJitter jitter;
  fprintf(stderr,"start byte code jitter\n");
  jitter.Compile(hsc);
  closure->SetConcreteCode(hsc->ToWord());
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
    Transform *transform = ncc->GetAbstractRepresentation();
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

  fprintf(stderr,"lazy byte concrete code created\n");
  closure->SetConcreteCode(ByteConcreteCode::New(abstractCode));

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
