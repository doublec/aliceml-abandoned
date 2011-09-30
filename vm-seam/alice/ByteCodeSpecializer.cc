


#if defined(INTERFACE)
#pragma implementation "alice/ByteCodeSpecializer.hh"
#endif


#include <stdio.h>
#include <stdlib.h>
#include "alice/AbstractCodeInterpreter.hh"
#include "alice/ByteCodeSpecializer.hh"
#include "alice/ByteCodeJitter.hh"
#include "alice/ByteCodeInterpreter.hh"
#include "alice/ByteConcreteCode.hh"
#include "alice/HotSpotConcreteCode.hh"


u_int ByteCodeSpecializer::calls;
u_int ByteCodeSpecializer::numSpecialized;
word ByteCodeSpecializer::closures;
bool ByteCodeSpecializer::trace;


void ByteCodeSpecializer::Init() {
  calls = 0;
  numSpecialized = 0;
  closures = Map::New(128)->ToWord();
  RootSet::Add(closures);
  trace = getenv("ALICE_TRACE_BYTE_CODE_SPECIALIZER") != NULL;
}


bool ByteCodeSpecializer::CanBeSpecialized(ByteConcreteCode *bcc) {
  TagVal *abstractCode = bcc->GetAbstractCode();
  Vector *subst = Vector::FromWordDirect(abstractCode->Sel(1));
  for (u_int i=subst->GetLength(); i--; ) {
    if (subst->Sub(i) == Store::IntToWord(Types::NONE)) {
      return true;
    }
  }
  return false;
}


bool ByteCodeSpecializer::Specialize(Closure *c) {
  ByteConcreteCode *bcc = ByteConcreteCode::FromWordDirect(c->GetConcreteCode());
  TagVal *abstractCode = bcc->GetAbstractCode();
  
  if (trace) {
    String *name = AbstractCodeInterpreter::MakeProfileName(bcc->GetAbstractCode());
    fprintf(stderr, "ByteCodeSpecializer - specializing: %s\n", name->ExportC());
  }
  
  Vector *subst = Vector::New(c->GetSize());
  for (u_int i=c->GetSize(); i--; ) {
    TagVal *some = TagVal::New1(Types::SOME, c->Sub(i));
    subst->Init(i, some->ToWord());
  }

  TagVal *newAbstractCode = TagVal::New(AbstractCode::Function, AbstractCode::functionWidth);
  newAbstractCode->Init(0, abstractCode->Sel(0));
  newAbstractCode->Init(1, subst->ToWord());
  newAbstractCode->Init(2, abstractCode->Sel(2));
  newAbstractCode->Init(3, abstractCode->Sel(3));
  newAbstractCode->Init(4, abstractCode->Sel(4));
  newAbstractCode->Init(5, abstractCode->Sel(5));
  newAbstractCode->Init(6, abstractCode->Sel(6));
  
  // go straight to ByteCode - not through HotSpot, since we already know its hot
  HotSpotCode *hsc = reinterpret_cast<HotSpotCode*>(ByteConcreteCode::New(newAbstractCode));
  ByteCodeJitter jitter;
  jitter.Compile(hsc);
  c->SetConcreteCode(hsc->ToWord());

#if PROFILE
  Profiler::IncClosures(hsc->ToWord());
#endif
  
  numSpecialized++;
}


bool ByteCodeSpecializer::RecordCall(Closure* c) {
  ConcreteCode *cc = ConcreteCode::FromWord(c->GetConcreteCode());
  
  if (cc != INVALID_POINTER && cc->GetInterpreter() == ByteCodeInterpreter::self) {
    ByteConcreteCode *bcc = reinterpret_cast<ByteConcreteCode*>(cc);
    
    if (CanBeSpecialized(bcc)) {
      Map *sc = Map::FromWordDirect(closures);
      word cWord = c->ToWord();
      u_int newN = Store::WordToInt(sc->CondGet(cWord, Store::IntToWord(0))) + 1;
      
      if (newN == RECORDS_PER_SPECIALIZE) {
	Specialize(c);
	sc->Remove(cWord);
      }
      else {
	sc->Put(cWord, Store::IntToWord(newN));
      }
    }
  }
  
  if ((calls & REMOVE_OLD_MASK) == 0) {
    RemoveOldStats();
    if (trace && (calls & DUMP_MASK) == 0) {
      DumpStats();
    }
  }
}
  

static word DecSingleStat(word wClosure, word wCount) {
  u_int newCount = Store::WordToInt(wCount) - 1;
  return newCount == 0 ? INVALID_POINTER : Store::IntToWord(newCount);
}


void ByteCodeSpecializer::RemoveOldStats() {
  if (trace) {
    fprintf(stderr, "ByteCodeSpecializer::RemoveOldStats\n");
  }
  Map::FromWordDirect(closures)->PartialMap(DecSingleStat);
}


static void DumpSingleStat(word closure, word wCount) {
  Closure *c = Closure::FromWordDirect(closure);
  ByteConcreteCode *bcc = ByteConcreteCode::FromWordDirect(c->GetConcreteCode());
  u_int count = Store::WordToInt(wCount);
  
  String *name = AbstractCodeInterpreter::MakeProfileName(bcc->GetAbstractCode());
  fprintf(stderr, "    %"U_INTF" records: %s\n", count, name->ExportC());
}


void ByteCodeSpecializer::DumpStats() {
  fprintf(stderr, "ByteCodeSpecializer::DumpStats (calls = %"U_INTF", numSpecialized = %"U_INTF")\n", calls, numSpecialized);
  Map::FromWordDirect(closures)->Apply(DumpSingleStat);
}
  