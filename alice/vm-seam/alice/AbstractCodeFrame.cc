//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "alice/AbstractCodeFrame.hh"
#endif

#ifdef LIVENESS_DEBUG
#include "alice/AliceConcreteCode.hh"
#endif

#include "alice/AbstractCodeFrame.hh"


#ifdef DEBUG_CHECK
static word dead;
#endif

#ifdef LIVENESS_DEBUG
static const BlockLabel DEAD_LABEL = HASHNODE_LABEL;

static void DisassembleAlice(Closure *closure) {
  AliceConcreteCode *concreteCode =
    AliceConcreteCode::FromWord(closure->GetConcreteCode());
  concreteCode->Disassemble(stderr);
}
#endif


word AbstractCodeFrame::GetLocal(u_int id) {
  Assert(id >= 0 && id < GetSize() - BASE_SIZE - StackFrame::GetBaseSize());
  word value = GetArg(BASE_SIZE + id);
#ifdef LIVENESS_DEBUG
  ::Block *p = Store::WordToBlock(value);
  if (p != INVALID_POINTER) {
    if (p->GetLabel() == DEAD_LABEL) {
      std::fprintf(stderr, "### USING KILLED VALUE ###\n");
      std::fprintf(stderr, "### killed as Local(%"S_INTF")\n",
		   Store::DirectWordToInt(p->GetArg(0)));
      std::fprintf(stderr, "### value before kill:\n");
      Debug::Dump(p->GetArg(1));
      std::fprintf(stderr, "### killed at pc=%p in function:\n",
		   TagVal::FromWordDirect(p->GetArg(2)));
      DisassembleAlice(Closure::FromWordDirect(p->GetArg(3)));
      return p->GetArg(1);
    }
  }
#else
  Assert(value != dead);
#endif
  return value;
}


void AbstractCodeFrame::KillLocal(u_int id, TagVal *pc, Closure *closure) {
  Assert(id >= 0 && id < GetSize() - BASE_SIZE - StackFrame::GetBaseSize());
  word value;
#ifdef LIVENESS_DEBUG
  ::Block *dead = Store::AllocBlock(DEAD_LABEL, 4);
  dead->InitArg(0, Store::IntToWord(id));
  dead->InitArg(1, GetLocal(id));
  dead->InitArg(2, pc->ToWord());
  dead->InitArg(3, closure->ToWord());
  value = dead->ToWord();
#else
#ifdef DEBUG_CHECK
  value = dead;
#else
  value = Store::IntToWord(0);
#endif
#endif
  ReplaceArg(BASE_SIZE + id, value);
}


#ifdef DEBUG_CHECK
void AbstractCodeFrame::Init() {
  dead = String::New("UNINITIALIZED OR DEAD")->ToWord();
  RootSet::Add(dead);
}
#endif
