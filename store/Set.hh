//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//
#ifndef __STORE__SET_HH__
#define __STORE__SET_HH__

#if defined(INTERFACE)
#pragma interface "store/Set.hh"
#endif

class Set : private DynamicBlock {
protected:
  Set *Enlarge(u_int oldsize, u_int newsize, u_int gen) {
    DynamicBlock *p = Store::AllocDynamicBlock(newsize, 0, gen);
    std::memcpy(p->GetBase(), GetBase(), (oldsize + 1) * sizeof(u_int));
    return STATIC_CAST(Set *, p);
  }
public:
  using DynamicBlock::InitArg;
  using DynamicBlock::GetArg;
  using DynamicBlock::GetArgUnchecked;
  using DynamicBlock::ToWord;

  u_int GetSize() {
    return GetActiveSize();
  }
  void Clear() {
    SetActiveSize(0);
  }
  Set *Add(word v, u_int gen = STORE_GEN_OLDEST) {
    u_int top = GetActiveSize();
    u_int max = DynamicBlock::GetSize();
    Set *p    = (((top + 1) < max) ? this : Enlarge(max, (max * 3) >> 1, gen));
    p->SetActiveSize(top + 1);
    p->InitArg(top, v);
    return p;
  }
  void AddUnchecked(word v) {
    u_int top = GetActiveSize();
    SetActiveSize(top + 1);
    InitArg(top, v);
  }
  Set *Export(u_int gen) {
    u_int size = GetActiveSize();
    return Enlarge(size, size, gen);
  }

  static Set *New(u_int s, u_int gen = STORE_GEN_OLDEST) {
    return STATIC_CAST(Set *, Store::AllocDynamicBlock(s, 0, gen));
  }
  static Set *FromWordDirect(word x) {
    return STATIC_CAST(Set *, DynamicBlock::FromWordDirect(x));
  }
};

#endif
