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
    return static_cast<Set *>(p);
  }
public:
  using DynamicBlock::InitArg;
  using DynamicBlock::GetArg;
  using DynamicBlock::GetArgUnchecked;
  using DynamicBlock::ToWord;

  u_int GetSize() {
    return GetScanSize();
  }
  void Clear() {
    SetScanSize(0);
  }
  Set *Add(word v, u_int gen = STORE_GEN_OLDEST) {
    u_int top = GetScanSize();
    u_int max = DynamicBlock::GetSize();
    Set *p    = ((top < max) ? this : Enlarge(max, (max * 3) >> 1, gen));
    p->SetScanSize(top + 1);
    p->InitArg(top, v);
    return p;
  }
  void AddUnchecked(word v) {
    u_int top = GetScanSize();
    SetScanSize(top + 1);
    InitArg(top, v);
  }
  Set *Export(u_int gen) {
    u_int size = GetScanSize();
    return Enlarge(size, size, gen);
  }

  static Set *New(u_int s, u_int gen = STORE_GEN_OLDEST) {
    return static_cast<Set *>(Store::AllocDynamicBlock(s, 0, gen));
  }
  static Set *FromWordDirect(word x) {
    return static_cast<Set *>(DynamicBlock::FromWordDirect(x));
  }
};

#endif
