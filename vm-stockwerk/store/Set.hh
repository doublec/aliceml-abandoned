//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
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

class Set : private Block {
private:
  static const u_int TOP_POS = 0;

  Set *Enlarge(u_int oldsize, u_int newsize) {
    Block *p = Store::AllocBlock(GENSET_LABEL, newsize);

    std::memcpy(p->GetBase(), GetBase(), oldsize * sizeof(word));
    return (Set *) p;
  }
public:
  using Block::InitArg;
  using Block::GetArg;
  using Block::ToWord;

  u_int GetSize() {
    return (u_int) (Store::DirectWordToInt(GetArg(TOP_POS)) - 1);
  }
  void MakeEmpty() {
    ((Block *) this)->InitArg(TOP_POS, 1);
  }
  Set *Push(word v) {
    u_int top = Store::DirectWordToInt(GetArg(TOP_POS));
    u_int max = Block::GetSize();
    Set *p    = ((top < max) ? this : Enlarge(max, (max * 3) >> 1));

    p->InitArg(TOP_POS, Store::IntToWord(top + 1));
    p->InitArg(top, v);
    return p;
  }

  static Set *New(u_int s) {
    Block *p = Store::AllocBlock(GENSET_LABEL, (s + 1));

    p->InitArg(TOP_POS, Store::IntToWord((TOP_POS + 1)));
    return (Set *) p;
  }
  static Set *FromWord(word x) {
    Block *p = Store::DirectWordToBlock(x);

    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == GENSET_LABEL));
    return (Set *) p;
  }
};

#endif __STORE__SET_HH__
