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
#include <cstdio>

#include "store.hh"
#include "alicedata.hh"
#include "CommonOp.hh"

#if defined(INTERFACE)
#pragma implementation "Builtins.hh"
#endif
#include "Builtins.hh"

static word CompareSubs(Block *a, Block *b) {
  int size = a->GetSize();

  for (int i = 1; i <= size; i++) {
    if (!Store::WordToInt(::Builtins::opeq(a->GetArg(i), b->GetArg(i)))) {
      return Store::IntToWord(0);
    }
  }
  return Store::IntToWord(1);
}

namespace Builtins {
  word opeq(word a, word b) {
    a = CommonOp::Sync(a);
    b = CommonOp::Sync(b);

    Block *ab = Store::WordToBlock(a);
    Block *bb = Store::WordToBlock(b);
    
    if ((ab != INVALID_POINTER) && (bb != INVALID_POINTER)) {
      t_label l = ab->GetLabel();

      if ((l == AliceLabel::Cell)) {
	return Store::IntToWord(ab == bb);
      }
      else if (l == AliceLabel::Array) {
	return CompareSubs(ab, bb);
      }
      else if (l == AliceLabel::ConVal) {
	if (bb->GetLabel() == AliceLabel::ConVal) {
	  return CompareSubs(ab, bb);
	}
	return Store::IntToWord(0);
      }
      else if (l == bb->GetLabel()) {
	return CompareSubs(ab, bb);
      }
      return Store::IntToWord(0);
    }
    else {
      Transient *at = Store::WordToTransient(a);
      Transient *bt = Store::WordToTransient(b);

      if ((at != INVALID_POINTER) && (bt != INVALID_POINTER)) {
	return Store::IntToWord(0); // to be determined
      }
      else {
	return Store::IntToWord(a == b);
      }
    }
  }
  word opnoteq(word a, word b) {
    return Store::IntToWord(1 - Store::WordToInt(opeq(a, b)));
  }
}
