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
#include "store.hh"
#include "alicedata.hh"
#include "CommonOp.hh"
#include "ListOp.hh"
#include "Vector.hh"

namespace Builtins {
  namespace Vector {
    word fromList(word l) {
      Block *a = (Block *) ::Vector::New(ListOp::Length(l));
      int i    = 1;

      while (true) {
	Block *cell = Store::WordToBlock(CommonOp::Sync(l));

	if (cell != INVALID_POINTER) {
	  a->InitArg(i++, ListOp::Car(cell));
	  l = ListOp::Cdr(cell);
	}
	else {
	  return a->ToWord();
	}
      }
    }
    word length(word v) {
      return Store::IntToWord(((::Vector *) Store::WordToBlock(CommonOp::Sync(v)))->GetSize());
    }
    word sub(word v, word i) {
      v = CommonOp::Sync(v);
      i = CommonOp::Sync(i);
      return ((::Vector *) Store::WordToBlock(v))->GetValue(Store::WordToInt(i));
    }
  }
}
