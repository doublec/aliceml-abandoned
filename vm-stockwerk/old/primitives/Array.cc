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

#include "datalayer/alicedata.hh"

#if defined(INTERFACE)
#pragma implementation "CommonOp.hh"
#endif
#include "CommonOp.hh"
#if defined(INTERFACE)
#pragma implementation "ListOp.hh"
#endif
#include "ListOp.hh"

#include "Array.hh"

namespace Builtins {
  namespace Array {
    word array(word n, word x) {
      int elems = Store::WordToInt(CommonOp::Sync(n));
      Block *a  = (Block *) ::Array::New(elems);

      for (int i = 1; i <= elems; i++) {
	a->InitArg(i, x);
      }

      return a->ToWord();
    }
    word fromList(word l) {
      Block *a = (Block *) ::Array::New(ListOp::Length(l));
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
    word length(word l) {
      l = CommonOp::Sync(l);
      return Store::IntToWord(((::Array *) Store::WordToBlock(l))->GetSize());
    }
    word sub(word a, word i) {
      a = CommonOp::Sync(a);
      i = CommonOp::Sync(i);
      return ((::Array *) Store::WordToBlock(a))->GetArg(Store::WordToInt(i));
    }
    word update(word a, word i, word x) {
      a = CommonOp::Sync(a);
      i = CommonOp::Sync(i);
      ((::Array *) Store::WordToBlock(a))->SetArg(Store::WordToInt(i), x);
      return Store::IntToWord(0); // to be determined
    }
  }
}
