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

#include "CommonOp.hh"

namespace Builtins {
  namespace Unsafe {
    namespace Array {
      word sub(word a, word i) {
	a = CommonOp::Sync(a);
	i = CommonOp::Sync(i);
	return Store::WordToBlock(a)->GetArg(1 + Store::WordToInt(i));
      }
      word update(word a, word i, word x) {
	a = CommonOp::Sync(a);
	i = CommonOp::Sync(i);
	Store::WordToBlock(a)->ReplaceArg(1 + Store::WordToInt(i), x);
	return Store::IntToWord(0); // to be determined
      }
    }
    namespace String {
      word sub(word a, word i) {
	int iv = Store::WordToInt(CommonOp::Sync(i));

	a = CommonOp::Sync(a);
	return Store::IntToWord((((::String *) Store::WordToBlock(a))->GetValue())[iv]);
      }
    }
    namespace Vector {
      word sub(word v, word i) {
	v = CommonOp::Sync(v);
	i = CommonOp::Sync(i);
	return Store::WordToBlock(v)->GetArg(1 + Store::WordToInt(i));
      }
    }
    word cast(word a) {
      return a; // to be determined
    }
    word getTag(word a) {
      Block *b;
      a = CommonOp::Sync(a);
      
      if ((b = Store::WordToBlock(a)) != INVALID_POINTER) {
	return Store::IntToWord((int) b->GetLabel());
      }
      else {
	return a; // to be determined
      } 
    }
    word getValue(word a) {
      return a; // to be determined
    }
  }
}
