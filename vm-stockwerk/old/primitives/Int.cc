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

#include <cstdlib>

#include "datalayer/alicedata.hh"

#include "CommonOp.hh"
#include "Int.hh"

namespace Builtins {
  namespace Int {
    word opnegate(word a) {
      // overflow checking to be determined
      return Store::IntToWord(-Store::WordToInt(CommonOp::Sync(a)));
    }
    word opadd(word a, word b) {
      // overflow checking
      return Store::IntToWord(Store::WordToInt(CommonOp::Sync(a)) +
			      Store::WordToInt(CommonOp::Sync(b)));
    }
    word opsub(word a, word b) {
      // overflow checking
      return Store::IntToWord(Store::WordToInt(CommonOp::Sync(a)) -
			      Store::WordToInt(CommonOp::Sync(b)));
    }
    word opmul(word a, word b) {
      // overflow checking
      return Store::IntToWord(Store::WordToInt(CommonOp::Sync(a)) *
			      Store::WordToInt(CommonOp::Sync(b)));
    }
    word opless(word a, word b) {
      return Store::IntToWord(Store::WordToInt(CommonOp::Sync(a)) <
			      Store::WordToInt(CommonOp::Sync(b)));
    }
    word opgreater(word a, word b) {
      return Store::IntToWord(Store::WordToInt(CommonOp::Sync(a)) >
			      Store::WordToInt(CommonOp::Sync(b)));
    }
    word oplessEq(word a, word b) {
      return Store::IntToWord(Store::WordToInt(CommonOp::Sync(a)) <=
			      Store::WordToInt(CommonOp::Sync(b)));
    }
    word opgreaterEq(word a, word b) {
      return Store::IntToWord(Store::WordToInt(CommonOp::Sync(a)) >=
			      Store::WordToInt(CommonOp::Sync(b)));
    }
    word abs(word a) {
      // overflow checking
      return Store::IntToWord(std::abs(Store::WordToInt(CommonOp::Sync(a))));
    }
    word compare(word a, word b) {
      int ai = Store::WordToInt(CommonOp::Sync(a));
      int bi = Store::WordToInt(CommonOp::Sync(b));
      // to be determined
      if (ai == bi) {
	return Store::IntToWord(0); // EQUAL
      }
      else if (ai < bi) {
	return Store::IntToWord(0); // LESS
      }
      else {
	return Store::IntToWord(0); // GREATER
      }
    }
    word div(word a, word b) {
      int ai = Store::WordToInt(CommonOp::Sync(a));
      int bi = Store::WordToInt(CommonOp::Sync(b));
      int b1 = (ai >= 0);
      int b2 = (bi >= 0);
      // overflow checking

      if (b1 == b2) {
	return Store::IntToWord((ai / bi));
      }
      else if (b2) {
	return Store::IntToWord(((ai - bi + 1) / bi));
      }
      else {
	return Store::IntToWord(((ai - bi - 1) / bi));
      }
    }
    word mod(word a, word b) {
      int ai = Store::WordToInt(CommonOp::Sync(a));
      int bi = Store::WordToInt(CommonOp::Sync(b));
      int c  = (ai % bi);
      // overflow checking

      if (c == 0) {
	return Store::IntToWord(c);
      }
      else {
	if (c < 0) {
	  if (bi <= 0) {
	    return Store::IntToWord(c);
	  }
	  else {
	    return Store::IntToWord((c + bi));
	  }
	}
	else {
	  if (bi < 0) {
	    return Store::IntToWord((c + bi));
	  }
	  else {
	    return Store::IntToWord(c);
	  }
	}
      }
    }
    word quot(word a, word b) {
      // overflow checking
      return Store::IntToWord(Store::WordToInt(CommonOp::Sync(a)) /
			      Store::WordToInt(CommonOp::Sync(b)));
    }
    word rem(word a, word b) {
      // overflow checking
      return Store::IntToWord(Store::WordToInt(CommonOp::Sync(a)) %
			      Store::WordToInt(CommonOp::Sync(b)));
    }
    word toString(word a) {
      static char buf[20];

      std::sprintf(buf, "%d", Store::WordToInt(CommonOp::Sync(a)));
      return String::New(buf)->ToWord();
    }
  }
}
