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

#include <ctype.h>

#include "datalayer/alicedata.hh"

#include "CommonOp.hh"
#include "Char.hh"

namespace Builtins {
  namespace Char {
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
    word ord(word a) {
      return CommonOp::Sync(a); // to be determined
    }
    word chr(word a) {
      static int minval = 0; // to be determined
      static int maxval = 0; // to be determined
      int ca            = Store::WordToInt(CommonOp::Sync(a));

      if ((ca >= minval) && (ca <= maxval)) {
	return Store::IntToWord(ca);
      }
      else {
	return Store::IntToWord(0); // to be determined
      }
    }
    word isAlpha(word a) {
      return Store::IntToWord(isalpha(Store::WordToInt(CommonOp::Sync(a))));
    }
    word isAlphaNum(word a) {
      return Store::IntToWord(isalnum(Store::WordToInt(CommonOp::Sync(a))));

    }
    word isCntrl(word a) {
      return Store::IntToWord(iscntrl(Store::WordToInt(CommonOp::Sync(a))));
    }
    word isDigit(word a) {
      return Store::IntToWord(isdigit(Store::WordToInt(CommonOp::Sync(a))));
    }
    word isGraph(word a) {
      return Store::IntToWord(isgraph(Store::WordToInt(CommonOp::Sync(a))));
    }
    word isHexDigit(word a) {
      return Store::IntToWord(isxdigit(Store::WordToInt(CommonOp::Sync(a))));
    }
    word isLower(word a) {
      return Store::IntToWord(islower(Store::WordToInt(CommonOp::Sync(a))));
    }
    word isPrint(word a) {
      return Store::IntToWord(isprint(Store::WordToInt(CommonOp::Sync(a))));
    }
    word isPunct(word a) {
      return Store::IntToWord(ispunct(Store::WordToInt(CommonOp::Sync(a))));
    }
    word isSpace(word a) {
      return Store::IntToWord(isspace(Store::WordToInt(CommonOp::Sync(a))));
    }
    word isUpper(word a) {
      return Store::IntToWord(isupper(Store::WordToInt(CommonOp::Sync(a))));
    }
    word toLower(word a) {
      return Store::IntToWord(tolower(Store::WordToInt(CommonOp::Sync(a))));
    }
    word toUpper(word a) {
      return Store::IntToWord(toupper(Store::WordToInt(CommonOp::Sync(a))));
    }
  }
}
