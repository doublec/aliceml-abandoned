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
#ifndef __INT_HH__
#define __INT_HH__

namespace Builtins {
  namespace Int {
    word opnegate(word a);

    word opadd(word a, word b);
    word opsub(word a, word b);
    word opmul(word a, word b);

    word opless(word a, word b);
    word opgreater(word a, word b);
    word oplessEq(word a, word b);
    word opgreaterEq(word a, word b);

    word abs(word a);
    word compare(word a, word b);
    word div(word a, word b);
    word mod(word a, word b);
    word quot(word a, word b);
    word rem(word a, word b);
    word toString(word a);
  }
}

#endif
