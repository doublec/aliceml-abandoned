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
#ifndef __STRING_HH__
#define __STRING_HH__

namespace Builtins {
  namespace String {
    word append(word a, word b);

    word opless(word a, word b);
    word opgreater(word a, word b);
    word oplessEq(word a, word b);
    word opgreaterEq(word a, word b);

    word compare(word a, word b);
    word explode(word a);
    word implode(word a);
    word size(word a);
    word sub(word a, word b);
    word substring(word a, word b, word c);
    word str(word a);
  }
}

#endif
