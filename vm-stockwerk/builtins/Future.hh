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
#ifndef __FUTURE_HH__
#define __FUTURE_HH__

namespace Builtins {
  namespace Future {
    word alarmQuote(word a);
    word await(word a);
    word awaitOne(word a);
    word byneed(word a);
    word concur(word a);
    word isFailed(word a);
    word isFuture(word a);
  }
}

#endif
