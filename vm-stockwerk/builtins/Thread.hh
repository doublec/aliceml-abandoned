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
#ifndef __THREAD_HH__
#define __THREAD_HH__

namespace Builtins {
  namespace Thread {
    word terminate(word a);
    word current(word a);
    word isSuspended(word a);
    word raiseIn(word a, word b);
    word resume(word a);
    word state(word a);
    word suspend(word a);
    word yield(word a);
  }
}

#endif
