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
