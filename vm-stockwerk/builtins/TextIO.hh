#ifndef __TEXTIO_HH__
#define __TEXTIO_HH__

namespace Builtins {
  namespace TextIO {
    word openIn(word a);
    word inputAll(word a);
    word inputLine(word a);
    word closeIn(word a);
    word openOut(word a);
    word output(word a, word b);
    word output1(word a, word b);
    word flushOut(word a);
    word closeOut(word a);
    word print(word a);
  }
}

#endif
