#ifndef __WORD_HH__
#define __WORD_HH__

namespace Builtins {
  namespace Word {
    word fromIntQuote(word a);
    word fromInt(word a);

    word toInt(word a);
    word toIntX(word a);

    word opadd(word a, word b);
    word opsub(word a, word b);
    word opmul(word a, word b);
    word opdiv(word a, word b);

    word mod(word a, word b);
    word orb(word a, word b);
    word xorb(word a, word b);
    word andb(word a, word b);
    word notb(word a);

    word shl(word a, word b);
    word shr(word a, word b);
    word arithshr(word a, word b);

    word toString(word a);
  }
}

#endif
