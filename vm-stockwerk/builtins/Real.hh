#ifndef __REAL_HH__
#define __REAL_HH__

namespace Builtins {
  namespace Real {
    word opnegate(word a);

    word opadd(word a, word b);
    word opsub(word a, word b);
    word opmul(word a, word b);
    word opdiv(word a, word b);

    word opless(word a, word b);
    word opgreater(word a, word b);
    word oplessEq(word a, word b);
    word opgreaterEq(word a, word b);

    word ceil(word a, word b);
    word compare(word a, word b);
    word floor(word a);
    word fromInt(word a);
    word realCeil(word a);
    word realFloor(word a);
    word realRound(word a);
    word realTrunc(word a);
    word rem(word a, word b);
    word round(word a);
    word toString(word a);
    word trunc(word a);
  }
}

#endif
