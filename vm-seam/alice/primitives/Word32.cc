//
// Author:
//   Guido Tack <tack@ps.uni-sb.de>
// 
// Copyright:
//   Guido Tack, 2003
// 
// Last change:
//   $Date$ by $Author$
//   $Revision$
// 

#include "alice/Authoring.hh"

#define WORD_PRECISION 32
#define STANDARD_WORD_PRECISION 31
#define STANDARD_WORD_NONBITS (STORE_WORD_WIDTH - STANDARD_WORD_PRECISION)
#define STANDARD_WORD_NONBITS_EXP (1 << STANDARD_WORD_NONBITS)

#define DECLARE_WORDN(w, x, n)						\
  u_int w = Store::WordToInt(x);					\
  if (static_cast<s_int>(w) == INVALID_INT) { REQUEST(x); } else {}	\
  w &= static_cast<u_int>(-1) >> (STORE_WORD_WIDTH - n);

#define DECLARE_STANDARD_WORD(w, x) \
  DECLARE_WORDN(w, x, STANDARD_WORD_PRECISION)

inline int mydiv(signed int a, signed int b) {
  // This function is only here to bypass a constant folding bug in g++.
  // If we define RETURN_WORD as
  //   RETURN_INT(static_cast<s_int>((w) * NONBITS_EXP) / NONBITS_EXP)
  // then RETURN_WORD(0x80000000) evaluates to RETURN_WORD(0x80000000)
  // instead of RETURN_WORD(0).
  return a / b;
}

// #define RETURN_LARGE_WORD(w)						\
//   RETURN_INT(mydiv(static_cast<s_int>((w) * LARGE_WORD_NONBITS_EXP),	\
// 		   LARGE_WORD_NONBITS_EXP))

#define WORD32_WORD_TO_WORD_OP(name, op)        \
  DEFINE2(name) {				\
    DECLARE_WORD32(i, x0);			\
    DECLARE_WORD32(j, x1);			\
    RETURN_WORD32(i op j);			\
  } END

#define WORD32_WORD_TO_BOOL_OP(name, op)	\
  DEFINE2(name) {				\
    DECLARE_WORD32(i, x0);			\
    DECLARE_WORD32(j, x1);			\
    RETURN_BOOL(i op j);			\
  } END

WORD32_WORD_TO_WORD_OP(Word32_opadd, +)
WORD32_WORD_TO_WORD_OP(Word32_opsub, -)
WORD32_WORD_TO_WORD_OP(Word32_opmul, *)

DEFINE1(Word32_opneg) {
  DECLARE_WORD32(i, x0);
  RETURN_WORD32(-i);
} END

WORD32_WORD_TO_BOOL_OP(Word32_opless, <)
WORD32_WORD_TO_BOOL_OP(Word32_opgreater, >)
WORD32_WORD_TO_BOOL_OP(Word32_oplessEq, <=)
WORD32_WORD_TO_BOOL_OP(Word32_opgreaterEq, >=)

DEFINE2(Word32_opshl) {
  DECLARE_WORD32(i, x0);
  DECLARE_STANDARD_WORD(j, x1);
  if (j > WORD_PRECISION) j = WORD_PRECISION; // see comment in Word.icc
  RETURN_WORD32(i << j);
} END

DEFINE2(Word32_opshr) {
  DECLARE_WORD32(i, x0);
  DECLARE_STANDARD_WORD(j, x1);
  if (j > WORD_PRECISION) j = WORD_PRECISION; // see comment in Word.icc
  RETURN_WORD32(i >> j);
} END

DEFINE2(Word32_oparithshr) {
  DECLARE_WORD32(i, x0);
  DECLARE_STANDARD_WORD(j, x1);

  //--** this implementation can be improved on many architectures
  if (i & (1 << (WORD_PRECISION - 1))) {
    RETURN_WORD32((i >> j) | ~(static_cast<u_int>(-1) >> j));
  } else {
    RETURN_WORD32(i >> j);
  }
} END

WORD32_WORD_TO_WORD_OP(Word32_andb, &)

DEFINE2(Word32_div) {
  DECLARE_WORD32(i, x0);
  DECLARE_WORD32(j, x1);
  if (j == 0)
    RAISE(PrimitiveTable::General_Div);
  RETURN_WORD32(i / j);
} END

DEFINE1(Word32_fromInt) {
  DECLARE_INT(i, x0);
  RETURN_WORD32(i);
} END

DEFINE1(Word32_fromLargeWord) {
  RETURN(x0);
} END

WORD32_WORD_TO_WORD_OP(Word32_mod, %)

DEFINE1(Word32_notb) {
  DECLARE_WORD32(i, x0);
  RETURN_WORD32(~i);
} END

WORD32_WORD_TO_WORD_OP(Word32_orb, |)

DEFINE1(Word32_toInt) {
  DECLARE_WORD32(i, x0);
  if (i > static_cast<u_int>(MAX_VALID_INT))
    RAISE(PrimitiveTable::General_Overflow);
  RETURN_INT(i);
} END

DEFINE1(Word32_toIntX) {
  DECLARE_WORD32(i, x0);
  if (i & (1 << (WORD_PRECISION - 1))) {
    RETURN_INT(i | ~(static_cast<u_int>(-1)));
  } else {
    RETURN_INT(i);
  }
} END

DEFINE1(Word32_fromLargeInt) {
  TEST_INTINF(i, x0);
  if (i!=INVALID_INT)
    RETURN_WORD32(i);
  DECLARE_INTINF(ii, x0);
  RETURN_WORD32(mpz_get_ui(ii->big()));
} END

DEFINE1(Word32_toLargeInt) {
  DECLARE_WORD32(i, x0);
  BigInt *b = BigInt::New(i);
  RETURN_INTINF(b);
} END

DEFINE1(Word32_toLargeIntX) {
  DECLARE_WORD32(i, x0);
  BigInt *b;
  if (i & (1 << (WORD_PRECISION - 1))) {
    b = BigInt::New(i | ~(STATIC_CAST(u_int, -1)));
  } else {
    b = BigInt::New(i);
  }
  RETURN_INTINF(b);
} END

DEFINE1(Word32_fromLarge) {
  RETURN(x0);
} END

DEFINE1(Word32_toLarge) {
  RETURN(x0);
} END

DEFINE1(Word32_toLargeX) {
  DECLARE_WORD32(i, x0);
  if (i & (1 << (WORD_PRECISION - 1))) {
    RETURN_WORD32(i | ~(static_cast<u_int>(-1)));
  } else {
    RETURN(x0);
  }
} END

WORD32_WORD_TO_WORD_OP(Word32_xorb, ^)

void PrimitiveTable::RegisterWord32() {
  Register("Word32.+", Word32_opadd, 2);
  Register("Word32.-", Word32_opsub, 2);
  Register("Word32.*", Word32_opmul, 2);
  Register("Word32.~", Word32_opneg, 1);
  Register("Word32.<", Word32_opless, 2);
  Register("Word32.>", Word32_opgreater, 2);
  Register("Word32.<=", Word32_oplessEq, 2);
  Register("Word32.>=", Word32_opgreaterEq, 2);
  Register("Word32.<<", Word32_opshl, 2);
  Register("Word32.>>", Word32_opshr, 2);
  Register("Word32.~>>", Word32_oparithshr, 2);
  Register("Word32.andb", Word32_andb, 2);
  Register("Word32.div", Word32_div, 2);
  Register("Word32.fromInt", Word32_fromInt, 1);
  Register("Word32.fromLargeInt", Word32_fromLargeInt, 1);
  Register("Word32.fromLarge", Word32_fromLarge, 1);
  Register("Word32.mod", Word32_mod, 2);
  Register("Word32.notb", Word32_notb, 1);
  Register("Word32.orb", Word32_orb, 2);
  Register("Word32.toInt", Word32_toInt, 1);
  Register("Word32.toIntX", Word32_toIntX, 1);
  Register("Word32.toLargeInt", Word32_toLargeInt, 1);
  Register("Word32.toLargeIntX", Word32_toLargeIntX, 1);
  Register("Word32.toLarge", Word32_toLarge, 1);
  Register("Word32.toLargeX", Word32_toLargeX, 1);
  Register("Word32.xorb", Word32_xorb, 2);
}
