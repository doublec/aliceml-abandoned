//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//   Leif Kornstaedt, 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "alice/Authoring.hh"

//--** ops implementable more efficiently directly on tagged representation

#define WORD_PRECISION INT_PRECISION

#define NONBITS (STORE_WORD_WIDTH - WORD_PRECISION)
#define NONBITS_EXP (1 << NONBITS)

inline int mydiv(signed int a, signed int b) {
  // This function is only here to bypass a constant folding bug in g++.
  // If we define RETURN_WORD as
  //   RETURN_INT(static_cast<s_int>((w) * NONBITS_EXP) / NONBITS_EXP)
  // then RETURN_WORD(0x80000000) evaluates to RETURN_WORD(0x80000000)
  // instead of RETURN_WORD(0).
  return a / b;
}

#define DECLARE_WORD(w, x)						\
  u_int w = Store::WordToInt(x);					\
  if (static_cast<s_int>(w) == INVALID_INT) { REQUEST(x); } else {}	\
  w &= static_cast<u_int>(-1) >> NONBITS;
#define RETURN_WORD(w) \
  RETURN_INT(mydiv(static_cast<s_int>((w) * NONBITS_EXP), NONBITS_EXP))

#define WORD_WORD_TO_WORD_OP(name, op)		\
  DEFINE2(name) {				\
    DECLARE_WORD(i, x0);			\
    DECLARE_WORD(j, x1);			\
    RETURN_WORD(i op j);			\
  } END

#define WORD_WORD_TO_BOOL_OP(name, op)		\
  DEFINE2(name) {				\
    DECLARE_WORD(i, x0);			\
    DECLARE_WORD(j, x1);			\
    RETURN_BOOL(i op j);			\
  } END

WORD_WORD_TO_WORD_OP(Word_opadd, +)
WORD_WORD_TO_WORD_OP(Word_opsub, -)
WORD_WORD_TO_WORD_OP(Word_opmul, *)

DEFINE1(Word_opneg) {
  DECLARE_WORD(i, x0);
  RETURN_WORD(-i);
} END

WORD_WORD_TO_BOOL_OP(Word_opless, <)
WORD_WORD_TO_BOOL_OP(Word_opgreater, >)
WORD_WORD_TO_BOOL_OP(Word_oplessEq, <=)
WORD_WORD_TO_BOOL_OP(Word_opgreaterEq, >=)

// In the shifting operations, we need to check whether
// the shift count exceeds WORD_PRECISION explicitly:
// gcc on x86 generates code that modulos the shift count by WORD_SIZE.
// (Although this seems to contradict the ISO standard.)

DEFINE2(Word_opshl) {
  DECLARE_WORD(i, x0);
  DECLARE_WORD(j, x1);
  if (j > WORD_PRECISION) j = WORD_PRECISION; // see above
  RETURN_WORD(i << j);
} END

DEFINE2(Word_opshr) {
  DECLARE_WORD(i, x0);
  DECLARE_WORD(j, x1);
  if (j > WORD_PRECISION) j = WORD_PRECISION; // see above
  RETURN_WORD(i >> j);
} END

DEFINE2(Word_oparithshr) {
  DECLARE_WORD(i, x0);
  DECLARE_WORD(j, x1);
  if (j > WORD_PRECISION - 1) j = WORD_PRECISION - 1; // see above
  //--** this implementation can be improved on many architectures
  if (i & (1 << (WORD_PRECISION - 1))) {
    RETURN_WORD((i >> j) | ~(static_cast<u_int>(-1) >> (j + NONBITS)));
  } else {
    RETURN_WORD(i >> j);
  }
} END

WORD_WORD_TO_WORD_OP(Word_andb, &)

DEFINE2(Word_div) {
  DECLARE_WORD(i, x0);
  DECLARE_WORD(j, x1);
  if (j == 0)
    RAISE(PrimitiveTable::General_Div);
  RETURN_WORD(i / j);
} END

DEFINE2(Word_fromIntQuote) { //--** should be fromInt (not Quote)
  DECLARE_INT(i, x1);
  RETURN_WORD(i);
} END

DEFINE2(Word_fromWordQuote) { //--** size?
  DECLARE_WORD(w, x1);
  RETURN_WORD(w);
} END

DEFINE2(Word_fromWordXQuote) { //--** size?
  DECLARE_WORD(w, x1);
  RETURN_WORD(w);
} END

WORD_WORD_TO_WORD_OP(Word_mod, %)

DEFINE1(Word_notb) {
  DECLARE_WORD(i, x0);
  RETURN_WORD(~i);
} END

WORD_WORD_TO_WORD_OP(Word_orb, |)

DEFINE1(Word_toInt) {
  DECLARE_WORD(i, x0);
  if (i > static_cast<u_int>(MAX_VALID_INT))
    RAISE(PrimitiveTable::General_Overflow);
  RETURN_WORD(i);
} END

DEFINE1(Word_toIntX) {
  DECLARE_WORD(i, x0);
  if (i & (1 << (WORD_PRECISION - 1))) {
    RETURN_WORD(i | ~(static_cast<u_int>(-1) >> NONBITS));
  } else {
    RETURN_WORD(i);
  }
} END

WORD_WORD_TO_WORD_OP(Word_xorb, ^)

void PrimitiveTable::RegisterWord() {
  Register("Word.+", Word_opadd, 2);
  Register("Word.-", Word_opsub, 2);
  Register("Word.*", Word_opmul, 2);
  Register("Word.~", Word_opneg, 1);
  Register("Word.<", Word_opless, 2);
  Register("Word.>", Word_opgreater, 2);
  Register("Word.<=", Word_oplessEq, 2);
  Register("Word.>=", Word_opgreaterEq, 2);
  Register("Word.<<", Word_opshl, 2);
  Register("Word.>>", Word_opshr, 2);
  Register("Word.~>>", Word_oparithshr, 2);
  Register("Word.andb", Word_andb, 2);
  Register("Word.div", Word_div, 2);
  Register("Word.fromInt'", Word_fromIntQuote, 2);
  Register("Word.fromWord'", Word_fromWordQuote, 2);
  Register("Word.fromWordX'", Word_fromWordXQuote, 2);
  Register("Word.mod", Word_mod, 2);
  Register("Word.notb", Word_notb, 1);
  Register("Word.orb", Word_orb, 2);
  Register("Word.toInt", Word_toInt, 1);
  Register("Word.toIntX", Word_toIntX, 1);
  Register("Word.wordSize", Store::IntToWord(WORD_PRECISION));
  Register("Word.xorb", Word_xorb, 2);
}
