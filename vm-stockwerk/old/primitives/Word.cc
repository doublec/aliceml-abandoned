//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include "builtins/Authoring.hh"

#define WORD_WORD_TO_WORD_OP(name, op)		\
  DEFINE2(name) {				\
    DECLARE_INT(i, x0);				\
    DECLARE_INT(j, x1);				\
    RETURN_INT(i op j);				\
  } END

//--** operations can be implemented more efficiently by not right-shifting!
//--** else topmost bit has to be truncated (or assertions fail)

WORD_WORD_TO_WORD_OP(Word_opadd, +)
WORD_WORD_TO_WORD_OP(Word_opsub, -)
WORD_WORD_TO_WORD_OP(Word_opmul, *)
WORD_WORD_TO_WORD_OP(Word_opshl, <<)

DEFINE2(Word_opshr) {
  DECLARE_INT(i, x0);
  DECLARE_INT(j, x1);
  RETURN_INT((i & 0x7FFFFFFF) >> j);
} END

DEFINE2(Word_oparithshr) {
  DECLARE_INT(i, x0);
  DECLARE_INT(j, x1);
  //--** this can be improved on many architectures
  if (i & (1 << 31)) {
    RETURN_INT((i >> j) | 1 << 31);
  } else {
    RETURN_INT(i >> j);
  }
} END

WORD_WORD_TO_WORD_OP(Word_andb, &)

DEFINE2(Word_div) {
  DECLARE_INT(i, x0);
  DECLARE_INT(j, x1);
  if (j == 0)
    RAISE(GlobalPrimitives::General_Div);
  RETURN_INT(i / j);
} END

DEFINE2(Word_fromIntQuote) { //--** should be fromInt (not Quote)
  DECLARE_INT(i, x0);
  RETURN(x0);
} END

WORD_WORD_TO_WORD_OP(Word_mod, %)

DEFINE1(Word_notb) {
  DECLARE_INT(i, x0);
  RETURN_INT(~i);
} END

WORD_WORD_TO_WORD_OP(Word_orb, |)

DEFINE1(Word_toInt) {
  DECLARE_INT(i, x0);
  if (i < 0)
    RAISE(GlobalPrimitives::General_Overflow);
  RETURN(x0);
} END

DEFINE1(Word_toIntX) {
  DECLARE_INT(i, x0);
  RETURN(x0);
} END

DEFINE1(Word_toString) {
  //--** inelegant; string is traversed twice
  static char buf[200];
  DECLARE_INT(i, x0);
  std::sprintf(buf, "%x", i);
  RETURN(String::New(buf)->ToWord());
} END

WORD_WORD_TO_WORD_OP(Word_xorb, ^)

void Primitive::RegisterWord() {
  Register("Word.+", Word_opadd, 2);
  Register("Word.-", Word_opsub, 2);
  Register("Word.*", Word_opmul, 2);
  Register("Word.<<", Word_opshl, 2);
  Register("Word.>>", Word_opshr, 2);
  Register("Word.~>>", Word_oparithshr, 2);
  Register("Word.andb", Word_andb, 2);
  Register("Word.div", Word_div, 2);
  Register("Word.fromInt'", Word_fromIntQuote, 2);
  Register("Word.mod", Word_mod, 2);
  Register("Word.notb", Word_notb, 1);
  Register("Word.orb", Word_orb, 2);
  Register("Word.toInt", Word_toInt, 1);
  Register("Word.toIntX", Word_toIntX, 1);
  Register("Word.toString", Word_toString, 1);
  Register("Word.wordSize", Store::IntToWord(31));
  Register("Word.xorb", Word_xorb, 2);
}
