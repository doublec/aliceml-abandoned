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
  signed int v = i << 1;
  RETURN_INT(v >> (j + 1));
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
  Register("Word.+", Word_opadd);
  Register("Word.-", Word_opsub);
  Register("Word.*", Word_opmul);
  Register("Word.<<", Word_opshl);
  Register("Word.>>", Word_opshr);
  Register("Word.~>>", Word_oparithshr);
  Register("Word.andb", Word_andb);
  Register("Word.div", Word_div);
  Register("Word.fromInt'", Word_fromIntQuote);
  Register("Word.mod", Word_mod);
  Register("Word.notb", Word_notb);
  Register("Word.orb", Word_orb);
  Register("Word.toInt", Word_toInt);
  Register("Word.toIntX", Word_toIntX);
  Register("Word.toString", Word_toString);
  Register("Word.wordSize", Store::IntToWord(31));
  Register("Word.xorb", Word_xorb);
};
