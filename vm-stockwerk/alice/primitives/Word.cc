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

#include <cstdio>
#include "emulator/Authoring.hh"

#define WORD_WORD_TO_WORD_OP(name, op)		\
  DEFINE2(name) {				\
    DECLARE_INT(i, x0);				\
    DECLARE_INT(j, x1);				\
    RETURN_INT(i op j);				\
  } END

#define WORD_WORD_TO_BOOL_OP(name, op)		\
  DEFINE2(name) {				\
    DECLARE_INT(i, x0);				\
    DECLARE_INT(j, x1);				\
    RETURN_BOOL(i op j);			\
  } END

//--** operations can be implemented more efficiently by not right-shifting!
//--** else topmost bit has to be truncated (or assertions fail)

WORD_WORD_TO_WORD_OP(Word_opadd, +)
WORD_WORD_TO_WORD_OP(Word_opsub, -)
WORD_WORD_TO_WORD_OP(Word_opmul, *)
WORD_WORD_TO_WORD_OP(Word_opshl, <<)
WORD_WORD_TO_BOOL_OP(Word_opless, <)
WORD_WORD_TO_BOOL_OP(Word_opgreater, >)
WORD_WORD_TO_BOOL_OP(Word_oplessEq, <=)
WORD_WORD_TO_BOOL_OP(Word_opgreaterEq, >=)

DEFINE2(Word_opshr) {
  DECLARE_INT(i, x0);
  DECLARE_INT(j, x1);
  RETURN_INT((i & 0x7FFFFFFF) >> j);
} END

DEFINE2(Word_oparithshr) {
  DECLARE_INT(i, x0);
  DECLARE_INT(j, x1);
  //--** this can be improved on many architectures
  if (i < 0) {
    RETURN_INT((i >> j) | ~(static_cast<unsigned int>(~1) >> j));
  } else {
    RETURN_INT(i >> j);
  }
} END

WORD_WORD_TO_WORD_OP(Word_andb, &)

DEFINE2(Word_div) {
  DECLARE_INT(i, x0);
  DECLARE_INT(j, x1);
  if (j == 0)
    RAISE(PrimitiveTable::General_Div);
  RETURN_INT(i / j);
} END

DEFINE2(Word_fromIntQuote) { //--** should be fromInt (not Quote)
  DECLARE_INT(i, x1);
  RETURN(x1);
} END

DEFINE2(Word_fromWordQuote) { //--** size?
  DECLARE_INT(w, x1);
  RETURN(x1);
} END

DEFINE2(Word_fromWordXQuote) { //--** size?
  DECLARE_INT(w, x1);
  RETURN(x1);
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
    RAISE(PrimitiveTable::General_Overflow);
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

void PrimitiveTable::RegisterWord() {
  Register("Word.+", Word_opadd, 2);
  Register("Word.-", Word_opsub, 2);
  Register("Word.*", Word_opmul, 2);
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
  Register("Word.notb", Word_notb, -1);
  Register("Word.orb", Word_orb, 2);
  Register("Word.toInt", Word_toInt, -1);
  Register("Word.toIntX", Word_toIntX, -1);
  Register("Word.toString", Word_toString, -1);
  Register("Word.wordSize", Store::IntToWord(31));
  Register("Word.xorb", Word_xorb, 2);
}
