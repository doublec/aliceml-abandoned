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

#include <ctype.h>
#include "alice/Authoring.hh"

#define INT_INT_TO_BOOL_OP(name, op)	\
  DEFINE2(name) {			\
    DECLARE_INT(c1, x0);		\
    DECLARE_INT(c2, x1);		\
    RETURN_BOOL(c1 op c2);		\
  } END

#define CHAR_TO_BOOL(name, test)	\
  DEFINE1(name) {			\
    DECLARE_INT(c, x0);			\
    RETURN_BOOL(test(c));		\
  } END

INT_INT_TO_BOOL_OP(Char_opless, <)
INT_INT_TO_BOOL_OP(Char_opgreater, >)
INT_INT_TO_BOOL_OP(Char_oplessEq, <=)
INT_INT_TO_BOOL_OP(Char_opgreaterEq, >=)

DEFINE1(Char_chr) {
  DECLARE_INT(c, x0);
  if (STATIC_CAST(u_char, c) != c) RAISE(PrimitiveTable::General_Chr);
  RETURN(x0);
} END

CHAR_TO_BOOL(Char_isAlpha, isalpha)
CHAR_TO_BOOL(Char_isAlphaNum, isalnum)
CHAR_TO_BOOL(Char_isCntrl, iscntrl)
CHAR_TO_BOOL(Char_isDigit, isdigit)
CHAR_TO_BOOL(Char_isGraph, isgraph)
CHAR_TO_BOOL(Char_isHexDigit, isxdigit)
CHAR_TO_BOOL(Char_isLower, islower)
CHAR_TO_BOOL(Char_isPrint, isprint)
CHAR_TO_BOOL(Char_isPunct, ispunct)
CHAR_TO_BOOL(Char_isSpace, isspace)
CHAR_TO_BOOL(Char_isUpper, isupper)

DEFINE1(Char_ord) {
  DECLARE_INT(c, x0);
  RETURN(x0);
} END

DEFINE1(Char_toLower) {
  DECLARE_INT(c, x0);
  RETURN_INT(tolower(c));
} END

DEFINE1(Char_toUpper) {
  DECLARE_INT(c, x0);
  RETURN_INT(toupper(c));
} END

void PrimitiveTable::RegisterChar() {
  Register("Char.<", Char_opless, 2);
  Register("Char.>", Char_opgreater, 2);
  Register("Char.<=", Char_oplessEq, 2);
  Register("Char.>=", Char_opgreaterEq, 2);
  Register("Char.ord", Char_ord, 1);
  Register("Char.chr", Char_chr, 1);
  Register("Char.isAlpha", Char_isAlpha, 1);
  Register("Char.isAlphaNum", Char_isAlphaNum, 1);
  Register("Char.isCntrl", Char_isCntrl, 1);
  Register("Char.isDigit", Char_isDigit, 1);
  Register("Char.isGraph", Char_isGraph, 1);
  Register("Char.isHexDigit", Char_isHexDigit, 1);
  Register("Char.isLower", Char_isLower, 1);
  Register("Char.isPrint", Char_isPrint, 1);
  Register("Char.isPunct", Char_isPunct, 1);
  Register("Char.isSpace", Char_isSpace, 1);
  Register("Char.isUpper", Char_isUpper, 1);
  Register("Char.toLower", Char_toLower, 1);
  Register("Char.toUpper", Char_toUpper, 1);
}
