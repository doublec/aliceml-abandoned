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

#include <cstdio>
#include "alice/primitives/Authoring.hh"

//--** overflow checking missing everywhere

#define INT_INT_TO_INT_OP(name, op)		\
  DEFINE2(name) {				\
    DECLARE_INT(i, x0);				\
    DECLARE_INT(j, x1);				\
    RETURN_INT(i op j);				\
  } END

#define INT_INT_TO_INT_OP_DIV(name, op)		\
  DEFINE2(name) {				\
    DECLARE_INT(i, x0);				\
    DECLARE_INT(j, x1);				\
    if (j == 0)					\
      RAISE(PrimitiveTable::General_Div);	\
    RETURN_INT(i op j);				\
  } END

#define INT_INT_TO_BOOL_OP(name, op)		\
  DEFINE2(name) {				\
    DECLARE_INT(i, x0);				\
    DECLARE_INT(j, x1);				\
    RETURN_BOOL(i op j);			\
  } END

DEFINE1(Int_opnegate) {
  DECLARE_INT(i, x0);
  RETURN_INT(-i);
} END

INT_INT_TO_INT_OP(Int_opadd, +)
INT_INT_TO_INT_OP(Int_opsub, -)
INT_INT_TO_INT_OP(Int_opmul, *)
INT_INT_TO_BOOL_OP(Int_opless, <)
INT_INT_TO_BOOL_OP(Int_opgreater, >)
INT_INT_TO_BOOL_OP(Int_oplessEq, <=)
INT_INT_TO_BOOL_OP(Int_opgreaterEq, >=)

DEFINE1(Int_abs) {
  DECLARE_INT(i, x0);
  RETURN_INT(std::abs(i));
} END

DEFINE2(Int_compare) {
  DECLARE_INT(i, x0);
  DECLARE_INT(j, x1);
  if (i == j) {
    RETURN_INT(0);   // EQUAL
  } else if (i < j) {
    RETURN_INT(2);   // LESS
  } else { // i > j
    RETURN_INT(1);   // GREATER
  }
} END

DEFINE2(Int_div) {
  DECLARE_INT(i, x0);
  DECLARE_INT(j, x1);
  if (j == 0)
    RAISE(PrimitiveTable::General_Div);
  bool b1 = i >= 0, b2 = j >= 0;
  if (b1 == b2) {
    RETURN_INT(i / j);
  } else if (b2) {
    RETURN_INT((i - j + 1) / j);
  } else {
    RETURN_INT((i - j - 1) / j);
  }
} END

DEFINE2(Int_mod) {
  DECLARE_INT(i, x0);
  DECLARE_INT(j, x1);
  if (j == 0)
    RAISE(PrimitiveTable::General_Div);
  s_int c = i % j;
  if (c == 0) {
    RETURN_INT(c);
  } else {
    if (c < 0) {
      if (j <= 0) {
	RETURN_INT(c);
      } else {
	RETURN_INT(c + j);
      }
    } else {
      if (j < 0) {
	RETURN_INT(c + j);
      } else {
	RETURN_INT(c);
      }
    }
  }
} END

INT_INT_TO_INT_OP_DIV(Int_quot, /)
INT_INT_TO_INT_OP_DIV(Int_rem, %)

void PrimitiveTable::RegisterInt() {
  Register("Int.~", Int_opnegate, 1);
  Register("Int.+", Int_opadd, 2);
  Register("Int.-", Int_opsub, 2);
  Register("Int.*", Int_opmul, 2);
  Register("Int.<", Int_opless, 2);
  Register("Int.>", Int_opgreater, 2);
  Register("Int.<=", Int_oplessEq, 2);
  Register("Int.>=", Int_opgreaterEq, 2);
  Register("Int.abs", Int_abs, 1);
  Register("Int.compare", Int_compare, 2);
  Register("Int.div", Int_div, 2);
  Register("Int.maxInt", Store::IntToWord(MAX_VALID_INT));
  Register("Int.minInt", Store::IntToWord(MIN_VALID_INT));
  Register("Int.mod", Int_mod, 2);
  Register("Int.precision", Store::IntToWord(INT_PRECISION));
  Register("Int.quot", Int_quot, 2);
  Register("Int.rem", Int_rem, 2);
}
