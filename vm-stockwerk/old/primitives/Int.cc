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

#include <cstdlib>

#include "builtins/Authoring.hh"

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
      RAISE(GlobalPrimitives::General_Div);	\
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
    RAISE(GlobalPrimitives::General_Div);
  int b1 = i >= 0;
  int b2 = j >= 0;
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
    RAISE(GlobalPrimitives::General_Div);
  int c = i % j;
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

DEFINE1(Int_toString) {
  //--** not elegant: string is traversed twice
  static char buf[20];
  DECLARE_INT(i, x0);
  std::sprintf(buf, "%d", i);
  RETURN(String::New(buf)->ToWord());
} END

void Primitive::RegisterInt() {
  Register("Int.~", Int_opnegate);
  Register("Int.+", Int_opadd);
  Register("Int.-", Int_opsub);
  Register("Int.*", Int_opmul);
  Register("Int.<", Int_opless);
  Register("Int.>", Int_opgreater);
  Register("Int.<=", Int_oplessEq);
  Register("Int.>=", Int_opgreaterEq);
  Register("Int.abs", Int_abs);
  Register("Int.compare", Int_compare);
  Register("Int.div", Int_div);
  Register("Int.maxInt", Store::IntToWord(0x3FFFFFFF));
  Register("Int.minInt", Store::IntToWord(-0x40000000));
  Register("Int.mod", Int_mod);
  Register("Int.precision", Store::IntToWord(31));
  Register("Int.quot", Int_quot);
  Register("Int.rem", Int_rem);
  Register("Int.toString", Int_toString);
};
