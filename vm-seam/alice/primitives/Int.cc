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

//--** on many architectures, overflow checks can be much more efficient

#define INT_INT_TO_INT_OP(name, check, op)	\
  DEFINE2(name) {				\
    DECLARE_INT(i, x0);				\
    DECLARE_INT(j, x1);				\
    if (check(i, j)) {				\
      RAISE(PrimitiveTable::General_Overflow);	\
    } else {					\
      RETURN_INT(i op j);			\
    }						\
  } END

#define INT_INT_TO_BOOL_OP(name, op)		\
  DEFINE2(name) {				\
    DECLARE_INT(i, x0);				\
    DECLARE_INT(j, x1);				\
    RETURN_BOOL(i op j);			\
  } END

DEFINE1(Int_opnegate) {
  DECLARE_INT(i, x0);
  if (i == MIN_VALID_INT)
    RAISE(PrimitiveTable::General_Overflow);
  RETURN_INT(-i);
} END

static inline bool CheckSum(s_int i, s_int j) {
  s_int sum = i + j;
  return sum < MIN_VALID_INT || sum > MAX_VALID_INT;
}

INT_INT_TO_INT_OP(Int_opadd, CheckSum, +)

static inline bool CheckDifference(s_int i, s_int j) {
  s_int difference = i - j;
  return difference < MIN_VALID_INT || difference > MAX_VALID_INT;
}

INT_INT_TO_INT_OP(Int_opsub, CheckDifference, -)

static inline bool CheckProduct(s_int i, s_int j) {
  if (j == 0)
    return false;
  else if (j > 0)
    if (i > 0)
      return i > MAX_VALID_INT / j;
    else // i < 0
      return -i > -MIN_VALID_INT / j;
  else // j < 0
    if (i > 0)
      return i > -MIN_VALID_INT / -j;
    else // i < 0
      return -i > MAX_VALID_INT / -j;
}

INT_INT_TO_INT_OP(Int_opmul, CheckProduct, *)

INT_INT_TO_BOOL_OP(Int_opless, <)
INT_INT_TO_BOOL_OP(Int_opgreater, >)
INT_INT_TO_BOOL_OP(Int_oplessEq, <=)
INT_INT_TO_BOOL_OP(Int_opgreaterEq, >=)

DEFINE1(Int_abs) {
  DECLARE_INT(i, x0);
  if (i == MIN_VALID_INT)
    RAISE(PrimitiveTable::General_Overflow);
  RETURN_INT(i >= 0? i: -i);
} END

DEFINE2(Int_compare) {
  DECLARE_INT(i, x0);
  DECLARE_INT(j, x1);
  if (i == j) {
    RETURN_INT(Types::EQUAL);
  } else if (i < j) {
    RETURN_INT(Types::LESS);
  } else { // i > j
    RETURN_INT(Types::GREATER);
  }
} END

DEFINE2(Int_div) {
  DECLARE_INT(i, x0);
  DECLARE_INT(j, x1);
  if (j == 0)
    RAISE(PrimitiveTable::General_Div);
  bool b1 = i >= 0, b2 = j >= 0;
  if (b1 == b2) {
    if (i == MIN_VALID_INT && j == -1)
      RAISE(PrimitiveTable::General_Overflow);
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
      if (j < 0) {
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

DEFINE2(Int_quot) {
  DECLARE_INT(i, x0);
  DECLARE_INT(j, x1);
  if (j == 0)
    RAISE(PrimitiveTable::General_Div);
  if (i == MIN_VALID_INT && j == -1)
    RAISE(PrimitiveTable::General_Overflow);
  RETURN_INT(i / j);
} END

DEFINE2(Int_rem) {
  DECLARE_INT(i, x0);
  DECLARE_INT(j, x1);
  if (j == 0)
    RAISE(PrimitiveTable::General_Div);
  RETURN_INT(i % j);
} END


static word Some(s_int i) {
  TagVal *some = TagVal::New(Types::SOME, 1);
  some->Init(0, Store::IntToWord(i));
  return some->ToWord();
}

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
  Register("Int.maxInt", Some(MAX_VALID_INT));
  Register("Int.minInt", Some(MIN_VALID_INT));
  Register("Int.mod", Int_mod, 2);
  Register("Int.precision", Some(INT_PRECISION));
  Register("Int.quot", Int_quot, 2);
  Register("Int.rem", Int_rem, 2);
}
