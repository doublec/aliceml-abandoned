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

#include <cmath>

#include "builtins/Authoring.hh"

#define REAL_TO_REAL(name, op)				\
  DEFINE1(name) {					\
    DECLARE_REAL(real, x0);				\
    RETURN(Real::New(op(real->GetValue()))->ToWord());	\
  } END

#define REAL_TO_INT(name, op)					\
  DEFINE1(name) {						\
    DECLARE_REAL(real, x0);					\
    RETURN_INT(static_cast<int>(op(real->GetValue())));		\
  } END

#define INT_TO_REAL(name, op)					\
  DEFINE1(name) {						\
    DECLARE_INT(i, x0);						\
    RETURN(Real::New(static_cast<double>(op(i)))->ToWord());	\
  } END

#define REAL_REAL_TO_REAL_OP(name, op)					 \
  DEFINE2(name) {							 \
    DECLARE_REAL(real1, x0);						 \
    DECLARE_REAL(real2, x1);						 \
    RETURN(Real::New(real1->GetValue() op real2->GetValue())->ToWord()); \
  } END

#define REAL_REAL_TO_BOOL_OP(name, op)				\
  DEFINE2(name) {						\
    DECLARE_REAL(real1, x0);					\
    DECLARE_REAL(real2, x1);					\
    RETURN_BOOL(real1->GetValue() op real2->GetValue());	\
  } END

#define REAL_REAL_TO_INT(name, op)				\
  DEFINE2(name) {						\
    DECLARE_REAL(real1, x0);					\
    DECLARE_REAL(real2, x1);					\
    double result = op(real1->GetValue(), real2->GetValue());	\
    RETURN_INT(static_cast<int>(result));			\
  } END

static inline double Trunc(double x) {
  if (x >= 0.0)
    return std::floor(x);
  else
    return std::ceil(x);
}

REAL_TO_REAL(Real_opnegate, -)
REAL_REAL_TO_REAL_OP(Real_opadd, +)
REAL_REAL_TO_REAL_OP(Real_opsub, +)
REAL_REAL_TO_REAL_OP(Real_opmul, +)
REAL_REAL_TO_REAL_OP(Real_opdiv, +)
REAL_REAL_TO_BOOL_OP(Real_opless, <)
REAL_REAL_TO_BOOL_OP(Real_opgreater, >)
REAL_REAL_TO_BOOL_OP(Real_oplessEq, <=)
REAL_REAL_TO_BOOL_OP(Real_opgreaterEq, >=)
REAL_TO_INT(Real_ceil, std::ceil)

DEFINE2(Real_compare) {
  DECLARE_REAL(real1, x0);
  DECLARE_REAL(real2, x1);
  double x = real1->GetValue();
  double y = real2->GetValue();
  if (x == y) {
    RETURN_INT(0);   // EQUAL
  } else if (x < y) {
    RETURN_INT(2);   // LESS
  } else { // x > y
    RETURN_INT(1);   // GREATER
  }
} END

REAL_TO_INT(Real_floor, std::floor)
INT_TO_REAL(Real_fromInt, /*identity*/)
REAL_TO_REAL(Real_realCeil, std::ceil)
REAL_TO_REAL(Real_realFloor, std::floor)
REAL_TO_REAL(Real_realRound, std::rint)
REAL_TO_REAL(Real_realTrunc, Trunc)
REAL_REAL_TO_INT(Real_rem, std::fmod)
REAL_TO_INT(Real_round, std::rint)

DEFINE1(Real_toString) {
  //--** not elegant; string is traversed twice
  static char buf[50];
  DECLARE_REAL(real, x0);
  std::sprintf(buf, "%g", real->GetValue());
  RETURN(String::New(buf)->ToWord());
} END

REAL_TO_INT(Real_trunc, Trunc)

void Primitive::RegisterReal() {
  Register("Real.~", Real_opnegate, 1);
  Register("Real.+", Real_opadd, 2);
  Register("Real.-", Real_opsub, 2);
  Register("Real.*", Real_opmul, 2);
  Register("Real./", Real_opdiv, 2);
  Register("Real.<", Real_opless, 2);
  Register("Real.>", Real_opgreater, 2);
  Register("Real.<=", Real_oplessEq, 2);
  Register("Real.>=", Real_opgreaterEq, 2);
  Register("Real.ceil", Real_ceil, 1);
  Register("Real.compare", Real_compare, 2);
  Register("Real.floor", Real_floor, 1);
  Register("Real.fromInt", Real_fromInt, 1);
  Register("Real.precision", Store::IntToWord(52));
  Register("Real.realCeil", Real_realCeil, 1);
  Register("Real.realFloor", Real_realFloor, 1);
  Register("Real.realRound", Real_realRound, 1);
  Register("Real.realTrunc", Real_realTrunc, 1);
  Register("Real.rem", Real_rem, 2);
  Register("Real.round", Real_round, 1);
  Register("Real.toString", Real_toString, 1);
  Register("Real.trunc", Real_trunc, 1);
};
