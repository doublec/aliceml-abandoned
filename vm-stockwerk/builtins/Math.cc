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

#define REAL_TO_REAL(name, op)					\
  DEFINE1(name) {						\
    DECLARE_REAL(real, x0);					\
    RETURN(Real::New(op(real->GetValue()))->ToWord());		\
  } END

#define REAL_REAL_TO_REAL(name, op)					   \
  DEFINE2(name) {							   \
    DECLARE_REAL(real1, x0);						   \
    DECLARE_REAL(real2, x1);						   \
    RETURN(Real::New(op(real1->GetValue(), real2->GetValue()))->ToWord()); \
  } END

REAL_TO_REAL(Math_acos, std::acos)
REAL_TO_REAL(Math_acosh, std::acosh)
REAL_TO_REAL(Math_asin, std::asin)
REAL_TO_REAL(Math_asinh, std::asinh)
REAL_TO_REAL(Math_atan, std::atanh)
REAL_TO_REAL(Math_atanh, std::atanh)
REAL_REAL_TO_REAL(Math_atan2, std::atan2);
REAL_TO_REAL(Math_cos, std::cos)
REAL_TO_REAL(Math_cosh, std::cosh)
REAL_TO_REAL(Math_exp, std::exp)
REAL_TO_REAL(Math_ln, std::log)
REAL_REAL_TO_REAL(Math_pow, std::pow);
REAL_TO_REAL(Math_sin, std::sin);
REAL_TO_REAL(Math_sinh, std::sinh);
REAL_TO_REAL(Math_sqrt, std::sqrt);
REAL_TO_REAL(Math_tan, std::tan);
REAL_TO_REAL(Math_tanh, std::tanh);

void Primitive::RegisterMath() {
  Register("Math.acos", Math_acos);
  Register("Math.acosh", Math_acosh);
  Register("Math.asin", Math_asin);
  Register("Math.asinh", Math_asinh);
  Register("Math.atan", Math_atan);
  Register("Math.atanh", Math_atanh);
  Register("Math.atan2", Math_atan2);
  Register("Math.cos", Math_cos);
  Register("Math.cosh", Math_cosh);
  Register("Math.e", Real::New(2.71828182846)->ToWord());
  Register("Math.exp", Math_exp);
  Register("Math.ln", Math_ln);
  Register("Math.pi", Real::New(3.14159265359)->ToWord());
  Register("Math.pow", Math_pow);
  Register("Math.sin", Math_sin);
  Register("Math.sinh", Math_sinh);
  Register("Math.sqrt", Math_sqrt);
  Register("Math.tan", Math_tan);
  Register("Math.tanh", Math_tanh);
};
