//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cmath>
#include "java/Authoring.hh"

#define DOUBLE_TO_DOUBLE_OP(name, op)			\
  DEFINE2(name) {					\
    DECLARE_DOUBLE(a, x0); x1 = x1;			\
    DRETURN(Double::New(op(a->GetValue()))->ToWord());	\
  } END

DOUBLE_TO_DOUBLE_OP(sin, std::sin)
DOUBLE_TO_DOUBLE_OP(cos, std::cos)
DOUBLE_TO_DOUBLE_OP(tan, std::tan)
DOUBLE_TO_DOUBLE_OP(asin, std::asin)
DOUBLE_TO_DOUBLE_OP(acos, std::acos)
DOUBLE_TO_DOUBLE_OP(atan, std::atan)
DOUBLE_TO_DOUBLE_OP(exp, std::exp)
DOUBLE_TO_DOUBLE_OP(log, std::log)
DOUBLE_TO_DOUBLE_OP(sqrt, std::sqrt)
DOUBLE_TO_DOUBLE_OP(ceil, std::ceil)
DOUBLE_TO_DOUBLE_OP(floor, std::floor)
DOUBLE_TO_DOUBLE_OP(rint, rint) //--** std::

void NativeMethodTable::java_lang_StrictMath(JavaString *className) {
  Register(className, "sin", "(D)D", sin, 2, false);
  Register(className, "cos", "(D)D", cos, 2, false);
  Register(className, "tan", "(D)D", tan, 2, false);
  Register(className, "asin", "(D)D", asin, 2, false);
  Register(className, "acos", "(D)D", acos, 2, false);
  Register(className, "atan", "(D)D", atan, 2, false);
  Register(className, "exp", "(D)D", exp, 2, false);
  Register(className, "log", "(D)D", log, 2, false);
  Register(className, "sqrt", "(D)D", sqrt, 2, false);
  Register(className, "ceil", "(D)D", ceil, 2, false);
  Register(className, "floor", "(D)D", floor, 2, false);
  Register(className, "rint", "(D)D", rint, 2, false);
}
