//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cmath>

#include "datalayer/alicedata.hh"

#include "CommonOp.hh"
#include "Math.hh"

namespace Builtins {
  namespace Math {
    word acos(word a) {
      a = CommonOp::Sync(a);
      return Real::New(std::acosh(((Real *) Store::WordToBlock(a))->GetValue()))->ToWord();
    }
    word acosh(word a) {
      a = CommonOp::Sync(a);
      return Real::New(std::acosh(((Real *) Store::WordToBlock(a))->GetValue()))->ToWord();
    }
    word asin(word a) {
      a = CommonOp::Sync(a);
      return Real::New(std::asin(((Real *) Store::WordToBlock(a))->GetValue()))->ToWord();
    }
    word asinh(word a) {
      a = CommonOp::Sync(a);
      return Real::New(std::asinh(((Real *) Store::WordToBlock(a))->GetValue()))->ToWord();
    }
    word atan(word a) {
      a = CommonOp::Sync(a);
      return Real::New(std::atan(((Real *) Store::WordToBlock(a))->GetValue()))->ToWord();
    }
    word atanh(word a) {
      a = CommonOp::Sync(a);
      return Real::New(std::atanh(((Real *) Store::WordToBlock(a))->GetValue()))->ToWord();
    }
    word atan2(word a, word b) {
      double y = ((Real *) Store::WordToBlock(CommonOp::Sync(a)))->GetValue();
      double x = ((Real *) Store::WordToBlock(CommonOp::Sync(b)))->GetValue();
      return Real::New(std::atan2(y, x))->ToWord();
    }
    word cos(word a) {
      a = CommonOp::Sync(a);
      return Real::New(std::cos(((Real *) Store::WordToBlock(a))->GetValue()))->ToWord();
    }
    word cosh(word a) {
      a = CommonOp::Sync(a);
      return Real::New(std::cosh(((Real *) Store::WordToBlock(a))->GetValue()))->ToWord();
    }
    word exp(word a) {
      a = CommonOp::Sync(a);
      return Real::New(std::exp(((Real *) Store::WordToBlock(a))->GetValue()))->ToWord();
    }
    word ln(word a) {
      a = CommonOp::Sync(a);
      return Real::New(std::log(((Real *) Store::WordToBlock(a))->GetValue()))->ToWord();
    }
    word pow(word a, word b) {
      double x = ((Real *) Store::WordToBlock(CommonOp::Sync(CommonOp::Sync(a))))->GetValue();
      double y = ((Real *) Store::WordToBlock(CommonOp::Sync(b)))->GetValue();
      return Real::New(std::pow(x, y))->ToWord();
    }
    word sin(word a) {
      a = CommonOp::Sync(a);
      return Real::New(std::sin(((Real *) Store::WordToBlock(a))->GetValue()))->ToWord();
    }
    word sinh(word a) {
      a = CommonOp::Sync(a);
      return Real::New(std::sinh(((Real *) Store::WordToBlock(a))->GetValue()))->ToWord();
    }
    word sqrt(word a) {
      a = CommonOp::Sync(a);
      return Real::New(std::sqrt(((Real *) Store::WordToBlock(a))->GetValue()))->ToWord();
    }
    word tan(word a) {
      a = CommonOp::Sync(a);
      return Real::New(std::tan(((Real *) Store::WordToBlock(a))->GetValue()))->ToWord();
    }
    word tanh(word a) {
      a = CommonOp::Sync(a);
      return Real::New(std::tanh(((Real *) Store::WordToBlock(a))->GetValue()))->ToWord();
    }
  }
}
