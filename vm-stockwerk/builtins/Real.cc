#include <cmath>

#include "store.hh"
#include "alicedata.hh"
#include "CommonOp.hh"
#include "Real.hh"

namespace Builtins {
  namespace Real {
    word opnegate(word a) {
      a = CommonOp::Sync(a);
      return ::Real::New(-((::Real *) Store::WordToBlock(a))->GetValue())->ToWord();
    }
    word opadd(word a, word b) {
      double x = ((::Real *) Store::WordToBlock(CommonOp::Sync(a)))->GetValue();
      double y = ((::Real *) Store::WordToBlock(CommonOp::Sync(b)))->GetValue();
      return ::Real::New(x + y)->ToWord();
    }
    word opsub(word a, word b) {
      double x = ((::Real *) Store::WordToBlock(CommonOp::Sync(a)))->GetValue();
      double y = ((::Real *) Store::WordToBlock(CommonOp::Sync(b)))->GetValue();
      return ::Real::New(x - y)->ToWord();
    }
    word opmul(word a, word b) {
      double x = ((::Real *) Store::WordToBlock(CommonOp::Sync(a)))->GetValue();
      double y = ((::Real *) Store::WordToBlock(CommonOp::Sync(b)))->GetValue();
      return ::Real::New(x * y)->ToWord();
    }
    word opdiv(word a, word b) {
      double x = ((::Real *) Store::WordToBlock(CommonOp::Sync(a)))->GetValue();
      double y = ((::Real *) Store::WordToBlock(CommonOp::Sync(b)))->GetValue();
      return ::Real::New(x / y)->ToWord();
    }
    word opless(word a, word b) {
      double x = ((::Real *) Store::WordToBlock(CommonOp::Sync(a)))->GetValue();
      double y = ((::Real *) Store::WordToBlock(CommonOp::Sync(b)))->GetValue();
      return Store::IntToWord(x < y);
    }
    word opgreater(word a, word b) {
      double x = ((::Real *) Store::WordToBlock(CommonOp::Sync(a)))->GetValue();
      double y = ((::Real *) Store::WordToBlock(CommonOp::Sync(b)))->GetValue();
      return Store::IntToWord(x > y);
    }
    word oplessEq(word a, word b) {
      double x = ((::Real *) Store::WordToBlock(CommonOp::Sync(a)))->GetValue();
      double y = ((::Real *) Store::WordToBlock(CommonOp::Sync(b)))->GetValue();
      return Store::IntToWord(x <= y);
    }
    word opgreaterEq(word a, word b) {
      double x = ((::Real *) Store::WordToBlock(CommonOp::Sync(a)))->GetValue();
      double y = ((::Real *) Store::WordToBlock(CommonOp::Sync(b)))->GetValue();
      return Store::IntToWord(x >= y);
    }
    word ceil(word a) {
      double x = ((::Real *) Store::WordToBlock(CommonOp::Sync(a)))->GetValue();
      return Store::IntToWord((int) std::ceil(x));
    }
    word compare(word a, word b) {
      double x = ((::Real *) Store::WordToBlock(CommonOp::Sync(a)))->GetValue();
      double y = ((::Real *) Store::WordToBlock(CommonOp::Sync(b)))->GetValue();
      // to be determined
      if (x == y) {
	return Store::IntToWord(0); // EQUAL
      }
      else if (x < y) {
	return Store::IntToWord(0); // LESS
      }
      else {
	return Store::IntToWord(0); // GREATER
      }
    }
    word floor(word a) {
      double x = ((::Real *) Store::WordToBlock(CommonOp::Sync(a)))->GetValue();
      return Store::IntToWord((int) std::floor(x));
    }
    word fromInt(word a) {
      return ::Real::New((double) Store::WordToInt(CommonOp::Sync(a)))->ToWord();
    }
    word realCeil(word a) {
      double x = ((::Real *) Store::WordToBlock(CommonOp::Sync(a)))->GetValue();
      return ::Real::New(std::ceil(x))->ToWord();
    }
    word realFloor(word a) {
      double x = ((::Real *) Store::WordToBlock(CommonOp::Sync(a)))->GetValue();
      return ::Real::New(std::floor(x))->ToWord();
    }
    word realRound(word a) {
      double x = ((::Real *) Store::WordToBlock(CommonOp::Sync(a)))->GetValue();
      return ::Real::New(std::rint(x))->ToWord();
    }
    word realTrunc(word a) {
      double x = ((::Real *) Store::WordToBlock(CommonOp::Sync(a)))->GetValue();

      if (x >= 0.0) {
	return ::Real::New(std::floor(x))->ToWord();
      }
      else {
	return ::Real::New(std::ceil(x))->ToWord();
      }
    }
    word rem(word a, word b) {
      double x = ((::Real *) Store::WordToBlock(CommonOp::Sync(a)))->GetValue();
      double y = ((::Real *) Store::WordToBlock(CommonOp::Sync(b)))->GetValue();
      // to be determined
      return ::Real::New((double) (x - (((long long) (x / y)) * y)))->ToWord();
    }
    word toString(word a) {
      static char buf[50];
      double x = ((::Real *) Store::WordToBlock(CommonOp::Sync(a)))->GetValue();
      std::sprintf(buf, "%g", x);
      return String::New(buf)->ToWord();
    }
  }
}
