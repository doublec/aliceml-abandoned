#include "store.hh"
#include "alicedata.hh"
#include "CommonOp.hh"
#include "Word.hh"

namespace Builtins {
  namespace Word {
    word fromIntQuote(word a) {
      return a; // to be determined
    }
    word fromInt(word a) {
      return Store::IntToWord(Store::WordToInt(CommonOp::Sync(a)));
    }
    word toInt(word a) {
      return Store::IntToWord(Store::WordToInt(CommonOp::Sync(a)));
    }
    word toIntX(word a) {
      return Store::IntToWord(Store::WordToInt(CommonOp::Sync(a)));
    }
    word opadd(word a, word b) {
      return Store::IntToWord(Store::WordToInt(CommonOp::Sync(a)) +
			      Store::WordToInt(CommonOp::Sync(b)));
    }
    word opsub(word a, word b) {
      return Store::IntToWord(Store::WordToInt(CommonOp::Sync(a)) -
			      Store::WordToInt(CommonOp::Sync(b)));
    }
    word opmul(word a, word b) {
      return Store::IntToWord(Store::WordToInt(CommonOp::Sync(a)) *
			      Store::WordToInt(CommonOp::Sync(b)));
    }
    word opdiv(word a, word b) {
      // exception handling to be determined
      return Store::IntToWord(Store::WordToInt(CommonOp::Sync(a)) /
			      Store::WordToInt(CommonOp::Sync(b)));
    }
    word mod(word a, word b) {
      // exception handling to be determined
      int ai = Store::WordToInt(CommonOp::Sync(a));
      int bi = Store::WordToInt(CommonOp::Sync(b));
      return Store::IntToWord((ai - ((int) (ai / bi))));
    }
    word orb(word a, word b) {
      int ai = Store::WordToInt(CommonOp::Sync(a));
      int bi = Store::WordToInt(CommonOp::Sync(b));
      return Store::IntToWord(ai | bi);
    }
    word xorb(word a, word b) {
      int ai = Store::WordToInt(CommonOp::Sync(a));
      int bi = Store::WordToInt(CommonOp::Sync(b));
      return Store::IntToWord(ai ^ bi);
    }
    word andb(word a, word b) {
      int ai = Store::WordToInt(CommonOp::Sync(a));
      int bi = Store::WordToInt(CommonOp::Sync(b));
      return Store::IntToWord(ai & bi);
    }
    word notb(word a) {
      return Store::IntToWord(~Store::WordToInt(CommonOp::Sync(a)));
    }
    word shl(word a, word b) {
      int ai = Store::WordToInt(CommonOp::Sync(a));
      int bi = Store::WordToInt(CommonOp::Sync(b));
      return Store::IntToWord(ai << bi);
    }
    word shr(word a, word b) {
      int ai = Store::WordToInt(CommonOp::Sync(a));
      int bi = Store::WordToInt(CommonOp::Sync(b));
      return Store::IntToWord(((u_int) ai) >> bi);
    }
    word arithshr(word a, word b) {
      int ai = Store::WordToInt(CommonOp::Sync(a));
      int bi = Store::WordToInt(CommonOp::Sync(b));
      return Store::IntToWord(ai >> bi); // to be determined
    }
    word toString(word a) {
      static char buf[200];
      
      std::sprintf(buf, "%d", Store::WordToInt(CommonOp::Sync(a)));
      return String::New(buf)->ToWord();
    }
  }
}
