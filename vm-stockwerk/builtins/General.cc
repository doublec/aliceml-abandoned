#include "store.hh"
#include "alicedata.hh"
#include "CommonOp.hh"
#include "General.hh"
#
namespace Builtins {
  namespace General {
    word assign(word c, word v) {
      Cell *cv = (Cell *) Store::WordToBlock(CommonOp::Sync(c));
      cv->SetValue(v);
      return Store::IntToWord(0); // to be determined
    }
    word exchange(word c, word nv) {
      Cell *cv = (Cell *) Store::WordToBlock(CommonOp::Sync(c));
      return cv->Exchange(nv);
    }
    word exnName(word a) {
      return a; // to be determined
    }
  }
}
