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

#include "datalayer/alicedata.hh"

#include "CommonOp.hh"
#include "General.hh"
#
namespace Builtins {
  namespace General {
    word assign(word c, word v) {
      Cell *cv = (Cell *) Store::WordToBlock(CommonOp::Sync(c));
      cv->Assign(v);
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
