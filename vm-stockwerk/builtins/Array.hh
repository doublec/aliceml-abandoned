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
#ifndef __ARRAY_HH__
#define __ARRAY_HH__

namespace Builtins {
  namespace Array {
    word array(word n, word x);
    word fromList(word l);
    word length(word a);
    word sub(word a, word i);
    word update(word a, word i, word x);
  }
}

#endif
