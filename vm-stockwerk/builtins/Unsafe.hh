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
#ifndef __UNSAFE_HH__
#define __UNSAFE_HH__

namespace Builtins {
  namespace Unsafe {
    namespace Array {
      word sub(word a, word i);
      word update(word a, word i, word x);
    }
    namespace String {
      word sub(word a, word i);
    }
    namespace Vector {
      word sub(word v, word i);
    }
    word cast(word a);
    word getTag(word a);
    word getValue(word a);
  }
}

#endif
