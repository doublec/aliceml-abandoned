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
#ifndef __HOLE_HH__
#define __HOLE_HH__

namespace Builtins {
  namespace Hole {
    word fail(word a, word b);
    word fill(word a, word b);
    word future(word a);
    word hole(word a);
    word isFailed(word a);
  }
}

#endif
