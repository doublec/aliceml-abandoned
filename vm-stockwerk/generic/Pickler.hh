//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__PICKLER_HH__
#define __GENERIC__PICKLER_HH__

#if defined(INTERFACE)
#pragma interface "generic/Pickler.hh"
#endif

#include "generic/Worker.hh"
#include "generic/String.hh"

class SeamDll Pickler {
public:
  // Exceptions
  static word Sited;

  static void Init();

  static Worker::Result Pack(word x);
  static Worker::Result Save(String *filename, word x);
};

#endif
