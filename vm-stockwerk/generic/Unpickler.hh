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

#ifndef __GENERIC__UNPICKLER_HH__
#define __GENERIC__UNPICKLER_HH__

#if defined(INTERFACE)
#pragma interface "generic/Unpickler.hh"
#endif

#include "generic/Worker.hh"
#include "generic/String.hh"

class SeamDll Unpickler {
public:
  // Exceptions
  static word Corrupt;

  static void Init();

  typedef word (*handler)(word);
  static void RegisterHandler(String *name, handler handler);

  static Worker::Result Unpack(String *string);
  static Worker::Result Load(String *filename);
};

#endif
