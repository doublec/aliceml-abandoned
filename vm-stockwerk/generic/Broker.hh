//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__BROKER_HH__
#define __GENERIC__BROKER_HH__

#if defined(INTERFACE)
#pragma interface "generic/Broker.hh"
#endif

#include "adt/ChunkMap.hh"
#include "generic/UniqueString.hh"
#include "generic/Worker.hh"

class SeamDll Broker {
public:
  // Exceptions:
  static word BrokerError;

  static void Init();

  static void Start(String *languageId, int argc, char *argv[]);
  static Worker::Result Load(String *languageId, String *key);

  static void Register(String *name, word value);
  static word Lookup(String *name);
};

#endif
