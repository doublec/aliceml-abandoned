//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cstring>
#include <cstdlib>
#if defined(__MINGW32__) || defined(_MSC_VER)
#include <winsock.h>
#else
#include <netdb.h>
#include <sys/utsname.h>
#include <arpa/inet.h>
#endif

#include "generic/RootSet.hh"
#include "generic/Pickler.hh"
#include "generic/Unpickler.hh"
#include "alice/Authoring.hh"

DEFINE0(UnsafeRemote_getLocalIP) {
#if defined(__MINGW32__) || defined(_MSC_VER)
  char nodeName[256];
  int ret = gethostname(nodeName, sizeof(nodeName));
  if (ret < 0) {
    Error("GetLocalIP");
  }
#else
  utsname unameBuffer;
  int ret = uname(&unameBuffer);
  if (ret < 0) {
    Error("GetLocalIP");
  }
  char *nodeName = &unameBuffer.nodename[0];
#endif

  hostent *entry = gethostbyname(nodeName);
  if (!entry) {
    entry = gethostbyname("localhost");
    if (!entry) {
      Error("GetLocalIP");
    }
  }

  in_addr addr;
  std::memcpy(&addr, entry->h_addr_list[0], sizeof(addr));
  RETURN(String::New(inet_ntoa(addr))->ToWord());
} END

static word callback;

DEFINE1(UnsafeRemote_setCallback) {
  callback = x0;
  RETURN_UNIT;
} END

DEFINE2(UnsafeRemote_dynamicCall) {
  if (callback == Store::IntToWord(0)) {
    RAISE(PrimitiveTable::Hole_Hole); //--** to be done
  } else {
    Scheduler::nArgs = 2;
    Scheduler::currentArgs[0] = x0;
    Scheduler::currentArgs[1] = x1;
    return Scheduler::PushCall(callback);
  }
} END

DEFINE1(UnsafeRemote_packValue) {
  Scheduler::PushFrameNoCheck(prim_self);
  return Pickler::Pack(x0);
} END

DEFINE1(UnsafeRemote_unpackValue) {
  DECLARE_STRING(packedValue, x0);
  Scheduler::PushFrameNoCheck(prim_self);
  return Unpickler::Unpack(packedValue);
} END

word UnsafeRemote() {
  RootSet::Add(callback);
  callback = Store::IntToWord(0);

  //--** enter UnsafeRemote_dynamicCall into the PrimitiveTable
  Record *record = Record::New(5);
  INIT_STRUCTURE(record, "UnsafeRemote", "getLocalIP",
		 UnsafeRemote_getLocalIP, 0, true);
  INIT_STRUCTURE(record, "UnsafeRemote", "setCallback",
		 UnsafeRemote_setCallback, 1, true);
  INIT_STRUCTURE(record, "UnsafeRemote", "dynamicCall",
		 UnsafeRemote_dynamicCall, 2, false);
  INIT_STRUCTURE(record, "UnsafeRemote", "packValue",
		 UnsafeRemote_packValue, 1, true);
  INIT_STRUCTURE(record, "UnsafeRemote", "unpackValue",
		 UnsafeRemote_unpackValue, 1, true);
  RETURN_STRUCTURE("UnsafeRemote$", record);
}
