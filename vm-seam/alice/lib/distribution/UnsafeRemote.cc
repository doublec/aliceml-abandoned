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

#if defined(__MINGW32__) || defined(_MSC_VER)
#include <cstring>
#include <cstdlib>
#include <winsock.h>
#else
#include <netdb.h>
#include <sys/utsname.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <cstring>
#include <cstdlib>
#endif

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

DEFINE1(UnsafeRemote_setCallback) {
  Assert(AliceLanguageLayer::remoteCallback == Store::IntToWord(0));
  AliceLanguageLayer::remoteCallback = x0;
  RETURN_UNIT;
} END

DEFINE1(UnsafeRemote_packValue) {
  PUSH_PRIM_SELF();
  return Pickler::Pack(x0);
} END

DEFINE1(UnsafeRemote_unpackValue) {
  DECLARE_STRING(packedValue, x0);
  PUSH_PRIM_SELF();
  return Unpickler::Unpack(packedValue);
} END

AliceDll word UnsafeRemote() {
  Record *record = Record::New(4);
  INIT_STRUCTURE(record, "UnsafeRemote", "getLocalIP",
		 UnsafeRemote_getLocalIP, 0);
  INIT_STRUCTURE(record, "UnsafeRemote", "setCallback",
		 UnsafeRemote_setCallback, 1);
  INIT_STRUCTURE(record, "UnsafeRemote", "packValue",
		 UnsafeRemote_packValue, 1);
  INIT_STRUCTURE(record, "UnsafeRemote", "unpackValue",
		 UnsafeRemote_unpackValue, 1);
  RETURN_STRUCTURE("UnsafeRemote$", record);
}
