//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Contributor:
//   Andreas Rossberg <rossberg@ps.uni-sb.de>
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
#include <netinet/in.h>
#include <arpa/inet.h>
#include <cstring>
#include <cstdlib>
#endif

#include "alice/Authoring.hh"

static word SitedArgumentConstructor;
static word SitedResultConstructor;
static word ProxyConstructor;
static word ProtocolConstructor;

DEFINE1(UnsafeComponent_Proxy) {
  ConVal *conVal =
    ConVal::New(Store::DirectWordToBlock(ProxyConstructor), 1);
  conVal->Init(0, x0);
  RETURN(conVal->ToWord());
} END

DEFINE1(UnsafeComponent_Protocol) {
  ConVal *conVal =
    ConVal::New(Store::DirectWordToBlock(ProtocolConstructor), 1);
  conVal->Init(0, x0);
  RETURN(conVal->ToWord());
} END

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

static word SitedConstructor;

AliceDll word UnsafeRemote() {
  SitedArgumentConstructor =
    UniqueConstructor::New("SitedArgument", "Remote.SitedArgument")->ToWord();
  RootSet::Add(SitedArgumentConstructor);
  SitedResultConstructor =
    UniqueConstructor::New("SitedResult", "Remote.SitedResult")->ToWord();
  RootSet::Add(SitedResultConstructor);
  ProxyConstructor =
    UniqueConstructor::New("Proxy", "Remote.Proxy")->ToWord();
  RootSet::Add(ProxyConstructor);
  ProtocolConstructor =
    UniqueConstructor::New("Protocol", "Remote.Protocol")->ToWord();
  RootSet::Add(ProtocolConstructor);

  Record *record = Record::New(16);
  record->Init("'SitedInternal", Pickler::Sited);
  record->Init("SitedInternal", Pickler::Sited);
  record->Init("'CorruptInternal", Unpickler::Corrupt);
  record->Init("CorruptInternal", Unpickler::Corrupt);
  record->Init("'SitedArgument", SitedArgumentConstructor);
  record->Init("SitedArgument", SitedArgumentConstructor);
  record->Init("'SitedResult", SitedResultConstructor);
  record->Init("SitedResult", SitedResultConstructor);
  record->Init("'Proxy", ProxyConstructor);
  INIT_STRUCTURE(record, "UnsafeComponent", "Proxy",
		 UnsafeComponent_Proxy, 1);
  record->Init("'Protocol", ProtocolConstructor);
  INIT_STRUCTURE(record, "UnsafeComponent", "Protocol",
		 UnsafeComponent_Protocol, 1);
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
