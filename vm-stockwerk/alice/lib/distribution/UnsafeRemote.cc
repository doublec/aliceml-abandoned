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
#include <winsock.h>
#else
#include <netdb.h>
#include <sys/utsname.h>
#include <arpa/inet.h>
#endif

#include "emulator/Authoring.hh"
#include "emulator/RootSet.hh"
#include "emulator/Pickler.hh"
#include "emulator/Unpickler.hh"

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
    return taskStack->PushCall(callback);
  }
} END

DEFINE1(UnsafeRemote_packValue) {
  return Pickler::Pack(x0, taskStack);
} END

DEFINE1(UnsafeRemote_unpackValue) {
  DECLARE_STRING(packedValue, x0);
  return Unpickler::Unpack(static_cast<Chunk *>(packedValue), taskStack);
} END

word UnsafeRemote(void) {
  RootSet::Add(callback);
  callback = Store::IntToWord(0);

  Tuple *t = Tuple::New(5);
  t->Init(0, Primitive::MakeClosure("UnsafeRemote_dynamicCall",
				    UnsafeRemote_dynamicCall, 2, false));
  t->Init(1, Primitive::MakeClosure("UnsafeRemote_getLocalIP",
				    UnsafeRemote_getLocalIP, 0, true));
  t->Init(2, Primitive::MakeClosure("UnsafeRemote_packValue",
				    UnsafeRemote_packValue, 1, true));
  t->Init(3, Primitive::MakeClosure("UnsafeRemote_setCallback",
				    UnsafeRemote_setCallback, 1, true));
  t->Init(4, Primitive::MakeClosure("UnsafeRemote_unpackValue",
				    UnsafeRemote_unpackValue, 1, true));
  RETURN_STRUCTURE(t);
}
