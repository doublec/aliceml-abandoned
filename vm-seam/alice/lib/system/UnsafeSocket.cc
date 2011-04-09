//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//


#if USE_WINSOCK
#include <cstring>
#include <winsock.h>
#else
#include <errno.h>

#include <netdb.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <cstring>
#endif

#include "Base.hh"

#include <sys/types.h>

#include "alice/Authoring.hh"

#if USE_WINSOCK
typedef int socklen_t;
#define EWOULDBLOCK WSAEWOULDBLOCK
#define EINPROGRESS WSAEINPROGRESS
#define Interruptible(t, res, call) t res = call; res = res;
#else
#include <unistd.h>
#define ioctlsocket ioctl
#define closesocket close
#define WSAGetLastError() errno
#define Interruptible(t, res, call)		\
  t res;					\
  do {						\
    res = call;					\
  } while (res < 0 && WSAGetLastError() == EINTR);
#endif

#define DECLARE_FD(i, x) \
  int i; \
  { \
    DECLARE_INT(DECLARE_FD_i, x); \
    i = static_cast<int>(DECLARE_FD_i); \
  }

#define DECLARE_PORT(i, x) \
  uint16_t i; \
  { \
    DECLARE_INT(DECLARE_PORT_p, x); \
    if (DECLARE_PORT_p < 0 || DECLARE_PORT_p >= 1<<16) { \
      RAISE(PrimitiveTable::General_Domain); \
    } \
    i = static_cast<uint16_t>(DECLARE_PORT_p); \
  }

static word SysErrConstructor;
#include "SysErr.icc"

//--** encapsulate sockets into IODesc
//     (for sitedness, finalization, and never closing more than once)

static int SetNonBlocking(int sock, bool flag) {
  unsigned long arg = flag;
  return ioctlsocket(sock, FIONBIO, &arg);
}

static const char *GetHostName(sockaddr_in *addr) {
  const char *host = inet_ntoa(addr->sin_addr);
  if (!std::strcmp(host, "127.0.0.1")) {
    // workaround for misconfigured offline hosts:
    host = "localhost";
  } else {
    hostent *entry =
      gethostbyaddr(reinterpret_cast<char *>(&addr->sin_addr),
		    sizeof(addr), AF_INET);
    if (entry)
      host = entry->h_name;
  }
  return host;
}

DEFINE1(UnsafeSocket_server) {
  DECLARE_PORT(port, x0);

  Interruptible(int, sock, socket(PF_INET, SOCK_STREAM, 0));
  if (sock < 0) { RAISE_SOCK_ERR(); }

  // bind a name to the socket:
  sockaddr_in addr;
  socklen_t addrLen = sizeof(addr);
  std::memset(&addr, 0, sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = htonl(INADDR_ANY);
  addr.sin_port = htons(port);
  Interruptible(int, res1, bind(sock, reinterpret_cast<sockaddr *>(&addr),
			   addrLen));
  if (res1 < 0) { RAISE_SOCK_ERR(); }

  // listen for connections:
  static const u_int backLog = 5;
  Interruptible(int, ret, listen(sock, backLog));
  Assert(ret == 0);
  SetNonBlocking(sock, true);

  Interruptible(int, res2, getsockname(sock, reinterpret_cast<sockaddr *>(&addr),
				  &addrLen));
  if (res2 < 0) { RAISE_SOCK_ERR(); }
  RETURN2(Store::IntToWord(sock), Store::IntToWord(ntohs(addr.sin_port)));
} END

DEFINE1(UnsafeSocket_accept) {
  DECLARE_FD(sock, x0);

  sockaddr_in addr;
  socklen_t addrLen = sizeof(addr);
 retry:
  Interruptible(int, client, accept(sock, reinterpret_cast<sockaddr *>(&addr),
			       &addrLen));
  if (client < 0) {
    if (WSAGetLastError() == EWOULDBLOCK) {
      Future *future = IOHandler::WaitReadable(sock);
      if (future != INVALID_POINTER) {
	REQUEST(future->ToWord());
      } else {
	goto retry;
      }
    } else {
      RAISE_SOCK_ERR();
    }
  }
  SetNonBlocking(client, true);

  RETURN3(Store::IntToWord(client),
	  String::New(GetHostName(&addr))->ToWord(),
	  Store::IntToWord(ntohs(addr.sin_port)));
} END

DEFINE2(UnsafeSocket_client) {
  DECLARE_STRING(host, x0);
  DECLARE_PORT(port, x1);

  Interruptible(int, sock, socket(PF_INET, SOCK_STREAM, 0));
  if (sock < 0) { RAISE_SOCK_ERR(); }
  SetNonBlocking(sock, true);

  hostent *entry = gethostbyname(host->ExportC());
  if (!entry) { RAISE_SOCK_ERR(); }
  sockaddr_in addr;
  std::memset(&addr, 0, sizeof(addr));
  addr.sin_family = AF_INET;
  std::memcpy(&addr.sin_addr, entry->h_addr_list[0], sizeof(addr.sin_addr));
  addr.sin_port = htons(port);

  Interruptible(int, ret, connect(sock, reinterpret_cast<sockaddr *>(&addr),
			     sizeof(addr)));
  if (ret < 0) {
    int error = WSAGetLastError();
    if (error == EWOULDBLOCK || error == EINPROGRESS) {
      //--** also check for exceptions on sock (connection failed)
      Future *future = IOHandler::WaitWritable(sock);
      if (future != INVALID_POINTER) {
	// Don't use macro REQUEST here, as we wish to drop our frame:
	Scheduler::SetCurrentData(future->ToWord());
	Scheduler::SetNArgs(1);
	Scheduler::SetCurrentArg(0, Store::IntToWord(sock));
	return Worker::REQUEST;
      }
    } else {
      RAISE_SOCK_ERR();
    }
  }
  RETURN_INT(sock);
} END

DEFINE1(UnsafeSocket_input1) {
  DECLARE_FD(sock, x0);

  u_char c;
 retry:
  Interruptible(s_int, n, recv(sock, reinterpret_cast<char *>(&c), 1, 0));
  if (n < 0) {
    int error = WSAGetLastError();
    if (error == EWOULDBLOCK) {
      Future *future = IOHandler::WaitReadable(sock);
      if (future != INVALID_POINTER) {
	REQUEST(future->ToWord());
      } else {
	goto retry;
      }
    } else {
      //--** map ECONNRESET to IO.Io {cause = ClosedStream, ...}
      //--** std::fprintf(stderr, "recv failed: %d\n", error);
      RAISE_SOCK_ERR();
    }
  } else if (n == 0) { // EOF
    RETURN_INT(Types::NONE);
  } else {
    Assert(n == 1);
    TagVal *tagVal = TagVal::New(Types::SOME, 1);
    tagVal->Init(0, Store::IntToWord(c));
    RETURN(tagVal->ToWord());
  }
} END

DEFINE2(UnsafeSocket_inputN) {
  DECLARE_FD(sock, x0);
  DECLARE_INT(count, x1);

  if (count < 0 || static_cast<u_int>(count) > ALICE_STRING_MAX_SIZE) {
    RAISE(PrimitiveTable::General_Size);
  }
  String *buffer = String::New(count);
 retry:
  Interruptible(s_int, n, recv(sock, reinterpret_cast<char *>(buffer->GetValue()),
			count, 0));
  if (n < 0) {
    if (WSAGetLastError() == EWOULDBLOCK) {
      Future *future = IOHandler::WaitReadable(sock);
      if (future != INVALID_POINTER) {
	REQUEST(future->ToWord());
      } else {
	goto retry;
      }
    } else {
      RAISE_SOCK_ERR();
    }
  } else if (n == 0) {
    RETURN(String::New(static_cast<u_int>(0))->ToWord());
  } else if (n == count) {
    RETURN(buffer->ToWord());
  } else {
    String *string = String::New(n);
    std::memcpy(string->GetValue(), buffer->GetValue(), n);
    RETURN(string->ToWord());
  }
} END

DEFINE2(UnsafeSocket_output1) {
  DECLARE_FD(sock, x0);
  DECLARE_INT(i, x1);
  u_char c = static_cast<u_char>(i);
 retry:
  Interruptible(s_int, res, send(sock, reinterpret_cast<char *>(&c), 1, 0));
  if (res < 0) {
    if (WSAGetLastError() == EWOULDBLOCK) {
      Future *future = IOHandler::WaitWritable(sock);
      if (future != INVALID_POINTER) {
	REQUEST(future->ToWord());
      } else {
	goto retry;
      }
    } else {
      RAISE_SOCK_ERR();
    }
  }
  RETURN_UNIT;
} END

DEFINE3(UnsafeSocket_output) {
  DECLARE_FD(sock, x0);
  DECLARE_STRING(string, x1);
  DECLARE_INT(offset, x2);

  Assert(offset >= 0 && static_cast<u_int>(offset) < string->GetSize());
  u_char *buffer = string->GetValue() + offset;
  u_int count = string->GetSize() - offset;
 retry:
  Interruptible(s_int, n, send(sock, reinterpret_cast<char *>(buffer), count, 0));
  if (n < 0) {
    if (WSAGetLastError() == EWOULDBLOCK) {
      Future *future = IOHandler::WaitWritable(sock);
      if (future != INVALID_POINTER) {
	REQUEST(future->ToWord());
      } else {
	goto retry;
      }
    } else {
      RAISE_SOCK_ERR();
    }
  } else {
    RETURN_INT(n);
  }
} END

DEFINE1(UnsafeSocket_close) {
  DECLARE_FD(sock, x0);
  Interruptible(int, res, closesocket(sock));
  IOHandler::Close(sock);
  RETURN_UNIT;
} END

AliceDll word UnsafeSocket() {
  //--** to be done: Windows Socket startup moved to UnsafeIO.cc
  SysErrConstructor =
    UniqueConstructor::New("SysErr", "OS.SysErr")->ToWord();
  RootSet::Add(SysErrConstructor);
  Record *record = Record::New(8);
  INIT_STRUCTURE_N(record, "UnsafeSocket", "server",
		   UnsafeSocket_server, 1, 2);
  INIT_STRUCTURE_N(record, "UnsafeSocket", "accept",
		 UnsafeSocket_accept, 1, 3);
  INIT_STRUCTURE(record, "UnsafeSocket", "client",
		 UnsafeSocket_client, 2);
  INIT_STRUCTURE(record, "UnsafeSocket", "input1",
		 UnsafeSocket_input1, 1);
  INIT_STRUCTURE(record, "UnsafeSocket", "inputN",
		 UnsafeSocket_inputN, 2);
  INIT_STRUCTURE(record, "UnsafeSocket", "output1",
		 UnsafeSocket_output1, 2);
  INIT_STRUCTURE(record, "UnsafeSocket", "output",
		 UnsafeSocket_output, 3);
  INIT_STRUCTURE(record, "UnsafeSocket", "close",
		 UnsafeSocket_close, 1);
  RETURN_STRUCTURE("UnsafeSocket$", record);
}
