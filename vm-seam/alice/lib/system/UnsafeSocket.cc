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
#define Interruptible(res, call) int res = call; res = res;
#else
#include <unistd.h>
#define ioctlsocket ioctl
#define closesocket close
#define WSAGetLastError() errno
#define Interruptible(res, call)		\
  int res;					\
  do {						\
    res = call;					\
  } while (res < 0 && WSAGetLastError() == EINTR);
#endif

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
  DECLARE_INT(port, x0);

  Interruptible(sock, socket(PF_INET, SOCK_STREAM, 0));
  if (sock < 0) { RAISE_SOCK_ERR(); } //--** IO.Io

  // bind a name to the socket:
  sockaddr_in addr;
  socklen_t addrLen = sizeof(addr);
  std::memset(&addr, 0, sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = htonl(INADDR_ANY);
  addr.sin_port = htons(port);
  Interruptible(res1, bind(sock, reinterpret_cast<sockaddr *>(&addr),
			   addrLen));
  if (res1 < 0) { RAISE_SOCK_ERR(); } //--** IO.Io

  // listen for connections:
  static const u_int backLog = 5;
  Interruptible(ret, listen(sock, backLog));
  Assert(ret == 0);
  SetNonBlocking(sock, true);

  Interruptible(res2, getsockname(sock, reinterpret_cast<sockaddr *>(&addr),
				  &addrLen));
  if (res2 < 0) { RAISE_SOCK_ERR(); } //--** IO.Io
  RETURN2(Store::IntToWord(sock), Store::IntToWord(ntohs(addr.sin_port)));
} END

DEFINE1(UnsafeSocket_accept) {
  DECLARE_INT(sock, x0);

  sockaddr_in addr;
  socklen_t addrLen = sizeof(addr);
 retry:
  Interruptible(client, accept(sock, reinterpret_cast<sockaddr *>(&addr),
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
      RAISE_SOCK_ERR(); //--** IO.Io
    }
  }
  SetNonBlocking(client, true);

  RETURN3(Store::IntToWord(client),
	  String::New(GetHostName(&addr))->ToWord(),
	  Store::IntToWord(ntohs(addr.sin_port)));
} END

DEFINE2(UnsafeSocket_client) {
  DECLARE_STRING(host, x0);
  DECLARE_INT(port, x1);

  Interruptible(sock, socket(PF_INET, SOCK_STREAM, 0));
  if (sock < 0) { RAISE_SOCK_ERR(); } //--** IO.Io
  SetNonBlocking(sock, true);

  hostent *entry = gethostbyname(host->ExportC());
  if (!entry) { RAISE_SOCK_ERR(); } //--** IO.Io
  sockaddr_in addr;
  std::memset(&addr, 0, sizeof(addr));
  addr.sin_family = AF_INET;
  std::memcpy(&addr.sin_addr, entry->h_addr_list[0], sizeof(addr.sin_addr));
  addr.sin_port = htons(port);

  Interruptible(ret, connect(sock, reinterpret_cast<sockaddr *>(&addr),
			     sizeof(addr)));
  if (ret < 0) {
    int error = WSAGetLastError();
    if (error == EWOULDBLOCK || error == EINPROGRESS) {
      //--** also check for exceptions on sock (connection failed)
      Future *future = IOHandler::WaitWritable(sock);
      if (future != INVALID_POINTER) {
	// Don't use macro REQUEST here, as we wish to drop our frame:
	Scheduler::currentData = future->ToWord();
	Scheduler::nArgs = Scheduler::ONE_ARG;
	Scheduler::currentArgs[0] = Store::IntToWord(sock);
	return Worker::REQUEST;
      }
    } else {
      RAISE_SOCK_ERR(); //--** IO.Io
    }
  }
  RETURN_INT(sock);
} END

DEFINE1(UnsafeSocket_input1) {
  DECLARE_INT(sock, x0);

  u_char c;
 retry:
  Interruptible(n, recv(sock, reinterpret_cast<char *>(&c), 1, 0));
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
      RAISE_SOCK_ERR(); //--** IO.Io
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
  DECLARE_INT(sock, x0);
  DECLARE_INT(count, x1);

  if (count < 0 || STATIC_CAST(u_int, count) > String::maxSize) {
    RAISE(PrimitiveTable::General_Size);
  }
  String *buffer = String::New(count);
 retry:
  Interruptible(n, recv(sock, reinterpret_cast<char *>(buffer->GetValue()),
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
      RAISE_SOCK_ERR(); //--** IO.Io
    }
  } else if (n == 0) {
    RETURN(String::New(STATIC_CAST(u_int, 0))->ToWord());
  } else if (n == count) {
    RETURN(buffer->ToWord());
  } else {
    String *string = String::New(n);
    std::memcpy(string->GetValue(), buffer->GetValue(), n);
    RETURN(string->ToWord());
  }
} END

DEFINE2(UnsafeSocket_output1) {
  DECLARE_INT(sock, x0);
  DECLARE_INT(i, x1);
  u_char c = i;
 retry:
  Interruptible(res, send(sock, reinterpret_cast<char *>(&c), 1, 0));
  if (res < 0) {
    if (WSAGetLastError() == EWOULDBLOCK) {
      Future *future = IOHandler::WaitWritable(sock);
      if (future != INVALID_POINTER) {
	REQUEST(future->ToWord());
      } else {
	goto retry;
      }
    } else {
      RAISE_SOCK_ERR(); //--** IO.Io
    }
  }
  RETURN_UNIT;
} END

DEFINE3(UnsafeSocket_output) {
  DECLARE_INT(sock, x0);
  DECLARE_STRING(string, x1);
  DECLARE_INT(offset, x2);

  Assert(offset >= 0 && STATIC_CAST(u_int, offset) < string->GetSize());
  u_char *buffer = string->GetValue() + offset;
  u_int count = string->GetSize() - offset;
 retry:
  Interruptible(n, send(sock, reinterpret_cast<char *>(buffer), count, 0));
  if (n < 0) {
    if (WSAGetLastError() == EWOULDBLOCK) {
      Future *future = IOHandler::WaitWritable(sock);
      if (future != INVALID_POINTER) {
	REQUEST(future->ToWord());
      } else {
	goto retry;
      }
    } else {
      RAISE_SOCK_ERR(); //--** IO.Io
    }
  } else {
    RETURN_INT(n);
  }
} END

DEFINE1(UnsafeSocket_close) {
  DECLARE_INT(sock, x0);
  Interruptible(res, closesocket(sock));
  IOHandler::Close(sock);
  RETURN_UNIT;
} END

AliceDll word UnsafeSocket() {
  //--** to be done: Windows Socket startup moved to UnsafeIO.cc
  SysErrConstructor =
    UniqueConstructor::New("SysErr", "OS.SysErr")->ToWord();
  RootSet::Add(SysErrConstructor);
  Record *record = Record::New(8);
  INIT_STRUCTURE(record, "UnsafeSocket", "server",
		 UnsafeSocket_server, 1);
  INIT_STRUCTURE(record, "UnsafeSocket", "accept",
		 UnsafeSocket_accept, 1);
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
