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

#if defined(INTERFACE)
#pragma implementation "generic/IOHandler.hh"
#endif

#include <cstdlib>
#include <cstring>
#include <errno.h>

#if USE_WINSOCK
#include <winsock.h>
#define GetLastError() WSAGetLastError()
#elif USE_POSIX_SELECT
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/types.h>
#define GetLastError() errno
#else
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <unistd.h>
#define GetLastError() errno
#endif

#include <sys/time.h>

#include "adt/Queue.hh"
#include "generic/RootSet.hh"
#include "generic/Transients.hh"
#include "generic/IOHandler.hh"

namespace {
  const BlockLabel ENTRY_LABEL = (BlockLabel) MIN_DATA_LABEL;

  class Entry: private Block {
  private:
    enum { FD_POS, FUTURE_POS, SIZE };
  public:
    using Block::ToWord;

    static Entry *New(int fd, Future *future) {
      Block *block = Store::AllocMutableBlock(ENTRY_LABEL, SIZE);
      block->InitArg(FD_POS, fd);
      block->InitArg(FUTURE_POS, future->ToWord());
      return STATIC_CAST(Entry *, block);
    }
    static Entry *FromWordDirect(word w) {
      Block *block = Store::DirectWordToBlock(w);
      Assert(block->GetLabel() == ENTRY_LABEL);
      return STATIC_CAST(Entry *, block);
    }

    int GetFD() {
      return Store::DirectWordToInt(GetArg(FD_POS));
    }
    Future *GetFuture() {
      Transient *transient = Store::WordToTransient(GetArg(FUTURE_POS));
      Assert(transient != INVALID_POINTER &&
	     transient->GetLabel() == FUTURE_LABEL);
      return STATIC_CAST(Future *, transient);
    }
  };

  class Set: private Queue {
  private:
    static const u_int initialQueueSize = 8; //--** to be checked
  public:
    using Queue::ToWord;
    using Queue::Blank;

    static Set *New() {
      return STATIC_CAST(Set *, Queue::New(initialQueueSize));
    }
    static Set *FromWordDirect(word w) {
      return STATIC_CAST(Set *, Queue::FromWordDirect(w));
    }

    void Add(Entry *entry) {
      Enqueue(entry->ToWord());
    }
    void Remove(int fd) {
      u_int n = GetNumberOfElements();
      for (u_int i = 0; i < n; i++) {
	Entry *entry = Entry::FromWordDirect(GetNthElement(i));
	if (entry->GetFD() == fd) {
	  Future *future = entry->GetFuture();
	  future->ScheduleWaitingThreads();
	  future->Become(REF_LABEL, Store::IntToWord(0));
	  Queue::RemoveNthElement(i);
	  return;
	}
      }
    }
    int EnterIntoFDSet(fd_set *fdSet) {
      int max = -1;
      FD_ZERO(fdSet);
      for (u_int i = GetNumberOfElements(); i--; ) {
	int fd = Entry::FromWordDirect(GetNthElement(i))->GetFD();
	if (fd > max)
	  max = fd;
	FD_SET(fd, fdSet);
      }
      return max;
    }
    void Schedule(fd_set *fdSet) {
      u_int n = GetNumberOfElements();
      for (u_int i = 0; i < n; i++) {
      again:
	Entry *entry = Entry::FromWordDirect(GetNthElement(i));
	if (FD_ISSET(entry->GetFD(), fdSet)) {
	  Future *future = entry->GetFuture();
	  future->ScheduleWaitingThreads();
	  future->Become(REF_LABEL, Store::IntToWord(0));
	  Queue::RemoveNthElement(i);
	  if (i < --n) goto again;
	}
      }
    }
  };

  word Readable, Writable;
};

int IOHandler::SocketPair(int type, int *sv) {
#if !USE_WINSOCK
  return socketpair(PF_UNIX, type, 0, sv);
#else
  int newsock = socket(AF_INET, type, 0);
  if (newsock < 0) return -1;
  // bind the socket to any unused port
  struct sockaddr_in sock_in;
  sock_in.sin_family = AF_INET;
  sock_in.sin_port = 0;
  sock_in.sin_addr.s_addr = INADDR_ANY;
  if (bind(newsock, (struct sockaddr *) &sock_in, sizeof(sock_in)) < 0)
    return -1;
  int len = sizeof(sock_in);
  if (getsockname(newsock, (struct sockaddr *) &sock_in, &len) < 0) {
    closesocket(newsock);
    return -1;
  }
  listen(newsock, 2);
  // create a connecting socket
  int outsock = socket(AF_INET, type, 0);
  if (outsock < 0) {
    closesocket(newsock);
    return -1;
  }
  sock_in.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  // Do a connect and accept the connection
  if (connect(outsock, (struct sockaddr *) &sock_in, sizeof(sock_in)) < 0) {
    closesocket(newsock);
    closesocket(outsock);
    return -1;
  }
  int insock = accept(newsock, (struct sockaddr *) &sock_in, &len);
  if (insock < 0) {
    closesocket(newsock);
    closesocket(outsock);
    return -1;
  }
  closesocket(newsock);
  sv[0] = insock;
  sv[1] = outsock;
  return 0;
#endif
}

int IOHandler::defaultFD;

void IOHandler::Init() {
#if USE_WINSOCK
  WSADATA wsa_data;
  WORD req_version = MAKEWORD(1, 1);
  if (WSAStartup(req_version, &wsa_data) != 0)
    Error("no usable WinSock DLL found");
#endif

  int sv[2];
  if (SocketPair(SOCK_STREAM, sv) == -1)
    Error("socketpair failed");
  defaultFD = sv[0];
  Readable = Set::New()->ToWord();
  Writable = Set::New()->ToWord();
  RootSet::Add(Readable);
  RootSet::Add(Writable);
}

void IOHandler::Poll() {
  Set *ReadableSet = Set::FromWordDirect(Readable);
  Set *WritableSet = Set::FromWordDirect(Writable);
  static fd_set readFDs, writeFDs;
  int maxRead = ReadableSet->EnterIntoFDSet(&readFDs);
  int maxWrite = WritableSet->EnterIntoFDSet(&writeFDs);
  int max = maxRead > maxWrite? maxRead: maxWrite;
  if (max >= 0) {
    struct timeval timeout;
    timeout.tv_sec = 0;
    timeout.tv_usec = 0;
    int ret = select(max + 1,
		     maxRead == -1? NULL: &readFDs,
		     maxWrite == -1? NULL: &writeFDs,
		     NULL, &timeout);
    if (ret < 0) {
      if (GetLastError() == EINTR)
	return;
      Error("IOHandler::Poll");
    } else if (ret > 0) {
      ReadableSet->Schedule(&readFDs);
      WritableSet->Schedule(&writeFDs);
    }
  }
}

void IOHandler::Block() {
  Set *ReadableSet = Set::FromWordDirect(Readable);
  Set *WritableSet = Set::FromWordDirect(Writable);
  static fd_set readFDs, writeFDs;
  int maxRead = ReadableSet->EnterIntoFDSet(&readFDs);
  int maxWrite = WritableSet->EnterIntoFDSet(&writeFDs);
  struct timeval *ptimeout = NULL;
  // select does not allow all wait sets to be empty - therefore
  // always wait on stdin
  FD_SET(defaultFD, &readFDs);
  maxRead = maxRead > defaultFD? maxRead: defaultFD;
  int max = maxRead > maxWrite? maxRead: maxWrite;
#if USE_WINSOCK
  // signals (such as timer events) do not interrupt the select call,
  // therefore we must not block infinitely (so that signals are polled)
  //--** better solution - notify via a socket?
  struct timeval timeout;
  timeout.tv_sec  = 0;
  timeout.tv_usec = 100; // same as TIME_SLICE in SignalHandler.cc
  ptimeout = &timeout;
#endif
  if (max >= 0) {
    int ret = select(max + 1,
		     maxRead == -1? NULL: &readFDs,
		     maxWrite == -1? NULL: &writeFDs,
		     NULL, ptimeout);
    if (ret < 0) {
      if (GetLastError() == EINTR)
	return;
      Error("IOHandler::Block");
    } else if (ret > 0) {
      ReadableSet->Schedule(&readFDs);
      WritableSet->Schedule(&writeFDs);
    }
  }
}

void IOHandler::Purge() {
  Set::FromWordDirect(Readable)->Blank();
  Set::FromWordDirect(Writable)->Blank();
}

bool IOHandler::IsReadable(int fd) {
  fd_set readFDs;
  FD_ZERO(&readFDs);
  FD_SET(fd, &readFDs);
  struct timeval timeout;
  timeout.tv_sec = 0;
  timeout.tv_usec = 0;
 retry:
  int ret = select(fd + 1, &readFDs, NULL, NULL, &timeout);
  if (ret < 0) {
    if (GetLastError() == EINTR)
      goto retry;
    Error("IOHandler::IsReadable");
  }
  return ret != 0;
}

Future *IOHandler::WaitReadable(int fd) {
  fd_set readFDs;
  FD_ZERO(&readFDs);
  FD_SET(fd, &readFDs);
  struct timeval timeout;
  timeout.tv_sec = 0;
  timeout.tv_usec = 0;
 retry:
  int ret = select(fd + 1, &readFDs, NULL, NULL, &timeout);
  if (ret < 0) {
    if (GetLastError() == EINTR)
      goto retry;
    Error("IOHandler::WaitReadable");
  } else if (ret == 0) {
    Future *future = Future::New();
    Entry *entry = Entry::New(fd, future);
    Set::FromWordDirect(Readable)->Add(entry);
    return future;
  } else {
    return INVALID_POINTER;
  }
}

bool IOHandler::IsWritable(int fd) {
  fd_set writeFDs;
  FD_ZERO(&writeFDs);
  FD_SET(fd, &writeFDs);
  struct timeval timeout;
  timeout.tv_sec = 0;
  timeout.tv_usec = 0;
 retry:
  int ret = select(fd + 1, &writeFDs, NULL, NULL, &timeout);
  if (ret < 0) {
    if (GetLastError() == EINTR)
      goto retry;
    Error("IOHandler::IsWritable");
  }
  return ret != 0;
}

Future *IOHandler::WaitWritable(int fd) {
  fd_set writeFDs;
  FD_ZERO(&writeFDs);
  FD_SET(fd, &writeFDs);
  struct timeval timeout;
  timeout.tv_sec = 0;
  timeout.tv_usec = 0;
 retry:
  int ret = select(fd + 1, NULL, &writeFDs, NULL, &timeout);
  if (ret < 0) {
    if (GetLastError() == EINTR)
      goto retry;
    Error("IOHandler::WaitWritable");
  } else if (ret == 0) {
    Future *future = Future::New();
    Entry *entry = Entry::New(fd, future);
    Set::FromWordDirect(Writable)->Add(entry);
    return future;
  } else {
    return INVALID_POINTER;
  }
}

void IOHandler::Close(int fd) {
  Set::FromWordDirect(Readable)->Remove(fd);
  Set::FromWordDirect(Writable)->Remove(fd);
}
