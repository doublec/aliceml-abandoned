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
#if defined(__MINGW32__) || defined(_MSC_VER)
#include <winsock.h>

#define GetLastError() WSAGetLastError()
#else
#include <sys/select.h>

#define GetLastError() errno
#endif

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
      Block *block = Store::AllocBlock(ENTRY_LABEL, SIZE);
      block->InitArg(FD_POS, fd);
      block->InitArg(FUTURE_POS, future->ToWord());
      return static_cast<Entry *>(block);
    }
    static Entry *FromWordDirect(word w) {
      Block *block = Store::DirectWordToBlock(w);
      Assert(block->GetLabel() == ENTRY_LABEL);
      return static_cast<Entry *>(block);
    }

    int GetFD() {
      return Store::DirectWordToInt(GetArg(FD_POS));
    }
    Future *GetFuture() {
      Transient *transient = Store::WordToTransient(GetArg(FUTURE_POS));
      Assert(transient != INVALID_POINTER &&
	     transient->GetLabel() == FUTURE_LABEL);
      return static_cast<Future *>(transient);
    }
  };

  class Set: private Queue {
  private:
    static const u_int initialQueueSize = 8; //--** to be checked
  public:
    using Block::ToWord;
    using Queue::Blank;

    static Set *New() {
      return static_cast<Set *>(Queue::New(initialQueueSize));
    }
    static Set *FromWordDirect(word w) {
      return static_cast<Set *>(Queue::FromWordDirect(w));
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
      Assert(0);
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

int IOHandler::defaultFD;

void IOHandler::Init() {
  defaultFD = -1;
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
  //--** stdin can be closed!  We need a better solution.
  //--** Should we create a socket (using socketpair) on which nothing is
  //--** ever written just so that we can wait on it?
  if (defaultFD != -1)
    if (!FD_ISSET(defaultFD, &readFDs)) {
      FD_SET(defaultFD, &readFDs);
      maxRead = maxRead > defaultFD? maxRead: defaultFD;
    }
  int max = maxRead > maxWrite? maxRead: maxWrite;
#if defined(__MINGW32__) || defined(_MSC_VER)
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
