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
#pragma implementation "emulator/IOHandler.hh"
#endif

#include <sys/select.h>

#include "adt/Queue.hh"
#include "emulator/IOHandler.hh"
#include "emulator/RootSet.hh"

static const BlockLabel ENTRY_LABEL = (BlockLabel) MIN_DATA_LABEL;

class Entry: private Block {
private:
  static const u_int FD_POS     = 0;
  static const u_int FUTURE_POS = 1;
  static const u_int SIZE       = 2;
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
  //--** Purge before GC
private:
  static const u_int initialQueueSize = 8; //--** to be checked
public:
  using Block::ToWord;

  static Set *New() {
    return static_cast<Set *>(Queue::New(initialQueueSize));
  }
  static Set *FromWordDirect(word w) {
    return static_cast<Set *>(Queue::FromWordDirect(w));
  }

  void Add(Entry *entry) {
    Enqueue(entry->ToWord());
  }
  u_int EnterIntoFDSet(fd_set *fdSet) {
    u_int count = GetNumberOfElements();
    FD_ZERO(fdSet);
    for (u_int i = count; i--; ) {
      FD_SET(Entry::FromWordDirect(GetNthElement(i))->GetFD(), fdSet);
    }
    return count;
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

static word Readable, Writable;

void IOHandler::Init() {
  Readable = Set::New()->ToWord();
  Writable = Set::New()->ToWord();
  RootSet::Add(Readable);
  RootSet::Add(Writable);
}

void IOHandler::Poll() {
  Set *ReadableSet = Set::FromWordDirect(Readable);
  Set *WritableSet = Set::FromWordDirect(Writable);
  static fd_set readFDs, writeFDs;
  u_int nRead = ReadableSet->EnterIntoFDSet(&readFDs);
  u_int nWrite = WritableSet->EnterIntoFDSet(&writeFDs);
  if (nRead + nWrite > 0) {
    struct timeval timeout;
    timeout.tv_sec = 0;
    timeout.tv_usec = 0;
    int ret = select((nRead > nWrite? nRead: nWrite) + 1,
		     &readFDs, &writeFDs, NULL, &timeout);
    if (ret < 0) {
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
  u_int nRead = ReadableSet->EnterIntoFDSet(&readFDs);
  u_int nWrite = WritableSet->EnterIntoFDSet(&writeFDs);
  if (nRead + nWrite > 0) {
    struct timeval timeout;
    timeout.tv_sec = 0;
    timeout.tv_usec = 0;
    int ret = select((nRead > nWrite? nRead: nWrite) + 1,
		     &readFDs, &writeFDs, NULL, &timeout);
    if (ret < 0) {
      Error("IOHandler::Poll");
    }
    Assert(ret > 0);
    ReadableSet->Schedule(&readFDs);
    WritableSet->Schedule(&writeFDs);
  }
}

Future *IOHandler::SignalReadable(int fd) {
  fd_set readFDs;
  FD_ZERO(&readFDs);
  FD_SET(fd, &readFDs);

  struct timeval timeout;
  timeout.tv_sec = 0;
  timeout.tv_usec = 0;

  int ret = select(fd + 1, &readFDs, NULL, NULL, &timeout);
  if (ret < 0) {
    Error("IOHandler::SignalReadable");
  } else if (ret == 0) {
    Future *future = Future::New();
    Entry *entry = Entry::New(fd, future);
    Set::FromWordDirect(Readable)->Add(entry);
    return future;
  } else {
    return INVALID_POINTER;
  }
}

Future *IOHandler::SignalWritable(int fd) {
  fd_set writeFDs;
  FD_ZERO(&writeFDs);
  FD_SET(fd, &writeFDs);

  struct timeval timeout;
  timeout.tv_sec = 0;
  timeout.tv_usec = 0;

  int ret = select(fd + 1, NULL, &writeFDs, NULL, &timeout);
  if (ret < 1) {
    Error("IOHandler::SignalWritable");
  } else if (ret == 0) {
    Future *future = Future::New();
    Entry *entry = Entry::New(fd, future);
    Set::FromWordDirect(Writable)->Add(entry);
    return future;
  } else {
    return INVALID_POINTER;
  }
}
