//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Contributor:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "generic/SignalHandler.hh"
#endif

#include "store/Store.hh"
#include "generic/SignalHandler.hh"
#include "generic/Scheduler.hh"
#include "generic/RootSet.hh"
#include "generic/Transients.hh"

#if defined(__MINGW32__) || defined(_MSC_VER)
#include <windows.h>
typedef int sig_atomic_t;
#else
#include <sys/types.h>
#include <time.h>
#include <sys/time.h>
#endif

//--** to be done: GetTime() wraps around every 71 weeks
//--** to be done: we use BlockSignals more often than necessary

static const u_int TIME_SLICE = 10; // milliseconds

word alarmHandlers; // sorted by time (ascending)

struct SigHandler {
  int signal;
  volatile sig_atomic_t pending;
  word handlers;
};

#define SIGLAST -1

static SigHandler sigHandlers[] =  {
#if defined(__MINGW32__) || defined(_MSC_VER)
  { CTRL_C_EVENT, 0, Store::IntToWord(0) },
  { CTRL_BREAK_EVENT, 0, Store::IntToWord(0) },
  { CTRL_CLOSE_EVENT, 0, Store::IntToWord(0) },
  { CTRL_LOGOFF_EVENT, 0, Store::IntToWord(0) },
  { CTRL_SHUTDOWN_EVENT, 0, Store::IntToWord(0) },
#else
#ifdef SIGHUP
  { SIGHUP, 0, Store::IntToWord(0) },
#endif
#ifdef SIGINT
  { SIGINT, 0, Store::IntToWord(0) },
#endif
#ifdef SIGPIPE
  { SIGPIPE, 0, Store::IntToWord(0) },
#endif
#ifdef SIGTERM
  { SIGTERM, 0, Store::IntToWord(0) },
#endif
#ifdef SIGCHLD
  { SIGCHLD, 0, Store::IntToWord(0) },
#endif
#ifdef SIGWINCH
  { SIGWINCH, 0, Store::IntToWord(0) },
#endif
#ifdef SIGUSR1
  { SIGUSR1, 0, Store::IntToWord(0) },
#endif
#ifdef SIGUSR2
  { SIGUSR2, 0, Store::IntToWord(0) },
#endif
#endif
  { SIGLAST, 0, Store::IntToWord(0) }
};

class SignalEntry: public Block {
private:
  static const BlockLabel SIGNAL_ENTRY_LABEL = MIN_DATA_LABEL;
  enum { CLOSURE_POS, NEXT_POS, SIZE };
public:
  static SignalEntry *New(word closure, word next) {
    Block *entry = Store::AllocBlock(SIGNAL_ENTRY_LABEL, SIZE);
    entry->InitArg(CLOSURE_POS, closure);
    entry->InitArg(NEXT_POS, next);
    return static_cast<SignalEntry *>(entry);
  }
  static SignalEntry *FromWordDirect(word entry) {
    Block *b = Store::DirectWordToBlock(entry);
    Assert(b->GetLabel() == SIGNAL_ENTRY_LABEL && b->GetSize() == SIZE);
    return static_cast<SignalEntry *>(b);
  }

  word GetClosure() {
    return GetArg(CLOSURE_POS);
  }
  word GetNext() {
    return GetArg(NEXT_POS);
  }
};

class TimerEntry: public Block {
private:
  static const BlockLabel TIMER_ENTRY_LABEL = MIN_DATA_LABEL;
  enum { TIME_POS, FUTURE_POS, NEXT_POS, SIZE };
public:
  static TimerEntry *New(u_int time, Future *future) {
    Block *entry = Store::AllocBlock(TIMER_ENTRY_LABEL, SIZE);
    entry->InitArg(TIME_POS, time);
    entry->InitArg(FUTURE_POS, future->ToWord());
    entry->InitArg(NEXT_POS, 0);
    return static_cast<TimerEntry *>(entry);
  }
  static TimerEntry *FromWordDirect(word entry) {
    Block *b = Store::DirectWordToBlock(entry);
    Assert(b->GetLabel() == TIMER_ENTRY_LABEL && b->GetSize() == SIZE);
    return static_cast<TimerEntry *>(b);
  }

  u_int GetTime() {
    return Store::DirectWordToInt(GetArg(TIME_POS));
  }
  Future *GetFuture() {
    Transient *transient = Store::DirectWordToTransient(GetArg(FUTURE_POS));
    Assert(transient->GetLabel() == FUTURE_LABEL);
    return static_cast<Future *>(transient);
  }
  TimerEntry *GetNext() {
    word next = GetArg(NEXT_POS);
    if (next == Store::IntToWord(0))
      return INVALID_POINTER;
    else
      return FromWordDirect(next);
  }
  void SetNext(TimerEntry *entry) {
    ReplaceArg(NEXT_POS, entry->ToWord());
  }
};

#if defined(__MINGW32__) || defined(_MSC_VER)
class Timer {
private:
  static u_int time;
  static HANDLE thread;

  static void Update() {
    time++;
    StatusWord::SetStatus(Scheduler::PreemptStatus() |
			  SignalHandler::SignalStatus());
  }
  static DWORD __stdcall TimerFunction(void *) {
    // Make sure that this thread is not mixed with others
    if (SetThreadPriority(GetCurrentThread(),
			  THREAD_PRIORITY_HIGHEST) == FALSE)
      Error("SetThreadPriority failed");
    while (true) {
      Sleep(TIME_SLICE);
      Update();
    }
  }
public:
  static void Init() {
    time = 0;
    DWORD threadId;
    thread = CreateThread(NULL, 1024, TimerFunction, NULL, 0, &threadId);
    if (thread == NULL)
      Error("Unable to start timer thread");
  }
  static u_int GetTime() {
    return time;
  }
  static void Suspend() {
    SuspendThread(thread);
  }
  static void Resume() {
    ResumeThread(thread);
  }
};

u_int Timer::time;
HANDLE Timer::thread;
#else
class Timer {
private:
  static volatile sig_atomic_t time;

  static void Update(int) {
    time++;
    StatusWord::SetStatus(Scheduler::PreemptStatus() |
			  SignalHandler::SignalStatus());
  }
public:
  static void Init() {
    time = 0;
    signal(SIGALRM, Update);

    struct itimerval value;
    int sec  = TIME_SLICE / 1000;
    int usec = (TIME_SLICE % 1000) * 1000;
    value.it_interval.tv_sec  = sec;
    value.it_interval.tv_usec = usec;
    value.it_value.tv_sec     = sec;
    value.it_value.tv_usec    = usec;
    if (setitimer(ITIMER_REAL, &value, NULL) < 0)
      Error("setitimer failed");
  }
  static u_int GetTime() {
    return time;
  }
};

volatile sig_atomic_t Timer::time;
#endif

#if defined(__MINGW32__) || defined(_MSC_VER)
static BOOL CALLBACK MyConsoleCtrlHandler(DWORD signal) {
  for (u_int i = 0; sigHandlers[i].signal != SIGLAST; i++)
    if (static_cast<DWORD>(sigHandlers[i].signal) == signal &&
	sigHandlers[i].handlers != Store::IntToWord(0)) {
      sigHandlers[i].pending++;
      return TRUE;
    }
  return FALSE;
}
#else
static void MySignalHandler(int signal) {
  for (u_int i = 0; sigHandlers[i].signal != SIGLAST; i++)
    if (sigHandlers[i].signal == signal) {
      sigHandlers[i].pending++;
      return;
    }
}
#endif

static void BlockSignals() {
#if defined(__MINGW32__) || defined(_MSC_VER)
  Timer::Suspend();
#else
  sigset_t set;
  sigfillset(&set);
  sigprocmask(SIG_SETMASK, &set, NULL);
#endif
}

static void UnblockSignals() {
#if defined(__MINGW32__) || defined(_MSC_VER)
  Timer::Resume();
#else
  sigset_t set;
  sigemptyset(&set);
  sigprocmask(SIG_SETMASK, &set, NULL);
#endif
}

//
// SignalHandler Methods
//
void SignalHandler::Init() {
  alarmHandlers = Store::IntToWord(0);
  RootSet::Add(alarmHandlers);
  for (u_int i = 0; sigHandlers[i].signal != SIGLAST; i++)
    RootSet::Add(sigHandlers[i].handlers);
  Timer::Init();
#if defined(__MINGW32__) || defined(_MSC_VER)
  SetConsoleCtrlHandler(MyConsoleCtrlHandler, TRUE);
#endif
}

static int FindSignal(int signal) {
  for (u_int i = 0; sigHandlers[i].signal != SIGLAST; i++)
    if (sigHandlers[i].signal == signal)
      return i;
  Error("illegal signal");
}

void SignalHandler::RegisterSignal(int _signal, word closure) {
  BlockSignals();
  u_int i = FindSignal(_signal);
#if !defined(__MINGW32__) && !defined(_MSC_VER)
  if (sigHandlers[i].handlers == Store::IntToWord(0))
    signal(_signal, MySignalHandler);
#endif
  sigHandlers[i].handlers =
    SignalEntry::New(closure, sigHandlers[i].handlers)->ToWord();
  UnblockSignals();
}

Future *SignalHandler::RegisterAlarm(u_int milliseconds) {
  BlockSignals();
  u_int time = Timer::GetTime() + (milliseconds + TIME_SLICE - 1) / TIME_SLICE;
  Future *future = Future::New();
  TimerEntry *newEntry = TimerEntry::New(time, future);
  if (alarmHandlers == Store::IntToWord(0)) // list was empty
    alarmHandlers = newEntry->ToWord();
  else { // insert into list, sorted by time (ascending)
    TimerEntry *entry = TimerEntry::FromWordDirect(alarmHandlers);
    TimerEntry *prev = INVALID_POINTER;
    while (entry->GetTime() <= time) { // skip this entry?
      prev = entry;
      entry = entry->GetNext();
      if (entry == INVALID_POINTER) // reached end of list
	goto done;
    }
    newEntry->SetNext(entry);
  done:
    if (prev == INVALID_POINTER)
      alarmHandlers = newEntry->ToWord();
    else
      prev->SetNext(newEntry);
  }
  UnblockSignals();
  return future;
}

static void CheckAlarms() {
  if (alarmHandlers == Store::IntToWord(0)) // no alarms
    return;
  TimerEntry *entry = TimerEntry::FromWordDirect(alarmHandlers);
  while (entry->GetTime() <= Timer::GetTime()) { // triggered?
    Future *future = entry->GetFuture();
    future->ScheduleWaitingThreads();
    future->Become(REF_LABEL, Store::IntToWord(0)); // unit
    entry = entry->GetNext();
    if (entry == INVALID_POINTER) { // all alarms have been triggered
      alarmHandlers = Store::IntToWord(0);
      return;
    }
  }
  alarmHandlers = entry->ToWord();
}

void SignalHandler::HandlePendingSignals() {
  BlockSignals();
  CheckAlarms();
  for (u_int i = 0; sigHandlers[i].signal != SIGLAST; i++)
    while (sigHandlers[i].pending > 0) {
      sigHandlers[i].pending--;
      word handlers = sigHandlers[i].handlers;
      while (handlers != Store::IntToWord(0)) {
	SignalEntry *entry = SignalEntry::FromWordDirect(handlers);
	word closure = entry->GetClosure();
	Scheduler::NewThread(closure, 1,
			     Store::IntToWord(sigHandlers[i].signal));
	handlers = entry->GetNext();
      }
      sigHandlers[i].handlers = Store::IntToWord(0);
    }
  UnblockSignals();
}
