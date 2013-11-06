//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Contributor:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "generic/SignalHandler.hh"
#endif

#include "generic/SignalHandler.hh"
#include "store/Store.hh"
#include "generic/Scheduler.hh"
#include "generic/RootSet.hh"
#include "generic/Transients.hh"
#include "generic/Time.hh"
#include "generic/Double.hh"

#if HAVE_SIGNAL
#include <signal.h>
#include <sys/time.h>
#endif

#if !HAVE_SIGNAL || HAVE_CONSOLECTRL
#include <windows.h>
#endif


namespace {

#if HAVE_SIG_ATOMIC_T
  typedef sig_atomic_t atomic_int;
#else
  typedef int atomic_int;
#endif

  //--** to be done: GetTime() wraps around every 71 weeks
  //--** to be done: we use BlockSignals more often than necessary

  const u_int TIME_SLICE = 10; // milliseconds

  word alarmHandlers; // sorted by time (ascending)

  struct SigHandler {
    int signal;
    volatile atomic_int pending;
    word handlers;
  };

#define SIGLAST -1

  SigHandler sigHandlers[] =  {
#if HAVE_CONSOLECTRL
    { CTRL_C_EVENT, 0, Store::IntToWord(0) },
    { CTRL_BREAK_EVENT, 0, Store::IntToWord(0) },
    { CTRL_CLOSE_EVENT, 0, Store::IntToWord(0) },
    { CTRL_LOGOFF_EVENT, 0, Store::IntToWord(0) },
    { CTRL_SHUTDOWN_EVENT, 0, Store::IntToWord(0) },
#endif
#if HAVE_SIGNAL && defined(SIGHUP)
    { SIGHUP, 0, Store::IntToWord(0) },
#endif
#if HAVE_SIGNAL && defined(SIGINT)
    { SIGINT, 0, Store::IntToWord(0) },
#endif
#if HAVE_SIGNAL && defined(SIGPIPE)
    { SIGPIPE, 0, Store::IntToWord(0) },
#endif
#if HAVE_SIGNAL && defined(SIGTERM)
    { SIGTERM, 0, Store::IntToWord(0) },
#endif
#if HAVE_SIGNAL && defined(SIGCHLD)
    { SIGCHLD, 0, Store::IntToWord(0) },
#endif
#if HAVE_SIGNAL && defined(SIGWINCH)
    { SIGWINCH, 0, Store::IntToWord(0) },
#endif
#if HAVE_SIGNAL && defined(SIGUSR1)
    { SIGUSR1, 0, Store::IntToWord(0) },
#endif
#if HAVE_SIGNAL && defined(SIGUSR2)
    { SIGUSR2, 0, Store::IntToWord(0) },
#endif
#if HAVE_SIGNAL && defined(SIGTSTP)
    { SIGTSTP, 0, Store::IntToWord(0) },
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
    enum {
      TIME_POS, // in microseconds, as a Double
      FUTURE_POS,
      NEXT_POS,
      SIZE
    };
  public:
    static TimerEntry *New(double time, Future *future) {
      Block *entry = Store::AllocMutableBlock(TIMER_ENTRY_LABEL, SIZE);
      entry->InitArg(TIME_POS, Double::New(time)->ToWord());
      entry->InitArg(FUTURE_POS, future->ToWord());
      entry->InitArg(NEXT_POS, static_cast<s_int>(0));
      return static_cast<TimerEntry *>(entry);
    }
    static TimerEntry *FromWordDirect(word entry) {
      Block *b = Store::DirectWordToBlock(entry);
      Assert(b->GetLabel() == TIMER_ENTRY_LABEL && b->GetSize() == SIZE);
      return static_cast<TimerEntry *>(b);
    }
    double GetTime() {
      return Double::FromWord(GetArg(TIME_POS))->GetValue();
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

#if HAVE_SIGNAL
  class Timer {
  private:
    static volatile atomic_int time;

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

  volatile atomic_int Timer::time;
#else
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
#endif

}


#if HAVE_CONSOLECTRL
static BOOL CALLBACK MyConsoleCtrlHandler(DWORD signal) {
  for (u_int i = 0; sigHandlers[i].signal != SIGLAST; i++)
    if (static_cast<DWORD>(sigHandlers[i].signal) == signal &&
	sigHandlers[i].handlers != Store::IntToWord(0)) {
      sigHandlers[i].pending++;
      StatusWord::SetStatus(SignalHandler::SignalStatus());
      return TRUE;
    }
  return FALSE;
}
#endif

#if HAVE_SIGNAL
static void MySignalHandler(int signal) {
  for (u_int i = 0; sigHandlers[i].signal != SIGLAST; i++)
    if (sigHandlers[i].signal == signal) {
      sigHandlers[i].pending++;
      StatusWord::SetStatus(SignalHandler::SignalStatus());
      return;
    }
}
#endif

static void BlockSignals() {
#if HAVE_SIGNAL
  sigset_t set;
  sigfillset(&set);
  sigprocmask(SIG_SETMASK, &set, NULL);
#else
  Timer::Suspend();
#endif
}

static void UnblockSignals() {
#if HAVE_SIGNAL
  sigset_t set;
  sigemptyset(&set);
  sigprocmask(SIG_SETMASK, &set, NULL);
#else
  Timer::Resume();
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
#if HAVE_CONSOLECTRL
  SetConsoleCtrlHandler(MyConsoleCtrlHandler, TRUE);
#endif
}

static u_int FindSignal(int signal) {
  for (u_int i = 0; sigHandlers[i].signal != SIGLAST; i++)
    if (sigHandlers[i].signal == signal)
      return i;
  Error("illegal signal");
}

void SignalHandler::RegisterSignal(int _signal, word closure) {
  BlockSignals();
  u_int i = FindSignal(_signal);
#if HAVE_SIGNAL
  if (sigHandlers[i].handlers == Store::IntToWord(0))
    signal(_signal, MySignalHandler);
#endif
  sigHandlers[i].handlers =
    SignalEntry::New(closure, sigHandlers[i].handlers)->ToWord();
  UnblockSignals();
}

Future *SignalHandler::RegisterAlarm(u_int milliseconds) {
  BlockSignals();
  double time = Time::GetElapsedMicroseconds() + static_cast<double>(milliseconds) * 1000.0;
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
  while (entry->GetTime() <= Time::GetElapsedMicroseconds()) { // triggered?
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
