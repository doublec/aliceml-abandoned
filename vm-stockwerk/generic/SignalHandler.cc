//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
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

typedef struct {
  int signal;
  bool pending;
  word handlers;
} SigHandler;

#define SIGLAST -1

static SigHandler sigHandlers[] =  {
  { SIGALRM, false, Store::IntToWord(0) },
  { SIGLAST, false, Store::IntToWord(0) }
};

class Entry : public Tuple {
private:
  enum { CLOSURE_POS, NEXT_POS, SIZE };
public:
  word GetClosure() {
    return Tuple::Sel(CLOSURE_POS);
  }
  word GetNext() {
    return Tuple::Sel(NEXT_POS);
  }
  static Entry *New(word closure, word next) {
    Tuple *entry = Tuple::New(SIZE);
    entry->Init(CLOSURE_POS, closure);
    entry->Init(NEXT_POS, next);
    return static_cast<Entry *>(entry);
  }
  static Entry *FromWordDirect(word entry) {
    return static_cast<Entry *>(Tuple::FromWordDirect(entry));
  }
};

class TimerEntry : public Tuple {
private:
  enum { CLOSURE_POS, NEXT_POS, TIME_POS, SIZE };
public:
  word GetClosure() {
    return Tuple::Sel(CLOSURE_POS);
  }
  word GetNext() {
    return Tuple::Sel(NEXT_POS);
  }
  u_int GetTime() {
    return Store::DirectWordToInt(Tuple::Sel(TIME_POS));
  }
  void SetNext(word entry) {
    static_cast<Block *>(this)->ReplaceArg(NEXT_POS, entry);
  }
  static TimerEntry *New(word closure, u_int time, word next) {
    Tuple *entry = Tuple::New(SIZE);
    entry->Init(CLOSURE_POS, closure);
    entry->Init(NEXT_POS, next);
    entry->Init(TIME_POS, Store::IntToWord(time));
    return static_cast<TimerEntry *>(entry);
  }
  static TimerEntry *FromWordDirect(word entry) {
    return static_cast<TimerEntry *>(Tuple::FromWordDirect(entry));
  }
};

#if defined(__MINGW32__) || defined(_MSC_VER)
#include <windows.h>

class Timer {
protected:
  static Timer *self;
  static u_int time;
public:
  HANDLE thread;
  int wait;
  // Timer Constructor
  Timer(int w);
  static void Init() {
    time = 0;
    self = new Timer(10);
  }
  // Timer Methods
  static HANDLE GetThread() {
    return self->thread;
  }
  static u_int GetTime() {
    return time;
  }
  static void Update() {
    time++;
    StatusWord::SetStatus(Scheduler::PreemptStatus() |
			  SignalHandler::SignalStatus());
    sigHandlers[0].pending = true;
  }
};

Timer *Timer::self;
u_int Timer::time;

static DWORD __stdcall TimerFunction(void *p) {
  Timer *timer = static_cast<Timer *>(p);
  // Make sure that this thread is not mixed with others
  if (SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_HIGHEST) == FALSE) {
    Error("SetThreadPriority failed");
  }
  while(1) {
    Sleep(timer->wait);
    Timer::Update();
  }
  delete timer;
  ExitThread(1);
  return 1;
}

Timer::Timer(int w) {
  wait = w;
  DWORD threadId;
  thread = CreateThread(NULL, 10000, TimerFunction, this, 0, &threadId);
  if (thread == NULL) {
    // to be done: critical or warning
    Error("Unable to start timer thread");
  }
}
#else
#include <sys/types.h>
#include <time.h>
#include <sys/time.h>

class Timer {
protected:
  static u_int time;

  static void SetAlarmTimer(u_int time) {
    struct itimerval newT;
    int sec  = time/1000;
    int usec = (time*1000)%1000000;
    newT.it_interval.tv_sec  = sec;
    newT.it_interval.tv_usec = usec;
    newT.it_value.tv_sec     = sec;
    newT.it_value.tv_usec    = usec;
    if (setitimer(ITIMER_REAL, &newT, NULL) < 0) {
      // to be done: critical or warning
      Error("setitimer failed\n");
    }
  }
  static void Update(int) {
    time++;
    StatusWord::SetStatus(Scheduler::PreemptStatus() | 
			  SignalHandler::SignalStatus());
    sigHandlers[0].pending = true;
    SetAlarmTimer(10);
  }
public:
  // Timer Static Constructor
  static void Init() {
    time = 0;
    for (u_int i = 0; sigHandlers[i].signal != SIGLAST; i++)
      RootSet::Add(sigHandlers[i].handlers);
    signal(SIGALRM, Timer::Update);
    SetAlarmTimer(10);
  }
  static u_int GetTime() {
    return time;
  }
};

u_int Timer::time;
#endif

//
// SignalHandler Methods
//
void SignalHandler::PushCall(word closure, int signal) {
  Thread *thread = Scheduler::NewThread(closure, 1, Store::IntToWord(signal));
  Scheduler::ScheduleThread(thread);
}

static Future *WordToFuture(word future) {
  Transient *transient = Store::WordToTransient(future);
  Assert(transient != INVALID_POINTER &&
	 transient->GetLabel() == FUTURE_LABEL);
  return static_cast<Future *>(transient);
}

void SignalHandler::CheckTimerEvents() {
  word handlers = sigHandlers[0].handlers;
  TimerEntry *prev = NULL;
  sigHandlers[0].pending = false;
  while (handlers != Store::IntToWord(0)) {
    TimerEntry *entry = TimerEntry::FromWordDirect(handlers);
    handlers = entry->GetNext();
    // We might have missed some ticks
    if (entry->GetTime() <= Timer::GetTime()) {
      Future *future = WordToFuture(entry->GetClosure());
      future->ScheduleWaitingThreads();
      future->Become(REF_LABEL, Store::IntToWord(0));
      if (prev == NULL)
	sigHandlers[0].handlers = handlers;
      else
	prev->SetNext(handlers);
    }
    else
      prev = entry;
  }
}

void SignalHandler::Init() {
  for (u_int i = 0; sigHandlers[i].signal != SIGLAST; i++)
    RootSet::Add(sigHandlers[i].handlers);
  Timer::Init();
}

void SignalHandler::BlockSignals() {
#if defined(__MINGW32__) || defined(_MSC_VER)
  SuspendThread(Timer::GetThread());
#else
  sigset_t set;
  sigfillset(&set);
  // These signals should not be blocked
  sigdelset(&set, SIGINT);
  sigdelset(&set, SIGHUP);
  sigdelset(&set, SIGTERM);
  sigprocmask(SIG_SETMASK, &set, NULL);
#endif
}

void SignalHandler::UnblockSignals() {
#if defined(__MINGW32__) || defined(_MSC_VER)
  ResumeThread(Timer::GetThread());
#else
  sigset_t set;
  sigemptyset(&set);
  sigprocmask(SIG_SETMASK, &set, NULL);
#endif
}

static int FindSignal(int signal) {
  u_int i = 0;
  while (sigHandlers[i].signal != signal) {
    if (sigHandlers[i].signal == SIGLAST) {
      Error("illegal signal");
    }
    i++;
  }
  return i;
}

void
SignalHandler::Register(int signal, word closure, u_int delay = 0) {
  BlockSignals();
  u_int i = FindSignal(signal);
  word entry;
  if (signal == SIGALRM) {
    u_int tickTime = Timer::GetTime() + (delay / 10);
    entry =
      TimerEntry::New(closure, tickTime, sigHandlers[i].handlers)->ToWord();
  }
  else
    entry = Entry::New(closure, sigHandlers[i].handlers)->ToWord();
  sigHandlers[i].handlers = entry;
  UnblockSignals();
}

bool SignalHandler::PendingTimerEvents() {
  return (sigHandlers[0].handlers != Store::IntToWord(0));
}

void SignalHandler::HandlePendingSignals() {
  BlockSignals();
  CheckTimerEvents();
  for (u_int i = 1; sigHandlers[i].signal != SIGLAST; i++)
    if (sigHandlers[i].pending) {
      sigHandlers[i].pending = false;
      word handlers = sigHandlers[i].handlers;
      while (handlers != Store::IntToWord(0)) {
	Entry *entry = Entry::FromWordDirect(handlers);
	handlers     = entry->GetNext();
	word closure = entry->GetClosure();
	// to be done: Use thread instead?
	SignalHandler::PushCall(closure, sigHandlers[i].signal);
      }
      sigHandlers[i].handlers = Store::IntToWord(0);
    }
  UnblockSignals();
}
