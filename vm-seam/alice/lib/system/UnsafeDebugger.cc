//
// Author:
//   Jens Regenberg <jens@ps.uni-sb.de>
//
// Copyright:
//   Jens Regenberg 2000-2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if DEBUGGER
#include <cstring>
#include "alice/Authoring.hh"
#include "alice/AbstractCodeInterpreter.hh"
#include "alice/AbstractCodeFrame.hh"
#include "alice/AliceConcreteCode.hh"
#include "alice/DebugEnvironment.hh"
#include "alice/PrimitiveTable.hh"
#include "alice/AliceDebuggerEvent.hh"
#include "alice/Types.hh"

word TranslateGenericDebuggerEvent(word event) {
  word thread = GenericEventAccessor::self->GetThread(event);
  word exn    = GenericEventAccessor::self->GetException(event);
  switch (GenericEventAccessor::self->GetType(event)) {
  case GenericDebuggerEvent::BLOCKED:
    {
      TagVal *curr = TagVal::New(0,1);
      curr->Init(0, thread);
      return curr->ToWord();
    }
    break;
  case GenericDebuggerEvent::RUNNABLE:
    {
      TagVal *curr = TagVal::New(4,1);
      curr->Init(0, thread);
      return curr->ToWord();
    }
    break;
  case GenericDebuggerEvent::TERMINATED:
    {
      TagVal *curr = TagVal::New(5,1);
      curr->Init(0, thread);
      return curr->ToWord();
    }
    break;
  case GenericDebuggerEvent::UNCAUGHT:
    {
      TagVal *curr = TagVal::New(6,2);
      curr->Init(0, thread);
      curr->Init(1, exn);
      return curr->ToWord();
    }
  default:
    Error("unknown generic debugger event");
  }
}



//
// UnsafeDebugger Functions
//


static word unknownThread;
static word invalidThreadState;
static word invalidBreakpoint;

#define RAISE_UNKNOWN_THREAD						\
  {									\
    ConVal *conVal =							\
      ConVal::New((Block *) Constructor::FromWordDirect(unknownThread), 0); \
    RAISE(conVal->ToWord());						\
  }

#define RAISE_INVALID_BREAKPOINT				       	\
  {									\
    ConVal *conVal =							\
      ConVal::New((Block *) Constructor::FromWordDirect(invalidBreakpoint), \
		  0);	\
    RAISE(conVal->ToWord());						\
  }

#define RAISE_INVALID_THREAD_STATE				       	\
  {									\
    ConVal *conVal =							\
      ConVal::New((Block *) Constructor::FromWordDirect(invalidThreadState), \
                  0);	\
    RAISE(conVal->ToWord());						\
  }

DEFINE0(UnsafeDebugger_getEventStream) {
  RETURN(Debugger::GetEventStream());
} END

DEFINE1(UnsafeDebugger_readEventStream) {
  DECLARE_BLOCKTYPE(EntryList, list, x0);
  word head = list->Hd();
  DebuggerEvent *event = DebuggerEvent::FromWord(head);
  if(event == INVALID_POINTER) {
    fprintf(stderr, "-->Ivalid pointer\n");
  }
  EventAccessor *accessor = event->GetAccessor();
  switch(accessor->GetLabel()) {
  case ALICE_EVENT_LABEL:
    {
      head = event->GetEvent();
    }
    break;
  case GENERIC_EVENT_LABEL:
    {
      head = TranslateGenericDebuggerEvent(event->GetEvent());
    }
    break;
  default:
    // has to be replaced by generation of an unknown-event
    Error("unknown event");
  }
  RETURN2(head,list->Tl());
} END

DEFINE1(UnsafeDebugger_singleStep) {
  DECLARE_THREAD(thread, x0);
//   if(thread->GetDebugMode() != Thread::DEBUG) {
//     RAISE_UNKNOWN_THREAD;
//   }

//   if(!(thread->IsSuspended())) {
//     RAISE_INVALID_THREAD_STATE;
//   }
  
  Debugger::SingleStep(thread);
  RETURN0;
} END

DEFINE1(UnsafeDebugger_detach) {
  DECLARE_THREAD(thread, x0);

  Debugger::Detach(thread);

  RETURN0;
} END

DEFINE0(UnsafeDebugger_breakpoint) {
  Debugger::Breakpoint(Scheduler::GetCurrentThread());
  RETURN0;
} END

DEFINE1(UnsafeDebugger_getRuntimeEnvironment) {
  DECLARE_THREAD(thread, x0);

  if(thread->GetDebugMode() != Thread::DEBUG) {
    RAISE_UNKNOWN_THREAD;
  }
  if(!(thread->IsSuspended())) {
    RAISE_INVALID_THREAD_STATE;
  }
  // Get topmost AbstractCodeFrame
  TaskStack *taskStack = thread->GetTaskStack();
  StackFrame *frame;
  word *base = (word *) taskStack->GetFrame(0);
  word *top  = base + taskStack->GetTop() - 1;
  while (top >= base) {
    frame = (StackFrame *) top;
    Worker *worker = frame->GetWorker();
    if (worker == STATIC_CAST(Worker *, AbstractCodeInterpreter::self)) { 
      break;
    }
    top -= worker->GetFrameSize(frame);
  }
  // Found no AbstractCodeFrame
  if (frame->GetWorker() != AbstractCodeInterpreter::self) {
    RETURN1(Store::IntToWord(Types::nil));
  }
  // compute the DebugEnvironment
  AbstractCodeFrame *aFrame = STATIC_CAST(AbstractCodeFrame *, frame);
  DebugEnvironment *dEnv = 
    DebugEnvironment::New(aFrame->GetLocalEnv(), aFrame->GetClosure());
  RETURN1(dEnv->GetNameValueList()->ToWord());
} END			  

DEFINE2(UnsafeDebugger_lookup) {
  DECLARE_THREAD(thread, x0);
  DECLARE_STRING(name,   x1);

  if(thread->GetDebugMode() != Thread::DEBUG) {
    RAISE_UNKNOWN_THREAD;
  }
  if(!(thread->IsSuspended())) {
    RAISE_INVALID_THREAD_STATE;
  }
  // Get topmost AbstractCodeFrame
  TaskStack *taskStack = thread->GetTaskStack();
  StackFrame *frame;
  word *base = (word *) taskStack->GetFrame(0);
  word *top  = base + taskStack->GetTop() - 1;
  while (top >= base) {
    frame = (StackFrame *) top;
    Worker *worker = frame->GetWorker();
    if (worker == STATIC_CAST(Worker *, AbstractCodeInterpreter::self)) { 
      break;
    }
    top -= worker->GetFrameSize(frame);
  }
  // compute the DebugEnvironment
  AbstractCodeFrame *aFrame = STATIC_CAST(AbstractCodeFrame *, frame);
  DebugEnvironment *dEnv = 
    DebugEnvironment::New(aFrame->GetLocalEnv(), aFrame->GetClosure());
  word val = dEnv->Lookup(name);
  RETURN1(val);

} END			  

DEFINE1(UnsafeDebugger_valueToString) {
  RETURN1(String::New("not implemented")->ToWord());
} END


// Exceptions & Structure

DEFINE0(UnsafeDebugger_UnknownThread) {
  Constructor *ccVal = Constructor::FromWord(unknownThread);
  ConVal *conVal     = ConVal::New((Block *)ccVal, 0);
  RETURN(conVal->ToWord());
} END

DEFINE0(UnsafeDebugger_InvalidThreadState) {
  Constructor *ccVal = Constructor::FromWord(invalidThreadState);
  ConVal *conVal     = ConVal::New((Block *)ccVal, 0);
  RETURN(conVal->ToWord());
} END

DEFINE0(UnsafeDebugger_InvalidBreakpoint) {
  Constructor *ccVal = Constructor::FromWord(invalidBreakpoint);
  ConVal *conVal     = ConVal::New((Block *)ccVal, 0);
  RETURN(conVal->ToWord());
} END

word UnsafeDebugger() {
  String *unknownThreadString = String::New("Debugger.UnknownThread");
  String *invalidBreakpointString = String::New("Debugger.InvalidBreakpoint");
  String *invalidThreadStateString = String::New("Debugger.InvalidThreadState");
  unknownThread =
    UniqueConstructor::New(unknownThreadString, unknownThreadString)->ToWord();
  RootSet::Add(unknownThread);

  invalidBreakpoint =
    UniqueConstructor::New(invalidBreakpointString,
			   invalidBreakpointString)->ToWord();
  RootSet::Add(invalidBreakpoint);

  invalidThreadState =
    UniqueConstructor::New(invalidThreadStateString,
			   invalidThreadStateString)->ToWord();
  RootSet::Add(invalidThreadState);

  Record *record = Record::New(14);
  record->Init("'UnknownThread", unknownThread);
  INIT_STRUCTURE(record, "UnsafeDebugger", "UnknownThread",
		 UnsafeDebugger_UnknownThread , 0);
  record->Init("'InvalidBreakpoint", invalidBreakpoint);
  INIT_STRUCTURE(record, "UnsafeDebugger", "InvalidBreakpoint",
		 UnsafeDebugger_InvalidBreakpoint , 0);
  record->Init("'InvalidThreadState", invalidThreadState);
  INIT_STRUCTURE(record, "UnsafeDebugger", "InvalidThreadState",
		 UnsafeDebugger_InvalidThreadState , 0);
  INIT_STRUCTURE(record, "UnsafeDebugger", "breakpoint",
		 UnsafeDebugger_breakpoint, 0);
  INIT_STRUCTURE(record, "UnsafeDebugger", "detach",
		 UnsafeDebugger_detach, 1);
  INIT_STRUCTURE(record, "UnsafeDebugger", "getEventStream",
		 UnsafeDebugger_getEventStream, 0);
  INIT_STRUCTURE(record, "UnsafeDebugger", "readEventStream",
		 UnsafeDebugger_readEventStream, 1);
  INIT_STRUCTURE(record, "UnsafeDebugger", "valueToString",
		 UnsafeDebugger_valueToString, 1);
  INIT_STRUCTURE(record, "UnsafeDebugger", "getRuntimeEnvironment",
		 UnsafeDebugger_getRuntimeEnvironment, 1);
  INIT_STRUCTURE(record, "UnsafeDebugger", "lookup",
		 UnsafeDebugger_lookup, 2);
  INIT_STRUCTURE(record, "UnsafeDebugger", "singleStep",
		 UnsafeDebugger_singleStep, 1);
  RETURN_STRUCTURE("UnsafeDebugger$", record);
}
#endif
