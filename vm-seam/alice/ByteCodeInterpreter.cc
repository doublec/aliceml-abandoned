//
// Author:
//   Christian Mueller <cmueller@ps.uni-sb.de>
//
// Copyright:
//   Christian Mueller, 2005
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "alice/ByteCodeInterpreter.hh"
#pragma implementation "alice/ByteCodeFrame.hh"
#endif

#include <sstream>
#include "alice/ByteCodeInterpreter.hh"
#include "alice/Data.hh"
#include "alice/Types.hh"
#include "alice/LazySelInterpreter.hh"
#include "alice/AbstractCode.hh"
#include "alice/AbstractCodeInterpreter.hh"
#include "alice/AliceLanguageLayer.hh"
#include "alice/ByteCode.hh"
#include "alice/ByteCodeAlign.hh"
#include "alice/ByteConcreteCode.hh"
#include "alice/ByteCodeFrame.hh"
#include "alice/ByteCodeBuffer.hh"
#include "alice/ByteCodeJitter.hh"
#include "alice/ByteCodeSpecializer.hh"
#include "alice/ByteCodeSourceLocations.hh"

using namespace ByteCodeInstr;

#ifdef THREADED // is defined in ByteCodeBuffer.hh

#define SETPC(x) PC = codeBase + x
#define SAVEPC(PC) {						\
    u_int offset = reinterpret_cast<u_int>(PC) - reinterpret_cast<u_int>(codeBase);	\
    frame->SavePC(offset / sizeof(u_int));			\
}
#define LOADSTATE(PC, CP, IP) {			\
    code = concreteCode->GetByteCode();		\
    codeBase = reinterpret_cast<u_int *>(code->GetBase());	\
    SETPC(frame->GetPC());			\
    CP = frame->GetCP();			\
    IP = concreteCode->GetImmediateArgs();	\
}
#define Case(INSTR) INSTR##LBL: DEBUG_INSTR(); startPC = PC; SKIP_INSTR(PC);

#define DISPATCH(PC)				\
  goto *(reinterpret_cast<void *>(*PC))
  
#define INSTR(instr, args) && instr##LBL,

#else // THREADED

#define SETPC(x) PC = x 
#define SAVEPC(PC) frame->SavePC(PC)
#define LOADSTATE(PC, CP, IP) {			\
    code = concreteCode->GetByteCode();		\
    codeBuffer = ReadBuffer::New(code);		\
    SETPC(frame->GetPC());			\
    CP = frame->GetCP();			\
    IP = concreteCode->GetImmediateArgs();	\
}

#define Case(INSTR) case INSTR:

#define DISPATCH(PC) break

#define INSTR(instr, args) instr,

#endif // THREADED

#undef REQUEST							
#undef RAISE

#define RAISE(w) {                                  \
  Scheduler::SetCurrentData(w);                     \
  Scheduler::SetCurrentBacktrace(Backtrace::New()); \
  SAVEPC(PC);                                       \
  return Worker::RAISE;                             \
}

#define REQUEST(x)							\
  { SAVEPC(startPC); /* repeat current instr after request */		\
    Scheduler::SetCurrentData(x);					\
    Scheduler::SetNArgs(0);						\
    BCI_DEBUG("request for %p needed, saved PC to %d\n",x,startPC);	\
    return Worker::REQUEST; }

#define REQUEST_WORD(Type,val,w)		\
  Type *val = Type::FromWord(w);		\
  if (val == INVALID_POINTER) REQUEST(w)

#define REQUEST_INT(val,w)			\
  s_int val = Store::WordToInt(w);		\
  if (val == INVALID_INT) REQUEST(w)

#define CHECK_PREEMPT() 			\
  if (StatusWord::GetStatus() != 0) {		\
    SAVEPC(PC);					\
    BCI_DEBUG("suspension required\n");		\
    return Worker::PREEMPT;			\
  }


static void RecordCall(Closure *closure) {
#if PROFILE
  Profiler::IncCalls(closure->GetConcreteCode());
#endif
  ByteCodeSpecializer::IncCalls(closure);
}

ByteCodeInterpreter *ByteCodeInterpreter::self;

void ByteCodeInterpreter::Init() {
  self = new ByteCodeInterpreter;

#ifdef THREADED
  BCI_DEBUG("initialize threaded code table ... ");
  ByteCodeInterpreter::self->Run(NULL); // initialize threaded code table
  BCI_DEBUG("[FINISHED]\n");
#endif
}

Transform *
ByteCodeInterpreter::GetAbstractRepresentation(ConcreteRepresentation *cr) {
  return reinterpret_cast<ByteConcreteCode *>(cr)->GetAbstractRepresentation();
}

u_int ByteCodeInterpreter::GetFrameSize(StackFrame *sFrame) {
  ByteCodeFrame *frame = reinterpret_cast<ByteCodeFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

u_int ByteCodeInterpreter::GetInArity(ConcreteCode *concreteCode) {
  ByteConcreteCode *bcc = reinterpret_cast<ByteConcreteCode *>(concreteCode);
  return bcc->GetInArity();
}

u_int ByteCodeInterpreter::GetOutArity(ConcreteCode *concreteCode) {
  ByteConcreteCode *bcc = reinterpret_cast<ByteConcreteCode *>(concreteCode);
  s_int outArity = bcc->GetOutArity();
  return (outArity == -1) ? INVALID_INT : outArity;
}

const char *ByteCodeInterpreter::Identify() {
  return "ByteCodeInterpreter";
}

Worker::Result ByteCodeInterpreter::Handle(word data, Tuple *package) {
  BCI_DEBUG("ByteCodeInterpreter::Handle started\n");
  StackFrame *sFrame = Scheduler::GetFrame();
  ByteCodeFrame *frame = reinterpret_cast<ByteCodeFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  
  u_int ipIndex = Store::DirectWordToInt(data);
  word handler = frame->GetIP()->Sel(ipIndex);
  Tuple *handlerData = Tuple::FromWordDirect(handler);
  u_int r0 = Store::DirectWordToInt(handlerData->Sel(0));
  u_int r1 = Store::DirectWordToInt(handlerData->Sel(1));
  u_int handlerPC = Store::DirectWordToInt(handlerData->Sel(2));
  SETREG(r0, package->ToWord());
  SETREG(r1, package->Sel(0));
  frame->SavePC(handlerPC);  
  return Worker::CONTINUE;
}

/*********************************
 * interpreter main loop
 *********************************/

Worker::Result ByteCodeInterpreter::Run(StackFrame *sFrame) {

#ifdef THREADED
  static void* instrTable[] = {
    #include "alice/ByteCodeInstrs.hh"
    NULL
  };
  if(sFrame == NULL) {
    ByteCode::RegisterInstrTable(instrTable);
    return Worker::CONTINUE; // return value doesn't matter
  }
#endif

  ByteCodeFrame *frame = reinterpret_cast<ByteCodeFrame *>(sFrame);
  Assert(sFrame->GetWorker() == this);
  ByteConcreteCode *concreteCode = frame->GetConcreteCode();
  Chunk *code = concreteCode->GetByteCode();

  register ProgramCounter startPC; // save position before current instr
  register ProgramCounter PC;
#if defined(THREADED)
  u_int *codeBase = reinterpret_cast<u_int *>(code->GetBase());
#else
  register ReadBuffer *codeBuffer = ReadBuffer::New(code);
#endif
  SETPC(frame->GetPC());
  register Tuple *IP = concreteCode->GetImmediateArgs();
  register Closure *CP = frame->GetCP();

  BCI_DEBUG("BCI::Run: start execution at PC %d\n",frame->GetPC());
#ifdef THREADED
  DISPATCH(PC);

#else
  u_int instr;

  while(true) {
    DEBUG_INSTR();
    startPC = PC; // remember start position for requests
    GET_INSTR(codeBuffer,PC,instr);
    switch(instr) {
#endif
   
      /*********************
       * calling convention
       *********************/

      // with conversion

    Case(ccc1)
      {
	ByteCodeInterpreter::Construct(); 
	SETREG(0,Scheduler::GetCurrentArg(0));    
      }
      DISPATCH(PC);

    Case(seam_ccc1) // reg
      {
	GET_1R(codeBuffer,PC,reg);
	ByteCodeInterpreter::Construct();       
	SETREG(reg,Scheduler::GetCurrentArg(0));
      }
      DISPATCH(PC);

    Case(cccn) // nArgs
      {
	GET_1I(codeBuffer,PC,nArgs);
	if(ByteCodeInterpreter::Deconstruct())
	  return Worker::REQUEST;	
	Assert(Scheduler::GetNArgs() == nArgs);
	// invariant: 0 .. n-1 are the target registers
	for(u_int i = nArgs; i--;) {
	  SETREG(i,Scheduler::GetCurrentArg(i));
	}
      }
      DISPATCH(PC);

    Case(seam_cccn) // args
      {
	GET_1I(codeBuffer,PC,argsAddr);
	if(ByteCodeInterpreter::Deconstruct())
	  return Worker::REQUEST;

	Vector *args = Vector::FromWordDirect(IP->Sel(argsAddr));
	for(u_int i = args->GetLength(); i--; ) {
	  TagVal *argOpt = TagVal::FromWord(args->Sub(i));
	  if(argOpt != INVALID_POINTER) {
	    u_int dst = Store::DirectWordToInt(argOpt->Sel(0));
	    SETREG(dst, Scheduler::GetCurrentArg(i));
	  }
	}
      }
      DISPATCH(PC);

      // without conversion

      Case(get_arg0) // dst
	{
	  GET_1R(codeBuffer,PC,dst);
	  SETREG(dst, Scheduler::GetCurrentArg(0));
	}
        DISPATCH(PC);

      Case(get_arg0_direct) 
	{
	  SETREG(0, Scheduler::GetCurrentArg(0));
	}
        DISPATCH(PC);

      Case(get_args_direct) // nArgs
	{
	  GET_1I(codeBuffer,PC,nArgs);
	  for(u_int i = nArgs; i--;) {
	    SETREG(i, Scheduler::GetCurrentArg(i));
	  }
	}
        DISPATCH(PC);

      Case(get_args) // args
	{
	  GET_1I(codeBuffer,PC,argsAddr);
	  Vector *args = Vector::FromWordDirect(IP->Sel(argsAddr));
	  for(u_int i = args->GetLength(); i--; ) {
	    TagVal *argOpt = TagVal::FromWord(args->Sub(i));
	    if(argOpt != INVALID_POINTER) {
	      u_int dst = Store::DirectWordToInt(argOpt->Sel(0));
	      SETREG(dst, Scheduler::GetCurrentArg(i));
	    }
	  }
	}
        DISPATCH(PC);


      /**********************************************
       * macro definition for function calls/returns
       **********************************************/

#define SET_ARGS0() Scheduler::SetNArgs(0)
#define SET_ARGS1()                             \
    Scheduler::SetNArgs(1);                     \
    GET_1R(codeBuffer,PC,r);                    \
    Scheduler::SetCurrentArg(0, GETREG(r))
#define SET_ARGS2()                             \
    Scheduler::SetNArgs(2);                     \
    GET_1R(codeBuffer,PC,r0);                   \
    Scheduler::SetCurrentArg(1, GETREG(r0));    \
    GET_1R(codeBuffer,PC,r1);                   \
    Scheduler::SetCurrentArg(0, GETREG(r1))
#define SET_ARGS3()                             \
    Scheduler::SetNArgs(3);                     \
    GET_1R(codeBuffer,PC,r0);                   \
    Scheduler::SetCurrentArg(2, GETREG(r0));    \
    GET_1R(codeBuffer,PC,r1);                   \
    Scheduler::SetCurrentArg(1, GETREG(r1));    \
    GET_1R(codeBuffer,PC,r2);                   \
    Scheduler::SetCurrentArg(0, GETREG(r2))
#define SET_ARGS()                              \
    Scheduler::SetNArgs(nArgs);                 \
    for(u_int i = nArgs; i--; ) {               \
      GET_1R(codeBuffer,PC,r);                  \
      Scheduler::SetCurrentArg(i, GETREG(r));   \
    }
      

      /**************************
       * normal function call
       *************************/

#define PRELUDE_SEAMCALL0() GET_1R(codeBuffer,PC,reg)
#define PRELUDE_SEAMCALL1() PRELUDE_SEAMCALL0()
#define PRELUDE_SEAMCALL2() PRELUDE_SEAMCALL0()
#define PRELUDE_SEAMCALL3() PRELUDE_SEAMCALL0()
#define PRELUDE_SEAMCALL() GET_1R1I(codeBuffer,PC,reg,nArgs)

#define SEAM_CALL(NOA) {						\
	PRELUDE_SEAMCALL##NOA();					\
        SET_ARGS##NOA();						\
	SAVEPC(PC);							\
	word wClosure = GETREG(reg);					\
	Closure *closure = Closure::FromWord(wClosure);		        \
	if(closure != INVALID_POINTER) {				\
	  ConcreteCode *cc =						\
	    ConcreteCode::FromWord(closure->GetConcreteCode());		\
	  if(cc != INVALID_POINTER) {					\
	    Interpreter *interpreter = cc->GetInterpreter();		\
	    if(interpreter == this) {					\
	      RecordCall(closure);					\
	      PushCall(reinterpret_cast<ByteConcreteCode*>(cc), closure);	\
	      if(StatusWord::GetStatus()) return Worker::PREEMPT;	\
	      frame = reinterpret_cast<ByteCodeFrame *>(Scheduler::GetFrame());	\
	      concreteCode = reinterpret_cast<ByteConcreteCode*>(cc);	\
	      LOADSTATE(PC, CP, IP);					\
	      DISPATCH(PC);						\
	    }								\
	  }								\
	}								\
	/* preemption test happens in Scheduler::PushCall */		\
      	return Scheduler::PushCall(wClosure);				\
      }

    Case(seam_call)  SEAM_CALL();
    Case(seam_call0) SEAM_CALL(0);
    Case(seam_call1) SEAM_CALL(1);
    Case(seam_call2) SEAM_CALL(2);
    Case(seam_call3) SEAM_CALL(3);


    /***************************
     * function tailcall
     ***************************/

    // normal tailcalls

#define SEAM_TAILCALL(NOA) {						\
	PRELUDE_SEAMCALL##NOA();					\
        SET_ARGS##NOA();						\
	word wClosure = GETREG(reg);					\
	Closure *closure = Closure::FromWord(wClosure);			\
	if(closure == CP) {	\
	  RecordCall(closure);						\
	  /* check CC *after* RecordCall, since RecordCall might mutate closure */	\
	  if(ConcreteCode::FromWord(closure->GetConcreteCode()) == reinterpret_cast<ConcreteCode*>(concreteCode)) {	\
	    SETPC(0);							\
	    CHECK_PREEMPT();						\
	    DISPATCH(PC);						\
          }								\
	}								\
	Scheduler::PopFrame(frame->GetSize());				\
	if(closure != INVALID_POINTER) {				\
	  ConcreteCode *cc =						\
	    ConcreteCode::FromWord(closure->GetConcreteCode());		\
	  if(cc != INVALID_POINTER) {					\
	    Interpreter *interpreter = cc->GetInterpreter();		\
	    if(interpreter == this) {					\
	      RecordCall(closure);					\
	      PushCall(reinterpret_cast<ByteConcreteCode*>(cc), closure);	\
	      if(StatusWord::GetStatus()) return Worker::PREEMPT;	\
	      frame = reinterpret_cast<ByteCodeFrame *>(Scheduler::GetFrame());		\
	      concreteCode = reinterpret_cast<ByteConcreteCode*>(cc);	\
	      LOADSTATE(PC, CP, IP);					\
	      DISPATCH(PC);						\
	    }								\
	  }								\
	}								\
	/* preemption check happens in Scheduler::PushCall */		\
	return  Scheduler::PushCall(wClosure);				\
      }

    Case(seam_tailcall)  SEAM_TAILCALL();
    Case(seam_tailcall0) SEAM_TAILCALL(0);
    Case(seam_tailcall1) SEAM_TAILCALL(1);
    Case(seam_tailcall2) SEAM_TAILCALL(2);
    Case(seam_tailcall3) SEAM_TAILCALL(3);


      /*****************************
       * specialized function calls
       *****************************/
    
      // ATTENTION: some specialized calls require that the immediate
      //            closure argument can be obtained with 
      //            "Closure::FromWordDirect"
      //            The compiler performs the dereferencing.
    
      // self calls

#define PRELUDE_SELFCALL0() 
#define PRELUDE_SELFCALL1() PRELUDE_SELFCALL0()
#define PRELUDE_SELFCALL2() PRELUDE_SELFCALL0()
#define PRELUDE_SELFCALL3() PRELUDE_SELFCALL0()
#define PRELUDE_SELFCALL() GET_1I(codeBuffer,PC,nArgs)

// this is only for non-tail self-calls - tail-calls are implemented as jumps
#define SELF_CALL(NOA) {					\
	PRELUDE_SELFCALL##NOA();				\
        SET_ARGS##NOA();					\
	SAVEPC(PC);						\
	RecordCall(CP);						\
	ByteCodeInterpreter::PushCall(concreteCode, CP);	\
	if(StatusWord::GetStatus()) return Worker::PREEMPT;	\
	frame = reinterpret_cast<ByteCodeFrame *>(Scheduler::GetFrame());	\
	SETPC(0);						\
        DISPATCH(PC);						\
      }

    Case(self_call)  SELF_CALL();
    Case(self_call0) SELF_CALL(0);
    Case(self_call1) SELF_CALL(1);
    Case(self_call2) SELF_CALL(2);
    Case(self_call3) SELF_CALL(3);

      // rewrite calls

#define REWRITE_TEST(wClosure,instr)				\
    Closure *closure = Closure::FromWordDirect(wClosure);	\
    ConcreteCode *cc =						\
      ConcreteCode::FromWord(closure->GetConcreteCode());	\
    Interpreter *interpreter = cc->GetInterpreter();		\
    if(interpreter == ByteCodeInterpreter::self) {		\
      REWRITE_INSTR(codeBuffer,startPC,instr);			\
      PC = startPC;						\
      DISPATCH(PC);						\
    }								

#define PRELUDE_IMMEDIATE_CALL0() GET_1I(codeBuffer,PC,closureAddr)
#define PRELUDE_IMMEDIATE_CALL1() PRELUDE_IMMEDIATE_CALL0()
#define PRELUDE_IMMEDIATE_CALL2() PRELUDE_IMMEDIATE_CALL0()
#define PRELUDE_IMMEDIATE_CALL3() PRELUDE_IMMEDIATE_CALL0()
#define PRELUDE_IMMEDIATE_CALL()  GET_2I(codeBuffer,PC,closureAddr,nArgs)      

#define REWRITE_CALL(N) {					\
	PRELUDE_IMMEDIATE_CALL##N();				\
	word wClosure = IP->Sel(closureAddr);			\
	REWRITE_TEST(wClosure,ByteCodeInstr::bci_call##N);	\
	SET_ARGS##N();						\
	SAVEPC(PC);						\
	return Scheduler::PushCall(wClosure);			\
      }

    Case(rewrite_call)  REWRITE_CALL();
    Case(rewrite_call0) REWRITE_CALL(0);
    Case(rewrite_call1) REWRITE_CALL(1);
    Case(rewrite_call2) REWRITE_CALL(2);
    Case(rewrite_call3) REWRITE_CALL(3);

#define REWRITE_TAILCALL(N) {						\
	PRELUDE_IMMEDIATE_CALL##N();					\
	word wClosure = IP->Sel(closureAddr);				\
	REWRITE_TEST(wClosure,ByteCodeInstr::bci_tailcall##N);	\
	SET_ARGS##N();							\
	Scheduler::PopFrame(frame->GetSize());				\
	return Scheduler::PushCall(wClosure);				\
      }
      
    Case(rewrite_tailcall)  REWRITE_TAILCALL();
    Case(rewrite_tailcall0) REWRITE_TAILCALL(0);
    Case(rewrite_tailcall1) REWRITE_TAILCALL(1);
    Case(rewrite_tailcall2) REWRITE_TAILCALL(2);
    Case(rewrite_tailcall3) REWRITE_TAILCALL(3);

      // immediate call

#define IMMEDIATE_CALL(N) {			\
	PRELUDE_IMMEDIATE_CALL##N();		\
	SET_ARGS##N();				\
	SAVEPC(PC);				\
	word wClosure = IP->Sel(closureAddr);	\
	return Scheduler::PushCall(wClosure);	\
      }

    Case(immediate_call)  IMMEDIATE_CALL();
    Case(immediate_call0) IMMEDIATE_CALL(0);
    Case(immediate_call1) IMMEDIATE_CALL(1);
    Case(immediate_call2) IMMEDIATE_CALL(2);
    Case(immediate_call3) IMMEDIATE_CALL(3);

#define IMMEDIATE_TAILCALL(N) {			\
	PRELUDE_IMMEDIATE_CALL##N();		\
	SET_ARGS##N();				\
	word wClosure = IP->Sel(closureAddr);	\
	Scheduler::PopFrame(frame->GetSize());	\
	return Scheduler::PushCall(wClosure);	\
      } 

    Case(immediate_tailcall)  IMMEDIATE_TAILCALL();
    Case(immediate_tailcall0) IMMEDIATE_TAILCALL(0);
    Case(immediate_tailcall1) IMMEDIATE_TAILCALL(1);
    Case(immediate_tailcall2) IMMEDIATE_TAILCALL(2);
    Case(immediate_tailcall3) IMMEDIATE_TAILCALL(3);

      // immediate byte code call
      
#define BCI_CALL(N) {							\
	PRELUDE_IMMEDIATE_CALL##N();					\
	SET_ARGS##N();							\
	SAVEPC(PC);							\
	/* invariant: closure and concrete code are determined */	\
	Closure *closure = Closure::FromWordDirect(IP->Sel(closureAddr));	\
	concreteCode = ByteConcreteCode::FromWordDirect(closure->GetConcreteCode());	\
	RecordCall(closure);						\
	PushCall(concreteCode, closure);				\
	if(StatusWord::GetStatus()) return Worker::PREEMPT;		\
	frame = reinterpret_cast<ByteCodeFrame *>(Scheduler::GetFrame());	\
	LOADSTATE(PC, CP, IP);						\
	DISPATCH(PC);							\
      } 
  
    Case(bci_call)  BCI_CALL();
    Case(bci_call0) BCI_CALL(0);
    Case(bci_call1) BCI_CALL(1);
    Case(bci_call2) BCI_CALL(2);
    Case(bci_call3) BCI_CALL(3);

#define BCI_TAILCALL(N) {						\
	PRELUDE_IMMEDIATE_CALL##N();					\
	SET_ARGS##N();							\
	/* invariant: closure and concrete code are determined */	\
	Closure *closure = Closure::FromWordDirect(IP->Sel(closureAddr));	\
	concreteCode = ByteConcreteCode::FromWordDirect(closure->GetConcreteCode());	\
	RecordCall(closure);						\
	Scheduler::PopFrame(frame->GetSize());				\
	PushCall(concreteCode, closure);				\
	if(StatusWord::GetStatus()) return Worker::PREEMPT;		\
	frame = reinterpret_cast<ByteCodeFrame *>(Scheduler::GetFrame());	\
	LOADSTATE(PC, CP, IP);						\
	DISPATCH(PC);							\
      }
    
    Case(bci_tailcall)  BCI_TAILCALL();
    Case(bci_tailcall0) BCI_TAILCALL(0);
    Case(bci_tailcall1) BCI_TAILCALL(1);
    Case(bci_tailcall2) BCI_TAILCALL(2);
    Case(bci_tailcall3) BCI_TAILCALL(3);


    /****************************************
     * macro definitions for primitve calls
     ***************************************/

#define PRELUDE_PRIMCALL0() GET_2I(codeBuffer, PC, interpreterAddr, cFunctionAddr);
#define PRELUDE_PRIMCALL1() PRELUDE_PRIMCALL0()
#define PRELUDE_PRIMCALL2() PRELUDE_PRIMCALL0()
#define PRELUDE_PRIMCALL3() PRELUDE_PRIMCALL0()
#define PRELUDE_PRIMCALL()  GET_3I(codeBuffer, PC, interpreterAddr, cFunctionAddr, nArgs);

    /**************************
     * normal primitive call
     *************************/

#define SEAM_PRIM_CALL(NOA) {						\
	PRELUDE_PRIMCALL##NOA();					\
        SET_ARGS##NOA();						\
	SAVEPC(PC);							\
	NEW_STACK_FRAME(primFrame, reinterpret_cast<Interpreter*>(interpreterAddr), 0);			\
	Worker::Result res = reinterpret_cast<Interpreter::function>(cFunctionAddr)();		\
	StackFrame *newFrame = Scheduler::GetFrame();			\
	/* test if we can skip the scheduler */				\
	if(res == CONTINUE && !StatusWord::GetStatus()			\
	   && newFrame->GetWorker() == this) {				\
	  frame = reinterpret_cast<ByteCodeFrame*>(newFrame);		\
	  concreteCode = frame->GetConcreteCode();			\
	  LOADSTATE(PC, CP, IP);					\
	  DISPATCH(PC);							\
	}								\
	return res;							\
      }									
      
    Case(seam_prim_call)  SEAM_PRIM_CALL();
    Case(seam_prim_call0) SEAM_PRIM_CALL(0);
    Case(seam_prim_call1) SEAM_PRIM_CALL(1);
    Case(seam_prim_call2) SEAM_PRIM_CALL(2);
    Case(seam_prim_call3) SEAM_PRIM_CALL(3);


    /*******************************
     * tail primitive call
     *******************************/

#define SEAM_PRIM_TAILCALL(NOA) {					\
	PRELUDE_PRIMCALL##NOA();					\
        SET_ARGS##NOA();						\
	Scheduler::PopFrame(frame->GetSize());				\
	NEW_STACK_FRAME(primFrame, reinterpret_cast<Interpreter*>(interpreterAddr), 0);	\
	Worker::Result res = reinterpret_cast<Interpreter::function>(cFunctionAddr)();		\
	StackFrame *newFrame = Scheduler::GetFrame();			\
	/* test if we can skip the scheduler */				\
	if(res == CONTINUE && !StatusWord::GetStatus()			\
	   && newFrame->GetWorker() == this) {				\
	  frame = reinterpret_cast<ByteCodeFrame*>(newFrame);		\
	  concreteCode = frame->GetConcreteCode();			\
	  LOADSTATE(PC, CP, IP);					\
	  DISPATCH(PC);							\
	}								\
	return res;							\
      }
    
    Case(seam_prim_tailcall)   SEAM_PRIM_TAILCALL();
    Case(seam_prim_tailcall0)  SEAM_PRIM_TAILCALL(0);
    Case(seam_prim_tailcall1)  SEAM_PRIM_TAILCALL(1);
    Case(seam_prim_tailcall2)  SEAM_PRIM_TAILCALL(2);
    Case(seam_prim_tailcall3)  SEAM_PRIM_TAILCALL(3);


    /*****************************
     * return from function call
     *****************************/

#define PRELUDE_RETURN0() 
#define PRELUDE_RETURN1() PRELUDE_RETURN0()
#define PRELUDE_RETURN2() PRELUDE_RETURN0()
#define PRELUDE_RETURN3() PRELUDE_RETURN0()
#define PRELUDE_RETURN()  GET_1I(codeBuffer,PC,nArgs)

#define SEAM_RETURN(NOA) {				\
	PRELUDE_RETURN##NOA();				\
        SET_ARGS##NOA();				\
	Scheduler::PopFrame(frame->GetSize());		\
	StackFrame *newFrame = Scheduler::GetFrame();	\
        /* dynamic test */ 				\
	if(newFrame->GetWorker() == this) {		\
	  frame = reinterpret_cast<ByteCodeFrame*>(newFrame);	\
	  concreteCode = frame->GetConcreteCode();	\
	  LOADSTATE(PC, CP, IP);			\
	  DISPATCH(PC);					\
	}						\
	/* preempt check only in call instructions */	\
        return Worker::CONTINUE; 			\
      }

    Case(seam_return)  SEAM_RETURN();
    Case(seam_return0) SEAM_RETURN(0);
    Case(seam_return1) SEAM_RETURN(1);
    Case(seam_return2) SEAM_RETURN(2);
    Case(seam_return3) SEAM_RETURN(3);

    // special case
    Case(seam_return_zero)
      {
	Scheduler::SetNArgs(1);
	Scheduler::SetCurrentArg(0, Store::IntToWord(0));
	Scheduler::PopFrame(frame->GetSize());
	StackFrame *newFrame = Scheduler::GetFrame();
	/* dynamic test */
	if(newFrame->GetWorker() == this) {
	  frame = reinterpret_cast<ByteCodeFrame*>(newFrame);
	  concreteCode = frame->GetConcreteCode();
	  LOADSTATE(PC, CP, IP);
	  DISPATCH(PC);
	}
	/* preemption check only in call instructions */
	return Worker::CONTINUE; // pass control to scheduler
      }
       

      /**************************************
       * closure building and initialization 
       **************************************/

    Case(mk_closure) // reg, code, size
      {
	GET_1R2I(codeBuffer,PC,reg,codeAddr,size);
	Closure *closure = Closure::New(IP->Sel(codeAddr),size);
	SETREG(reg, closure->ToWord());
      }
      DISPATCH(PC);

    Case(init_closure) // r0, r1, index
      {
	GET_2R1I(codeBuffer,PC,r0,r1,index);
	Closure *closure = Closure::FromWord(GETREG(r0));
	closure->Init(index,GETREG(r1));
      }
      DISPATCH(PC);

    Case(spec_closure) // r0, r1, template, size
      {
	GET_2R2I(codeBuffer,PC,r0,r1,tempAddr,size);
	TagVal *templ = TagVal::FromWordDirect(IP->Sel(tempAddr));
	TagVal *abstractCode =
	  TagVal::New(AbstractCode::Function, AbstractCode::functionWidth);    
	abstractCode->Init(0, templ->Sel(0));
	abstractCode->Init(1, GETREG(r1)); // substitution
	abstractCode->Init(2, templ->Sel(2));
	abstractCode->Init(3, templ->Sel(3));
	abstractCode->Init(4, templ->Sel(4));
	abstractCode->Init(5, templ->Sel(5));
	abstractCode->Init(6, templ->Sel(6));
	word wConcreteCode =
	  AliceLanguageLayer::concreteCodeConstructor(abstractCode);
	Closure *closure = Closure::New(wConcreteCode,size);
	SETREG(r0, closure->ToWord());
      }
      DISPATCH(PC);

    /********************************
     * handling of external requests
     *******************************/

    Case(await) // reg
      {
	GET_1R(codeBuffer,PC,reg);
	word requestWord = GETREG(reg);
	Transient *transient = Store::WordToTransient(requestWord);
	if(transient != INVALID_POINTER) {
	  SAVEPC(startPC); // repeat await until value is not a transient or is failed
	  Scheduler::SetCurrentData(requestWord);
	  Scheduler::SetNArgs(0);
	  return Worker::REQUEST; 
	}
      }
      DISPATCH(PC);
  

    /***************************************
     * basic jumps
     **************************************/

    Case(jump) // target
      {
	GET_1I(codeBuffer,PC,offset);
	PC += static_cast<s_int>(offset);
      }
      DISPATCH(PC);

    
    Case(check_preempt_jump) // target
      {
	GET_1I(codeBuffer,PC,offset);
	PC += static_cast<s_int>(offset);
	if(StatusWord::GetStatus()) {
	  SAVEPC(PC);
	  return Worker::PREEMPT;
	}
      }
      DISPATCH(PC);


      /**********************************
       * integer instructions
       **********************************/

    Case(itest) // r, map
      {
	GET_1R1I(codeBuffer,PC,reg,iaddr);
	IntMap *map = IntMap::FromWordDirect(IP->Sel(iaddr));
	word testVal = GETREG(reg);
	REQUEST_INT(dummy,testVal);
	// this is needed due to possible pointer chains
	testVal = Store::IntToWord(dummy); 
	word jmp = map->CondGet(testVal);
	if(jmp != INVALID_POINTER) 
	  PC += Store::DirectWordToInt(jmp);
      }
      DISPATCH(PC);

    Case(citest) // reg, offset, size
      {
	GET_1R2I(codeBuffer,PC,reg,offset,size);
	REQUEST_INT(relJump,GETREG(reg));
	s_int index = relJump - offset; 
	if(index < size) {
	  ProgramCounter addr = PC + index;
	  GET_1I(codeBuffer,addr,target);
	  PC += target;
	} else {
	  PC += size; // jump over the jump table
	}
      }
      DISPATCH(PC);

    Case(ijump_eq) // r, val, target
      {
	GET_1R2I(codeBuffer,PC,reg,number,jumpTarget);
	REQUEST_INT(cmpNumber,GETREG(reg));
	if(cmpNumber == number) 
	  PC += jumpTarget;
      }
      DISPATCH(PC);

    Case(isub) // r0, r1, r2
      {
	GET_3R(codeBuffer,PC,r0,r1,r2);		
#if defined(__i386__)
	word xw = PointerOp::Deref(GETREG(r1));
	if (PointerOp::IsTransient(xw))
	  REQUEST(xw);
	word yw = PointerOp::Deref(GETREG(r2));
	if (PointerOp::IsTransient(yw))
	  REQUEST(yw);
	
	__asm__ __volatile__("sub %[yw],%[xw]\n\t"
			     "jo asm_raiseoverflow\n\t"
			     "incl %0"
			     : [xw] "+r"(xw)
			     : [yw] "g"(yw)
			     : "cc"
			     );

	SETREG(r0, xw);
#elif defined(__ppc__)
	word xw = PointerOp::Deref(GETREG(r1));
	if (PointerOp::IsTransient(xw))
	  REQUEST(xw);
	register word yw = PointerOp::Deref(GETREG(r2));
	if (PointerOp::IsTransient(yw))
	  REQUEST(yw);
	__asm__ __volatile__("subfo. %[xw],%[yw],%[xw]\n\t"
			     "bso asm_raiseoverflow\n\t"
			     "addi %[xw],%[xw],1"
			     : [xw] "+r"(xw)
			     : [yw] "r"(yw)
			     : "cc"
			     );
	SETREG(r0, xw);
#else
	REQUEST_INT(x,GETREG(r1));
	REQUEST_INT(y,GETREG(r2));
	s_int diff = x-y;
	if (diff < MIN_VALID_INT || diff > MAX_VALID_INT) {
	  RAISE(PrimitiveTable::General_Overflow);
	}	  
	else
	  SETREG(r0, Store::IntToWord(diff));
#endif
      }
      DISPATCH(PC);

    Case(iadd) // r0, r1, r2
      {
	GET_3R(codeBuffer,PC,r0,r1,r2);

#if defined(__i386__)
	word xw = PointerOp::Deref(GETREG(r1));
	if (PointerOp::IsTransient(xw))
	  REQUEST(xw);
	word yw = PointerOp::Deref(GETREG(r2));
	if (PointerOp::IsTransient(yw))
	  REQUEST(yw);

	__asm__ __volatile__("add %[yw],%[xw]\n\t"
			     "jo asm_raiseoverflow\n\t"
			     "decl %0\n\t"
			     : [xw] "+r"(xw)
			     : [yw] "g"(yw)
			     : "cc"
			     );
	SETREG(r0, xw);
#elif defined(__ppc__)
	word xw = PointerOp::Deref(GETREG(r1));
	if (PointerOp::IsTransient(xw))
	  REQUEST(xw);
	register word yw = PointerOp::Deref(GETREG(r2));
	if (PointerOp::IsTransient(yw))
	  REQUEST(yw);
	__asm__ __volatile__("addo. %[xw],%[xw],%[yw]\n\t"
			     "bso asm_raiseoverflow\n\t"
			     "addi %[xw],%[xw],-1"
			     : [xw] "+r"(xw)
			     : [yw] "r"(yw)
			     : "cc"
			     );
	SETREG(r0, xw);
#else
	REQUEST_INT(x,GETREG(r1));
	REQUEST_INT(y,GETREG(r2));
	s_int sum = x + y;
	if (sum < MIN_VALID_INT || sum > MAX_VALID_INT)
	  RAISE(PrimitiveTable::General_Overflow)
	else
	  SETREG(r0, Store::IntToWord(sum));
#endif
      }
      DISPATCH(PC);

    Case(iinc) // r0, r1
      {
	GET_2R(codeBuffer,PC,r0,r1);
#if defined(__i386__)
	word xw = PointerOp::Deref(GETREG(r1));
	if (PointerOp::IsTransient(xw))
	  REQUEST(xw);
	
	__asm__ __volatile__("add $2,%[xw]\n\t"
			     "jo asm_raiseoverflow\n\t"
			     : [xw] "+r"(xw)
			     :
			     : "cc"
			     );

	SETREG(r0, xw);
#elif defined(__ppc__)
	word xw = PointerOp::Deref(GETREG(r1));
	if (PointerOp::IsTransient(xw))
	  REQUEST(xw);
	register int yw = 2;
	__asm__ __volatile__("addo. %[xw],%[xw],%[yw]\n\t"
			     "bso asm_raiseoverflow\n\t"
			     : [xw] "+r"(xw)
			     : [yw] "r"(yw)
			     : "cc"
			     );
	SETREG(r0, xw);
#else
	REQUEST_INT(x,GETREG(r1));
	s_int result = x+1;
	if (result > MAX_VALID_INT) 
	  RAISE(PrimitiveTable::General_Overflow) 	
	else
	  SETREG(r0, Store::IntToWord(result));	
#endif
      }
      DISPATCH(PC);

    Case(idec) // r0, r1
      {
	GET_2R(codeBuffer,PC,r0,r1);
#if defined(__i386__)
	word xw = PointerOp::Deref(GETREG(r1));
	if (PointerOp::IsTransient(xw))
	  REQUEST(xw);
	
	__asm__ __volatile__("sub $2,%[xw]\n\t"
			     "jo asm_raiseoverflow\n\t"
			     : [xw] "+r"(xw)
			     :
			     : "cc"
			     );

	SETREG(r0, xw);
#elif defined(__ppc__)
	word xw = PointerOp::Deref(GETREG(r1));
	if (PointerOp::IsTransient(xw))
	  REQUEST(xw);
	register int yw = 2;
	__asm__ __volatile__("subfo. %[xw],%[yw],%[xw]\n\t"
			     "bso asm_raiseoverflow\n\t"
			     : [xw] "+r"(xw)
			     : [yw] "r"(yw)
			     : "cc"
			     );
	SETREG(r0, xw);
#else
	REQUEST_INT(x,GETREG(r1));
	s_int result = x-1;
	if (result < MIN_VALID_INT) 
	  RAISE(PrimitiveTable::General_Overflow) 	
	else
	  SETREG(r0, Store::IntToWord(result));	
#endif
      }
      DISPATCH(PC);
      
    Case(iequal) // r0, r1, i0
      {
	GET_2R1I(codeBuffer, PC, dst, test, cons);
	word wTest = PointerOp::Deref(GETREG(test));
	if (PointerOp::IsTransient(wTest)) {
	  REQUEST(wTest);
	}
	else if (PointerOp::IsInt(wTest)) {
	  s_int iTest = Store::DirectWordToInt(wTest);
	  SETREG(dst, Store::IntToWord(iTest == static_cast<s_int>(cons)));
	}
	else {
	  SETREG(dst, Store::IntToWord(0));
	}
      }
      DISPATCH(PC);

#define INLINE_INT_COMPARE(op) {		\
      GET_3R(codeBuffer, PC, dst, a, b);	\
      word wA = GETREG(a);			\
      s_int iA = Store::WordToInt(wA);		\
      if (iA == INVALID_INT) {			\
	REQUEST(wA);				\
      }						\
      word wB = GETREG(b);			\
      s_int iB = Store::WordToInt(wB);		\
      if (iB == INVALID_INT) {		\
	REQUEST(wB);				\
      }						\
      SETREG(dst, Store::IntToWord(iA op iB));	\
      DISPATCH(PC);				\
    }

    Case(iless)       INLINE_INT_COMPARE(<);
    Case(igreater)    INLINE_INT_COMPARE(>);
    Case(iless_eq)    INLINE_INT_COMPARE(<=);
    Case(igreater_eq) INLINE_INT_COMPARE(>=);
     
      /**********************************
       * real instructions
       **********************************/

    Case(rtest) // r, map
      {
	GET_1R1I(codeBuffer,PC,reg,iaddr);
	ChunkMap *map = ChunkMap::FromWordDirect(IP->Sel(iaddr));
	word testVal = GETREG(reg);
	REQUEST_WORD(Real,dummy,testVal);
	testVal = dummy->ToWord();
	word jmp = map->CondGet(testVal);
	if(jmp != INVALID_POINTER)
	  PC += Store::DirectWordToInt(jmp);
      }
      DISPATCH(PC);

    Case(rjump_eq) // r, val, target
      {
	GET_1R2I(codeBuffer,PC,reg,iaddr,jumpOffset);
	REQUEST_WORD(Real,cmpValue,GETREG(reg));
	Real *real = Real::FromWordDirect(IP->Sel(iaddr));
	if(cmpValue->GetValue() == real->GetValue()) 
	  PC += jumpOffset;
      }
      DISPATCH(PC);


      /**********************************
       * string instructions
       *********************************/

    Case(stest) // r, map
      {
	GET_1R1I(codeBuffer,PC,reg,iaddr);
	ChunkMap *map = ChunkMap::FromWordDirect(IP->Sel(iaddr));
	word testVal = GETREG(reg);
	REQUEST_WORD(String,dummy,testVal);
	testVal = dummy->ToWord();
	word jmp = map->CondGet(testVal);
	if(jmp != INVALID_POINTER) 
	  PC += Store::DirectWordToInt(jmp);
      }
      DISPATCH(PC);

    Case(sjump_eq) // r, val, target
      {
	GET_1R2I(codeBuffer,PC,reg,iaddr,jumpOffset);
	REQUEST_WORD(String,s2,GETREG(reg));
	String *s1 = String::FromWordDirect(IP->Sel(iaddr));
	u_int s1Size = s1->GetSize();
	if (s1Size == s2->GetSize() &&
	      !std::memcmp(s1->GetValue(), s2->GetValue(), s1Size)) 
	  PC += jumpOffset;
      }
      DISPATCH(PC);


      /******************************************
       * basic register loads
       *****************************************/

    Case(load_global) // r, i
      {
	GET_1R1I(codeBuffer,PC,reg,index);
	SETREG(reg, CP->Sub(index));
      }
      DISPATCH(PC);

    Case(load_immediate) // r, i
      {
	GET_1R1I(codeBuffer,PC,reg,iaddr);
	SETREG(reg, IP->Sel(iaddr));
      }
      DISPATCH(PC);

    Case(load_int) // r, int
      {
	GET_1R1I(codeBuffer,PC,reg,number);
	SETREG(reg, Store::IntToWord(static_cast<s_int>(number))); 
      }
      DISPATCH(PC);

    Case(load_zero) // r
      {
	GET_1R(codeBuffer,PC,reg);
	SETREG(reg, Store::IntToWord(0));
      }
      DISPATCH(PC);

    Case(load_reg) // r0, r1
      {
	GET_2R(codeBuffer,PC,r0,r1);
	SETREG(r0, GETREG(r1));
      }
      DISPATCH(PC);

    Case(swap_regs) // r0, r1
      {
	GET_2R(codeBuffer,PC,r0,r1);
	word tmp = GETREG(r0);	
	SETREG(r0, GETREG(r1));
	SETREG(r1, tmp);
      }
      DISPATCH(PC);

      /*******************************
       * basic register sets (stores)
       *******************************/

    Case(set_global) // reg, addr
      {
	GET_1R1I(codeBuffer,PC,reg,addr);
	CP->Init(addr,GETREG(reg)); // or Update ???
      }
      DISPATCH(PC);


      /****************************
       * reference cells
       ***************************/

    Case(load_cell) // r0, r1
      {
	GET_2R(codeBuffer,PC,r0,r1);
	REQUEST_WORD(Cell,cell,GETREG(r1));
	SETREG(r0, cell->Access());
      }  
      DISPATCH(PC);

    Case(set_cell) // r0, r1
      {
	GET_2R(codeBuffer,PC,r0,r1);
	REQUEST_WORD(Cell,cell,GETREG(r0));
	cell->Assign(GETREG(r1));
      }
      DISPATCH(PC);

    Case(new_cell) // r0, r1
      {
	GET_2R(codeBuffer,PC,r0,r1);
	SETREG(r0, Cell::New(GETREG(r1))->ToWord());
      }
      DISPATCH(PC);


      /*****************************
       * vector instructions
       *****************************/

    Case(new_vec) // r, size
      {
	GET_1R1I(codeBuffer,PC,reg,size);
	SETREG(reg, Vector::New(size)->ToWord());
      }
      DISPATCH(PC);

    Case(init_vec) // r0, r1, index
      {
	GET_2R1I(codeBuffer,PC,r0,r1,index);
	Vector *vec = Vector::FromWord(GETREG(r0));
	vec->Init(index,GETREG(r1));
      }
      DISPATCH(PC);

    Case(load_vec) // r0, r1, index
      {
	GET_2R1I(codeBuffer,PC,r0,r1,index);
	Vector *vec = Vector::FromWord(GETREG(r1));
	SETREG(r0, vec->Sub(index));
      }
      DISPATCH(PC);

    Case(vectest) // r, map
      {
	GET_1R1I(codeBuffer,PC,reg,iaddr);
	IntMap *map = IntMap::FromWord(IP->Sel(iaddr));
	REQUEST_WORD(Vector,vec,GETREG(reg));
	word size = Store::IntToWord(vec->GetLength());
	word jmp = map->CondGet(size);
	if(jmp != INVALID_POINTER)
	  PC += Store::DirectWordToInt(jmp);
      }
      DISPATCH(PC);


      /*****************************
       * tuple instructions
       ****************************/

      // basic instructions

    Case(new_tup) // r, size
      {
	GET_1R1I(codeBuffer, PC, reg, size);
	SETREG(reg, Tuple::New(size)->ToWord());	
      }
      DISPATCH(PC);

    Case(select_tup) // r0, r1, index
      {
	GET_2R1I(codeBuffer,PC,r0,r1,index);
	REQUEST_WORD(Tuple,tuple,GETREG(r1));
	SETREG(r0, tuple->Sel(index));
      }
      DISPATCH(PC);

    Case(init_tup) // r0, r1, index 
      {
	GET_2R1I(codeBuffer,PC,r0,r1,index);
	Tuple *tuple = Tuple::FromWordDirect(GETREG(r0));
	tuple->Init(index, GETREG(r1));
      }
      DISPATCH(PC);

      // specialized instructions

    Case(new_pair) // r0 <- (r1,r2)
      {
	GET_3R(codeBuffer,PC,r0,r1,r2);
	Tuple *pair = Tuple::New(2);
	pair->Init(0, GETREG(r1));
	pair->Init(1, GETREG(r2));
	SETREG(r0, pair->ToWord());	
      }
      DISPATCH(PC);

    Case(new_triple) // r0 <- (r1,r2,r3)
      {
	GET_4R(codeBuffer,PC,r0,r1,r2,r3);
	Tuple *triple = Tuple::New(3);
	triple->Init(0, GETREG(r1));
	triple->Init(1, GETREG(r2));
	triple->Init(2, GETREG(r3));
	SETREG(r0, triple->ToWord());	
      }
      DISPATCH(PC);

    Case(get_tup2) // (r0,r1) <- r2
      {
	GET_3R(codeBuffer,PC,r0,r1,r2);
	REQUEST_WORD(Tuple,pair,GETREG(r2));
	SETREG(r0, pair->Sel(0));
	SETREG(r1, pair->Sel(1));
      }
      DISPATCH(PC);

    Case(get_tup3) // (r0,r1,r2) <- r3
      {
	GET_4R(codeBuffer,PC,r0,r1,r2,r3);
	REQUEST_WORD(Tuple,triple,GETREG(r3));
	SETREG(r0, triple->Sel(0));
	SETREG(r1, triple->Sel(1));
	SETREG(r2, triple->Sel(2));
      }
      DISPATCH(PC);

#define SELECT_TUP(index) {			\
	GET_2R(codeBuffer,PC,r1,r2);		\
	REQUEST_WORD(Tuple,tuple,GETREG(r2));	\
	SETREG(r1, tuple->Sel(index));		\
	DISPATCH(PC);				\
      }

    Case(select_tup0) SELECT_TUP(0);
    Case(select_tup1) SELECT_TUP(1);
    Case(select_tup2) SELECT_TUP(2);

      /****************************
       * tagval instructions
       ****************************/

    // generic construction and initialisation

#define NEW_TAGVAL(TagValType) {			\
	GET_1R2I(codeBuffer,PC,reg,size,tag);		\
	TagValType *tagVal = TagValType::New(tag,size);	\
	SETREG(reg, tagVal->ToWord());			\
	DISPATCH(PC);					\
      }

    Case(new_tagval)    NEW_TAGVAL(TagVal);
    Case(new_bigtagval) NEW_TAGVAL(BigTagVal);

#define INIT_TAGVAL(TagValType) {				\
	GET_2R1I(codeBuffer,PC,r0,r1,index);			\
	TagValType *tagVal = TagValType::FromWord(GETREG(r0));	\
	tagVal->Init(index,GETREG(r1));				\
	DISPATCH(PC);						\
      }

    Case(init_tagval)    INIT_TAGVAL(TagVal);
    Case(init_bigtagval) INIT_TAGVAL(BigTagVal);

    // super-instructions for construction+initialisation

#define INIT_TAG(tagVal,index) {		\
      GET_1R(codeBuffer,PC,reg);		\
      tagVal->Init(index,GETREG(reg));		\
    }
#define INIT_TAGV1(tagVal) INIT_TAG(tagVal,0)
#define INIT_TAGV2(tagVal) INIT_TAG(tagVal,1); INIT_TAGV1(tagVal); 
#define INIT_TAGV3(tagVal) INIT_TAG(tagVal,2); INIT_TAGV2(tagVal) 
#define INIT_TAGV4(tagVal) INIT_TAG(tagVal,3); INIT_TAGV3(tagVal) 
#define INIT_TAGV(tagVal) {			\
  for(u_int i=size; i--; ) INIT_TAG(tagVal,i)	\
}

#define TV_PRELUDE1() u_int size = 1; GET_1R1I(codeBuffer,PC,reg,tag);
#define TV_PRELUDE2() u_int size = 2; GET_1R1I(codeBuffer,PC,reg,tag);
#define TV_PRELUDE3() u_int size = 3; GET_1R1I(codeBuffer,PC,reg,tag);
#define TV_PRELUDE4() u_int size = 4; GET_1R1I(codeBuffer,PC,reg,tag);
#define TV_PRELUDE()  GET_1R2I(codeBuffer,PC,reg,tag,size);

#define NEW_TAGVAL_INIT(TagValType,N) {			\
      TV_PRELUDE##N();                                  \
      TagValType *tagVal = TagValType::New(tag,size);	\
      INIT_TAGV##N(tagVal);				\
      SETREG(reg, tagVal->ToWord());			\
      DISPATCH(PC);					\
    }

    Case(new_tagval_init)  NEW_TAGVAL_INIT(TagVal, );
    Case(new_tagval_init1) NEW_TAGVAL_INIT(TagVal,1);
    Case(new_tagval_init2) NEW_TAGVAL_INIT(TagVal,2);
    Case(new_tagval_init3) NEW_TAGVAL_INIT(TagVal,3);
    Case(new_tagval_init4) NEW_TAGVAL_INIT(TagVal,4);

    Case(new_bigtagval_init)  NEW_TAGVAL_INIT(BigTagVal, );
    Case(new_bigtagval_init1) NEW_TAGVAL_INIT(BigTagVal,1);
    Case(new_bigtagval_init2) NEW_TAGVAL_INIT(BigTagVal,2);
    Case(new_bigtagval_init3) NEW_TAGVAL_INIT(BigTagVal,3);
    Case(new_bigtagval_init4) NEW_TAGVAL_INIT(BigTagVal,4);

    // generic load instructions

#define LOAD_TAGVAL(TagValType) {				\
	GET_2R1I(codeBuffer,PC,r0,r1,index);			\
	TagValType *tagVal = TagValType::FromWord(GETREG(r1));	\
	SETREG(r0, tagVal->Sel(index));				\
	DISPATCH(PC);						\
      }

    Case(load_tagval)    LOAD_TAGVAL(TagVal);
    Case(load_bigtagval) LOAD_TAGVAL(BigTagVal);

    // super-instructions for simultaneous loading of several fields

#define LOAD_TAGVAL1(TagValType) {				\
	GET_2R(codeBuffer,PC,r0,r1);				\
	TagValType *tagVal = TagValType::FromWord(GETREG(r1));	\
	SETREG(r0, tagVal->Sel(0));				\
	DISPATCH(PC);						\
      }

    Case(load_tagval1)    LOAD_TAGVAL1(TagVal);
    Case(load_bigtagval1) LOAD_TAGVAL1(BigTagVal);

#define LOAD_TAGVAL2(TagValType) {				\
	GET_3R(codeBuffer,PC,r0,r1,r2);				\
	TagValType *tagVal = TagValType::FromWord(GETREG(r2));	\
	SETREG(r0, tagVal->Sel(0));				\
	SETREG(r1, tagVal->Sel(1));				\
	DISPATCH(PC);						\
      }

    Case(load_tagval2)    LOAD_TAGVAL2(TagVal);
    Case(load_bigtagval2) LOAD_TAGVAL2(BigTagVal);

#define LOAD_TAGVAL3(TagValType) {				\
	GET_4R(codeBuffer,PC,r0,r1,r2,r3);			\
	TagValType *tagVal = TagValType::FromWord(GETREG(r3));	\
	SETREG(r0, tagVal->Sel(0));				\
	SETREG(r1, tagVal->Sel(1));				\
	SETREG(r2, tagVal->Sel(2));				\
	DISPATCH(PC);						\
      }

    Case(load_tagval3)    LOAD_TAGVAL3(TagVal);
    Case(load_bigtagval3) LOAD_TAGVAL3(BigTagVal);

    // general tagtest instructions

#define TAGTEST(TagValType) {						\
	GET_1R1I(codeBuffer,PC,reg,iaddr);				\
	IntMap *map = IntMap::FromWordDirect(IP->Sel(iaddr));		\
	word testVal = GETREG(reg);					\
	TagValType *tagVal = TagValType::FromWord(testVal);		\
	word tag;							\
	if(tagVal == INVALID_POINTER) {					\
	  REQUEST_INT(dummy,testVal);					\
	  tag = Store::IntToWord(dummy);				\
	}								\
	else {								\
	  tag = Store::IntToWord(tagVal->GetTag());			\
	}								\
	word jmp = map->CondGet(tag);					\
	if(jmp != INVALID_POINTER)					\
	  PC += Store::DirectWordToInt(jmp);				\
	DISPATCH(PC);							\
      }

    Case(tagtest)    TAGTEST(TagVal);
    Case(bigtagtest) TAGTEST(BigTagVal);

    // special case: test table of size 1

#define TAGTEST1(TagValType)       {				\
      GET_1R2I(codeBuffer,PC,reg,tag,target);			\
      word testVal = GETREG(reg);				\
      TagValType *tagVal = TagValType::FromWord(testVal);	\
      u_int testTag;						\
      if(tagVal == INVALID_POINTER) {				\
	REQUEST_INT(dummy,testVal);				\
	testTag = dummy;					\
      }								\
      else {							\
	testTag = tagVal->GetTag();				\
      }								\
      if(testTag == tag)					\
	PC += target;						\
      DISPATCH(PC);						\
    }

    Case(tagtest1)    TAGTEST1(TagVal);
    Case(bigtagtest1) TAGTEST1(BigTagVal);

    // compact tag tests with inlined test table

#define CTAGTEST(TagValType)    {				\
      GET_1R1I(codeBuffer,PC,reg,size);				\
      word testVal = GETREG(reg);				\
      TagValType *tagVal = TagValType::FromWord(testVal);	\
      u_int tag;						\
      if(tagVal == INVALID_POINTER) {				\
	REQUEST_INT(dummy,testVal);				\
	tag = dummy;						\
      }								\
      else							\
	tag = tagVal->GetTag();					\
      /* range check */						\
      if(tag < size) {						\
	ProgramCounter addr = PC + tag;				\
	GET_1I(codeBuffer,addr,target);				\
	PC += target;						\
      } else {							\
	PC += size; /* jump over the jump table */		\
      }								\
      DISPATCH(PC);						\
    }

    Case(ctagtest) CTAGTEST(TagVal);
    Case(cbigtagtest) CTAGTEST(BigTagVal);

#define CTAGTEST_DIRECT(TagValType) {				\
      GET_1R1I(codeBuffer,PC,reg,size);				\
      word testVal = GETREG(reg);				\
      TagValType *tagVal = TagValType::FromWord(testVal);	\
      u_int tag;						\
      if(tagVal == INVALID_POINTER) {				\
	REQUEST_INT(dummy,testVal);				\
	tag = dummy;						\
      }								\
      else							\
	tag = tagVal->GetTag();					\
      ProgramCounter addr = PC+tag;				\
      GET_1I(codeBuffer,addr,target);				\
      PC += target;						\
      DISPATCH(PC);						\
    }								
								
    Case(ctagtest_direct) CTAGTEST_DIRECT(TagVal);		
    Case(cbigtagtest_direct) CTAGTEST_DIRECT(BigTagVal);


      /*****************************
       * constructor instructions
       *****************************/

    Case(new_con) // r, name
      {
	GET_1R1I(codeBuffer,PC,r,nameAddr);
	Constructor *constructor = 
	  Constructor::New(String::FromWordDirect(IP->Sel(nameAddr)));
	SETREG(r, constructor->ToWord());
      }
      DISPATCH(PC);

    Case(prepare_con) // r0, r1, size
      {
	GET_2R1I(codeBuffer,PC,r0,r1,size);
	word requestWord = GETREG(r1);
	Block *constructor = Store::WordToBlock(requestWord);
	if(constructor == INVALID_POINTER)
	  REQUEST(requestWord);
	ConVal *conVal = ConVal::New(constructor, size);
	SETREG(r0, conVal->ToWord());
      }
      DISPATCH(PC);

    Case(init_con) // r0, r1, index
      {
	GET_2R1I(codeBuffer,PC,r0,r1,index);
	ConVal *conVal = ConVal::FromWord(GETREG(r0)); // or Direct ???
	conVal->Init(index,GETREG(r1));
      }
      DISPATCH(PC);

    Case(load_con) // r0, r1, index
      {
	GET_2R1I(codeBuffer,PC,r0,r1,index);
	ConVal *conVal = ConVal::FromWord(GETREG(r1));
	SETREG(r0, conVal->Sel(index));
      }
      DISPATCH(PC);

    Case(contest) // r0, r1, target
      {
	GET_2R1I(codeBuffer,PC,r0,r1,target);
	REQUEST_WORD(ConVal,conVal,GETREG(r0));
	Block *testConstructor = Store::WordToBlock(GETREG(r1));
	if(testConstructor == INVALID_POINTER) REQUEST(GETREG(r1));
   
	Block *constructor;
	if(conVal->IsConVal()) 
	  constructor = conVal->GetConstructor();
	else
	  constructor = Store::DirectWordToBlock(conVal->ToWord());
	
	if(testConstructor != constructor) 
	  PC += target;
      }
      DISPATCH(PC);


      // check if we can bundle new/prepare/set_con(arg) into one instruction

      /****************************
       * polymorphic records
       ***************************/

    Case(new_polyrec) // reg, labelVec
      {
	GET_1R1I(codeBuffer,PC,reg,labelsAddr);
	Vector *labels = Vector::FromWordDirect(IP->Sel(labelsAddr));
	Record *record = Record::New(labels);
	SETREG(reg, record->ToWord());
      }
      DISPATCH(PC);

    Case(init_polyrec) // r0,r1,index (according to labelVec from new_polyrec)
      {
	GET_2R1I(codeBuffer,PC,r0,r1,index);
	Record *record = Record::FromWord(GETREG(r0));
	record->Init(index,GETREG(r1));
      }
      DISPATCH(PC);

    Case(lazyselect_polyrec) // r0, r1, labelAddr
      {
	GET_2R1I(codeBuffer,PC,r0,r1,labelAddr);
	word wRecord = GETREG(r1);
	Record *record = Record::FromWord(wRecord);
	UniqueString *label = UniqueString::FromWordDirect(IP->Sel(labelAddr));
	if (record == INVALID_POINTER) { // transient -> create byneed
	  BCI_DEBUG("lazy select of %s\n",label->ToString()->ExportC());
	  LazySelClosure *closure = LazySelClosure::New(wRecord, label);
	  Byneed *byneed = Byneed::New(closure->ToWord());
	  SETREG(r0, byneed->ToWord());
	}
	else {
	  BCI_DEBUG("non-lazy select of %s\n",label->ToString()->ExportC());
	  SETREG(r0, record->PolySel(label));
	}
      }
      DISPATCH(PC);

    Case(lazyselect_polyrec_n) // r0, regs, labels
      {
	GET_1R2I(codeBuffer,PC,r0,regsAddr,labelsAddr);
	Vector *regs = Vector::FromWordDirect(IP->Sel(regsAddr));
	Vector *labels = Vector::FromWordDirect(IP->Sel(labelsAddr));
	word wRecord = GETREG(r0);
	Record *record = Record::FromWord(wRecord);	
	if (record == INVALID_POINTER) { // transient -> create byneed
	  for (u_int i = regs->GetLength(); i--; ) {
	    UniqueString *label = UniqueString::FromWordDirect(labels->Sub(i));
	    LazySelClosure *closure = LazySelClosure::New(wRecord, label);
	    Byneed *byneed = Byneed::New(closure->ToWord());
	    u_int dst = Store::DirectWordToInt(regs->Sub(i));
	    SETREG(dst,byneed->ToWord());
	  }
	}
	else {
	  for (u_int i = regs->GetLength(); i--; ) {
	    UniqueString *label = UniqueString::FromWordDirect(labels->Sub(i));
	    u_int dst = Store::DirectWordToInt(regs->Sub(i));
	    SETREG(dst, record->PolySel(label));
	  }
	}
      }
      DISPATCH(PC);


      // specialized instructions
      // add instruction to select several fields at once

      /*****************************
       * exceptions
       ****************************/

    Case(install_handler) // handler
       {
	GET_1I(codeBuffer,PC,handlerAddr);
	Scheduler::PushHandler(Store::IntToWord(handlerAddr)); 
       }
       DISPATCH(PC);      

    Case(remove_handler)
       {
	Scheduler::PopHandler();
       }
       DISPATCH(PC);
    
    Case(raise) // reg
       {
	GET_1R(codeBuffer,PC,reg);
	word val = GETREG(reg);
	if(Store::WordToTransient(val) != INVALID_POINTER)
	  REQUEST(val);
	Scheduler::SetCurrentData(val);
	Scheduler::SetCurrentBacktrace(Backtrace::New());
	SAVEPC(PC);
	return Worker::RAISE;
       }
       DISPATCH(PC);      
      
     Case(reraise) // reg
       {
	GET_1R(codeBuffer,PC,reg);
	Tuple *package = Tuple::FromWordDirect(GETREG(reg));
	Scheduler::SetCurrentData(package->Sel(0));
	Scheduler::SetCurrentBacktrace
	  (Backtrace::FromWordDirect(package->Sel(1)));
	SAVEPC(PC);
	return Worker::RERAISE;
       }
       DISPATCH(PC);      


      /***************************************
       * some inlined primitives
       ***************************************/

     Case(inlined_future_byneed) // r0, r1
       {
	 GET_2R(codeBuffer,PC,r0,r1);
	 SETREG(r0, Byneed::New(GETREG(r1))->ToWord());
       }
       DISPATCH(PC);

     Case(inlined_hole_hole) // reg
       {
	 GET_1R(codeBuffer,PC,reg);
	 SETREG(reg, Hole::New()->ToWord());
       }
       DISPATCH(PC);

     Case(inlined_hole_fill) // r0, r1
       {
	 GET_2R(codeBuffer,PC,r0,r1);
	 Transient *transient = Store::WordToTransient(GETREG(r0));
	 if (transient == INVALID_POINTER 
	     || transient->GetLabel() != HOLE_LABEL)
	   RAISE(PrimitiveTable::Hole_Hole);
	 Hole *hole = reinterpret_cast<Hole *>(transient);
	 if (!hole->Fill(GETREG(r1))) {
	   RAISE(PrimitiveTable::Future_Cyclic);
	 }
       }
       DISPATCH(PC);

     Case(inlined_equal) // r0, r1, r2
       {
	 GET_3R(codeBuffer, PC, dst, a, b);
	 int res = Alice::Compare(GETREG(a), GETREG(b));
	 if (res < 0) { // Compare called Scheduler::SetCurrentData(..)
	   SAVEPC(startPC);
	   Scheduler::SetNArgs(0);
	   return Worker::REQUEST;
	 }
	 SETREG(dst, Store::IntToWord(res));
       }
       DISPATCH(PC);
     
#define INLINED_ROW_SUB(type, checked) {	\
  GET_3R(codeBuffer, PC, dst, a, b);		\
  word wA = GETREG(a);				\
  type *row = type::FromWord(wA);		\
  if (row == INVALID_POINTER) {			\
    REQUEST(wA);				\
  }						\
  word wB = GETREG(b);				\
  s_int index = Store::WordToInt(wB);		\
  if (index == INVALID_INT) {			\
    REQUEST(wB);				\
  }						\
  if (checked && (index < 0 || index >= row->GetLength())) {	\
    RAISE(PrimitiveTable::General_Subscript);	\
  }						\
  SETREG(dst, row->Sub(index));	\
  DISPATCH(PC);					\
}
     Case(inlined_array_sub)   INLINED_ROW_SUB(Array, true);
     Case(inlined_array_usub)  INLINED_ROW_SUB(Array, false);
     Case(inlined_vector_sub)  INLINED_ROW_SUB(Vector, true);
     Case(inlined_vector_usub) INLINED_ROW_SUB(Vector, false);

#define INLINED_ROW_LENGTH(type) {	\
  GET_2R(codeBuffer, PC, dst, a);	\
  word wA = GETREG(a);			\
  type *row = type::FromWord(wA);	\
  if (row == INVALID_POINTER) {		\
    REQUEST(wA);			\
  }					\
  SETREG(dst, Store::IntToWord(row->GetLength()));	\
  DISPATCH(PC);				\
}
     Case(inlined_array_length)  INLINED_ROW_LENGTH(Array);
     Case(inlined_vector_length) INLINED_ROW_LENGTH(Vector);
       
      /****************************************
       * debug support
       ***************************************/

    Case(debug_msg)
      {
	GET_1I(codeBuffer,PC,iaddr);
	String *s = String::FromWordDirect(IP->Sel(iaddr));
	fprintf(stderr,"VM DEBUG: %s\n", s->ExportC());
      }
      DISPATCH(PC);

      /****************************************
       * dummy instruction to prevent dead code
       * elimination for raiseoverflow label
       ***************************************/
    Case(dummy_raiseoverflow)
      {
	goto raiseoverflow;
      }
      DISPATCH(PC);

#ifndef THREADED
    default:
      {
	fprintf(stderr, "BCI: instr number %"U_INTF" unknown\n", instr);
	return Worker::CONTINUE;
      }
    }
  }
#endif
 raiseoverflow:
  __asm__ __volatile__ ("asm_raiseoverflow:");
  RAISE(PrimitiveTable::General_Overflow);
}


void ByteCodeInterpreter::PushCall(Closure *closure) { 
  BCI_DEBUG("BCI::PushCall(%p) current frame=%p --> ",
	    closure->ToWord(),Scheduler::GetFrame());
  Assert(ConcreteCode::FromWord(closure->GetConcreteCode())->GetInterpreter() == this);
  ByteCodeFrame *frame = ByteCodeFrame::New(closure);
  BCI_DEBUG(" new frame=%p\n",frame);  
}

void ByteCodeInterpreter::PushCall(ByteConcreteCode *bcc, Closure *closure) {
  BCI_DEBUG("BCI::PushCall(%p) current frame=%p --> ",
	    closure->ToWord(),Scheduler::GetFrame());
  ByteCodeFrame *frame = ByteCodeFrame::New(bcc, closure);
  BCI_DEBUG(" new frame=%p\n",frame);
}

ByteCodeFrame *ByteCodeInterpreter::DupFrame(ByteCodeFrame *bcFrame) {
  // isn't working :-(
  u_int size = bcFrame->GetSize();
  StackFrame *frame = reinterpret_cast<StackFrame*>(frame);
  word wFrame = frame->Clone();
  StackFrame *newFrame = Scheduler::PushFrame(size);
  StackFrame::New(newFrame,size,wFrame);
  return reinterpret_cast<ByteCodeFrame*>(newFrame);
}

void ByteCodeInterpreter::DumpFrame(StackFrame *sFrame, std::ostream& out) {
  Assert(sFrame->GetWorker() == this);
  ByteCodeFrame *frame = reinterpret_cast<ByteCodeFrame *>(sFrame);
  ByteConcreteCode *cc = frame->GetConcreteCode();
  ByteCodeSourceLocations::PrintFrame(cc->GetAbstractCode()->Sel(0), cc->GetSourceLocations(), frame->GetPC(), out);
}

#if PROFILE

word ByteCodeInterpreter::GetProfileKey(StackFrame *sFrame) {
  return reinterpret_cast<ByteCodeFrame *>(sFrame)->GetConcreteCode()->ToWord();
}

String *ByteCodeInterpreter::GetProfileName(StackFrame *sFrame) {
  ByteCodeFrame *frame = reinterpret_cast<ByteCodeFrame *>(sFrame);
  return GetProfileName(
    reinterpret_cast<ConcreteCode*>(frame->GetConcreteCode()));
}

word ByteCodeInterpreter::GetProfileKey(ConcreteCode *cc) {
  return cc->ToWord();
}

String *ByteCodeInterpreter::GetProfileName(ConcreteCode *cc) {
  Assert(cc->GetInterpreter() == this);
  return AbstractCodeInterpreter::MakeProfileName(
    reinterpret_cast<ByteConcreteCode*>(cc)->GetAbstractCode());
}

#endif
