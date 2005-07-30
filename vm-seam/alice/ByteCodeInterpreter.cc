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
#endif

#include "alice/ByteCodeInterpreter.hh"
#include "alice/Data.hh"
#include "alice/Types.hh"
#include "alice/LazySelInterpreter.hh"
#include "alice/AbstractCode.hh"
#include "alice/AliceLanguageLayer.hh"
#include "alice/ByteCode.hh"
#include "alice/ByteCodeAlign.hh"
#include "alice/ByteConcreteCode.hh"
#include "alice/ByteCodeFrame.hh"

using namespace ByteCodeInstr;

#ifdef THREADED // is defined in ByteCodeAlign.hh

#define Case(INSTR) INSTR##LBL

#define DISPATCH(PC)				\
  DEBUG_INSTR();				\
  startPC = PC;					\
  GET_INSTR(codeBuffer,PC,instr);		\
  goto *((void*) instr)
  
#define INSTR(instr) && instr##LBL,

#else // THREADED

#define Case(INSTR) case INSTR

#define DISPATCH(PC) break

#define INSTR(instr) instr,

#endif // THREADED

#undef REQUEST							
#undef RAISE

// TODO: add backtrace
#define RAISE(w) {						\
  Scheduler::SetCurrentData(w);					\
  Scheduler::PopFrame();					\
  return Worker::RAISE;						\
}

#define REQUEST(x)							\
    { PC = startPC;	/* repeat current instruction after request */	\
      frame->SaveState(PC,CP,IP);					\
      Scheduler::SetCurrentData(x);					\
      Scheduler::SetNArgs(0);						\
      BCI_DEBUG("request for %p needed\n",x);				\
      return Worker::REQUEST; }
							  
#define REQUEST_WORD(Type,val,w)		\
  Type *val = Type::FromWord(w);		\
  if (val == INVALID_POINTER) REQUEST(w)

#define REQUEST_INT(val,w)			\
  s_int val = Store::WordToInt(w);		\
  if (val == INVALID_INT) REQUEST(w)

#define CHECK_PREEMPT() 			\
  if (StatusWord::GetStatus() != 0) {		\
    frame->SaveState(PC,CP,IP);			\
    BCI_DEBUG("suspension required\n");		\
    return Worker::PREEMPT;			\
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
  return STATIC_CAST(ByteConcreteCode *, cr)->GetAbstractRepresentation();
}

u_int ByteCodeInterpreter::GetFrameSize(StackFrame *sFrame) {
  ByteCodeFrame *frame = STATIC_CAST(ByteCodeFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  return frame->GetSize();
}

u_int ByteCodeInterpreter::GetInArity(ConcreteCode *concreteCode) {
  ByteConcreteCode *bcc = STATIC_CAST(ByteConcreteCode *, concreteCode);
  return bcc->GetInArity();
}

u_int ByteCodeInterpreter::GetOutArity(ConcreteCode *concreteCode) {
  ByteConcreteCode *bcc = STATIC_CAST(ByteConcreteCode *, concreteCode);
  s_int outArity = bcc->GetOutArity();
  return (outArity == -1) ? INVALID_INT : outArity;
}

const char *ByteCodeInterpreter::Identify() {
  return "ByteCodeInterpreter";
}

Worker::Result ByteCodeInterpreter::Handle(word data) {
  BCI_DEBUG("ByteCodeInterpreter::Handle started\n");
  StackFrame *sFrame = Scheduler::GetFrame();
  ByteCodeFrame *frame = STATIC_CAST(ByteCodeFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  word exn = Scheduler::GetCurrentData();
  Tuple *package = Tuple::New(2);
  package->Init(0, exn);
  package->Init(1, Scheduler::GetCurrentBacktrace()->ToWord());
  u_int ipIndex = Store::DirectWordToInt(data);
  word handler = frame->GetIP()->Sel(ipIndex);
  Tuple *handlerData = Tuple::FromWordDirect(handler);
  u_int r0 = Store::DirectWordToInt(handlerData->Sel(0));
  u_int r1 = Store::DirectWordToInt(handlerData->Sel(1));
  u_int handlerPC = Store::DirectWordToInt(handlerData->Sel(2));
  SETREG(r0, package->ToWord());
  SETREG(r1, exn);
  frame->SavePC(handlerPC);  
  return Worker::CONTINUE;
}

/*********************************
 * interpreter main loop
 *********************************/

Worker::Result ByteCodeInterpreter::Run(StackFrame *sFrame) {

#ifdef THREADED
  if(sFrame == NULL) {
    static void* instrTable[] = {
      #include "alice/ByteCodeInstrs.hh"
      NULL
    };
    ByteCode::RegisterInstrTable(instrTable);
    return Worker::CONTINUE; // return value doesn't matter
  }
#endif

  ByteCodeFrame *frame = STATIC_CAST(ByteCodeFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  u_int instr;
  u_int startPC;   // PC is implicitely incremented by the GET_* macros
  Chunk *code;
  ReadBuffer *codeBuffer;

  u_int PC;
  Tuple *IP;
  Closure *CP;

  frame->GetState(&PC,&CP,&IP);
  code = frame->GetCode();
  codeBuffer = ReadBuffer::New(code);

  BCI_DEBUG("BCI::Run: start execution at PC %d\n",PC);
#ifdef THREADED	
  DISPATCH(PC);

#else
  while(true) {
    DEBUG_INSTR();
    startPC = PC; // remember start position for jumps
    GET_INSTR(codeBuffer,PC,instr);
    switch(instr) {
#endif
   
      /*********************************
       * calling convention conversion
       *********************************/

    Case(ccc1):
      {
	ByteCodeInterpreter::Construct();       
	SETREG(0, Scheduler::GetCurrentArg(0));
      }
      DISPATCH(PC);

    Case(cccn): // nArgs
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

    Case(seam_ccc1): // reg
      {
	GET_1R(codeBuffer,PC,reg);
	ByteCodeInterpreter::Construct();       
	SETREG(reg,Scheduler::GetCurrentArg(0));
      }
      DISPATCH(PC);

    Case(seam_ccc1_wildcard):
      {
	ByteCodeInterpreter::Construct();       
      }
      DISPATCH(PC);

    Case(seam_cccn): // nArgs
      {
	GET_1I(codeBuffer,PC,nArgs);
	if(ByteCodeInterpreter::Deconstruct())
	  return Worker::REQUEST;
	
	Assert(Scheduler::GetNArgs() == nArgs);
      }
      DISPATCH(PC);


      /**********************************************
       * macro definition for function calls/returns
       **********************************************/

#define SET_SCHEDULER_ARGS1(r0)			\
	Scheduler::SetNArgs(1);			\
	Scheduler::SetCurrentArg(0,GETREG(r0));
#define SET_SCHEDULER_ARGS2(r0,r1)		\
	Scheduler::SetNArgs(2);			\
	Scheduler::SetCurrentArg(0,GETREG(r0));	\
	Scheduler::SetCurrentArg(1,GETREG(r1));
#define SET_SCHEDULER_ARGS3(r0,r1,r2)		\
	Scheduler::SetNArgs(3);			\
	Scheduler::SetCurrentArg(0,GETREG(r0));	\
	Scheduler::SetCurrentArg(1,GETREG(r1));	\
	Scheduler::SetCurrentArg(2,GETREG(r2));
#define SET_SCHEDULER_ARGS4(r0,r1,r2,r3)	\
	Scheduler::SetNArgs(4);			\
	Scheduler::SetCurrentArg(0,GETREG(r0));	\
	Scheduler::SetCurrentArg(1,GETREG(r1));	\
	Scheduler::SetCurrentArg(2,GETREG(r2));	\
	Scheduler::SetCurrentArg(3,GETREG(r3));

#define PREPARE_CALL() 				\
      GET_1R1I(codeBuffer,PC,reg,nArgs)
#define PREPARE_CALL0()				\
      GET_1R(codeBuffer,PC,reg);		\
      Scheduler::SetNArgs(0)
#define PREPARE_CALL1()				\
      GET_2R(codeBuffer,PC,reg,r0);		\
      SET_SCHEDULER_ARGS1(r0)
#define PREPARE_CALL2()				\
      GET_3R(codeBuffer,PC,reg,r0,r1);		\
      SET_SCHEDULER_ARGS2(r0,r1)
#define PREPARE_CALL3()				\
      GET_4R(codeBuffer,PC,reg,r0,r1,r2);	\
      SET_SCHEDULER_ARGS3(r0,r1,r2)
      

      /**************************
       * normal function call
       *************************/

    // specialized calls

#define SAVE_ARGS() /* dummy */
#define SAVE_ARGS1() word arg0 = GETREG(r0)
#define SAVE_ARGS2() SAVE_ARGS1(); word arg1 = GETREG(r1)
#define SAVE_ARGS3() SAVE_ARGS2(); word arg2 = GETREG(r2)

#define SET_ARGS() /* dummy */
#define SET_ARGS1() SETREG(0,arg0)
#define SET_ARGS2() SET_ARGS1(); SETREG(1,arg1)
#define SET_ARGS3() SET_ARGS2(); SETREG(2,arg2)

#ifdef DO_REWRITING
#define REWRITE_SEAM_CALL(NOA)
#else
#define REWRITE_SEAM_CALL(NOA) 	/* dummy */
#endif

#define BCI_CALL(NOA) {							\
        PREPARE_CALL##NOA();						\
	frame->SaveState(PC,CP,IP);					\
	ByteCodeInterpreter::PushCall(Closure::FromWord(GETREG(reg)));	\
	if(StatusWord::GetStatus()) return Worker::PREEMPT;		\
	frame = (ByteCodeFrame *) Scheduler::GetFrame();		\
	frame->GetState(&PC,&CP,&IP);					\
	code = frame->GetCode();					\
	codeBuffer = ReadBuffer::New(code);				\
        DISPATCH(PC);							\
      }

    Case(bci_call):  BCI_CALL();
    Case(bci_call0): BCI_CALL(0);
    Case(bci_call1): BCI_CALL(1);
    Case(bci_call2): BCI_CALL(2);
    Case(bci_call3): BCI_CALL(3);

    Case(bci_call_direct0): Error("bci_call_direct0 not yet implemented\n");
    Case(bci_call_direct1): Error("bci_call_direct1 not yet implemented\n");
    Case(bci_call_direct2): Error("bci_call_direct2 not yet implemented\n");
    Case(bci_call_direct3): Error("bci_call_direct3 not yet implemented\n");

      // normal calls

#define SEAM_CALL(NOA) {						\
        PREPARE_CALL##NOA();						\
	frame->SaveState(PC,CP,IP);					\
	word wClosure = GETREG(reg);					\
	Closure *closure = Closure::FromWord(wClosure);			\
        REWRITE_SEAM_CALL(NOA);						\
	/* dynamic self call test */					\
	if(closure !=INVALID_POINTER) {					\
	  ConcreteCode *cc =						\
	    ConcreteCode::FromWord(closure->GetConcreteCode());		\
	  if(cc != INVALID_POINTER) {					\
	    Interpreter *interpreter = cc->GetInterpreter();		\
	    if(interpreter == this) {					\
	      PushCall(closure);					\
	      if(StatusWord::GetStatus()) return Worker::PREEMPT;	\
	      frame = (ByteCodeFrame *) Scheduler::GetFrame();		\
	      frame->GetState(&PC,&CP,&IP);				\
	      code = frame->GetCode();					\
	      codeBuffer = ReadBuffer::New(code);			\
	      DISPATCH(PC);						\
	    }								\
	  } 								\
	}								\
	/* preemption test happens in Scheduler::PushCall */		\
      	return Scheduler::PushCall(wClosure);				\
    }

    Case(seam_call):  SEAM_CALL();
    Case(seam_call0): SEAM_CALL(0);
    Case(seam_call1): SEAM_CALL(1);
    Case(seam_call2): SEAM_CALL(2);
    Case(seam_call3): SEAM_CALL(3);


    /***************************
     * function tailcall
     ***************************/

    // specialized self tailcall instructions
    // rewriting has to be cleaned up

#define PREPARE_DIRECTCALL() /* dummy */
#define PREPARE_DIRECTCALL1() SETREG(0,GETREG(r0))
#define PREPARE_DIRECTCALL2() { PREPARE_DIRECTCALL1(); SETREG(1,GETREG(r1)); }
#define PREPARE_DIRECTCALL3() { PREPARE_DIRECTCALL2(); SETREG(2,GETREG(r2)); }

#ifdef DO_REWRITING
#define REWRITE_SEAM_TAILCALL(NOA)    					
#else
#define REWRITE_SEAM_TAILCALL(NOA) /* dummy */
#endif 

    // skip CCC
      Case(self_tailcall_direct0): 
	Error("self_tailcall_direct0 not yet implemented\n");
      Case(self_tailcall_direct1): 
	Error("self_tailcall_direct1 not yet implemented\n");
      Case(self_tailcall_direct2): 
	Error("self_tailcall_direct1 not yet implemented\n");
      Case(self_tailcall_direct3): 
	Error("self_tailcall_direct1 not yet implemented\n");

#define SELF_TAILCALL(NOA) {			\
      PREPARE_CALL##NOA();			\
      PC = 0;					\
      CHECK_PREEMPT();				\
      DISPATCH(PC);				\
}
      
      Case(self_tailcall):  SELF_TAILCALL();
      Case(self_tailcall0): SELF_TAILCALL(0);
      Case(self_tailcall1): SELF_TAILCALL(1);
      Case(self_tailcall2): SELF_TAILCALL(2);
      Case(self_tailcall3): SELF_TAILCALL(3);

    // bci tailcalls

      Case(bci_tailcall):  Error("bci_tailcall not yet implemented");
      Case(bci_tailcall0): Error("bci_tailcall0 not yet implemented");
      Case(bci_tailcall1): Error("bci_tailcall1 not yet implemented");
      Case(bci_tailcall2): Error("bci_tailcall2 not yet implemented");
      Case(bci_tailcall3): Error("bci_tailcall3 not yet implemented");

      Case(bci_tailcall_direct0): 
	Error("bci_tailcall_direct0 not yet implemented");
      Case(bci_tailcall_direct1): 
	Error("bci_tailcall_direct1 not yet implemented");
      Case(bci_tailcall_direct2): 
	Error("bci_tailcall_direct2 not yet implemented");
      Case(bci_tailcall_direct3): 
	Error("bci_tailcall_direct3 not yet implemented");


    // normal tailcalls

#define SEAM_TAILCALL(NOA) {						\
      PREPARE_CALL##NOA();						\
      word wClosure = GETREG(reg);					\
      Closure *closure = Closure::FromWord(wClosure);			\
      REWRITE_SEAM_TAILCALL(NOA);					\
      Scheduler::PopFrame(frame->GetSize());				\
      /* dynamic self call test */					\
      if(closure !=INVALID_POINTER) {					\
	  ConcreteCode *cc =						\
	    ConcreteCode::FromWord(closure->GetConcreteCode());		\
	  if(cc != INVALID_POINTER) {					\
	    Interpreter *interpreter = cc->GetInterpreter();		\
	    if(interpreter == this) {					\
	      PushCall(closure);					\
	      if(StatusWord::GetStatus()) return Worker::PREEMPT;	\
	      frame = (ByteCodeFrame *) Scheduler::GetFrame();		\
	      frame->GetState(&PC,&CP,&IP);				\
	      code = frame->GetCode();					\
	      codeBuffer = ReadBuffer::New(code);			\
	      DISPATCH(PC);						\
	    }								\
	  }								\
	}								\
      /* preemption check happens in Scheduler::PushCall */		\
      return  Scheduler::PushCall(wClosure);				\
    }

    Case(seam_tailcall):  SEAM_TAILCALL();
    Case(seam_tailcall0): SEAM_TAILCALL(0);
    Case(seam_tailcall1): SEAM_TAILCALL(1);
    Case(seam_tailcall2): SEAM_TAILCALL(2);
    Case(seam_tailcall3): SEAM_TAILCALL(3);


    /****************************************
     * macro definitions for primitve calls
     ***************************************/

#define PREPARE_PRIMCALL()			\
      GET_2I(codeBuffer,PC,nArgs,primAddr)
#define PREPARE_PRIMCALL0()			\
      u_int nArgs = 0;				\
      GET_1I(codeBuffer,PC,primAddr);		\
      Scheduler::SetNArgs(0)
#define PREPARE_PRIMCALL1()			\
      u_int nArgs = 1;				\
      GET_1R1I(codeBuffer,PC,r0,primAddr);	\
      SET_SCHEDULER_ARGS1(r0)
#define PREPARE_PRIMCALL2()			\
      u_int nArgs = 2;				\
      GET_2R1I(codeBuffer,PC,r0,r1,primAddr);	\
      SET_SCHEDULER_ARGS2(r0,r1)

    /**************************
     * normal primitive call
     *************************/

#define SEAM_CALL_PRIM(NOA) {							\
      PREPARE_PRIMCALL##NOA();							\
      frame->SaveState(PC,CP,IP);						\
      Interpreter *interpreter = STATIC_CAST(Interpreter*,primAddr);		\
      /*Worker::Result res = Primitive::Execute(interpreter);*/			\
      NEW_STACK_FRAME(primFrame, interpreter, 0);				\
      Worker::Result res = interpreter->GetCFunction()();			\
      StackFrame *newFrame = Scheduler::GetFrame();				\
      /* test if we can skip the scheduler */					\
      if(res == CONTINUE && !StatusWord::GetStatus()				\
         && newFrame->GetWorker() == this) {					\
	frame = (ByteCodeFrame*) newFrame;					\
	frame->GetState(&PC,&CP,&IP);						\
	code = frame->GetCode();						\
	codeBuffer = ReadBuffer::New(code);					\
	DISPATCH(PC);								\
      }										\
      return res;								\
    }									

      
    Case(seam_call_prim):  SEAM_CALL_PRIM();
    Case(seam_call_prim0): SEAM_CALL_PRIM(0);
    Case(seam_call_prim1): SEAM_CALL_PRIM(1);
    Case(seam_call_prim2): SEAM_CALL_PRIM(2);


    /*******************************
     * tail primitive call
     *******************************/

#define SEAM_TAILCALL_PRIM(NOA) {					\
      PREPARE_PRIMCALL##NOA();						\
      Scheduler::PopFrame(frame->GetSize());				\
      Interpreter *interpreter = STATIC_CAST(Interpreter*,primAddr);	\
      /*Worker::Result res = Primitive::Execute(interpreter);*/		\
      NEW_STACK_FRAME(primFrame, interpreter, 0);			\
      Worker::Result res = interpreter->GetCFunction()();		\
      StackFrame *newFrame = Scheduler::GetFrame();			\
      /* test if we can skip the scheduler */				\
      if(res == CONTINUE && !StatusWord::GetStatus()			\
         && newFrame->GetWorker() == this) {				\
	  frame = (ByteCodeFrame*) newFrame;				\
	  frame->GetState(&PC,&CP,&IP);					\
	  code = frame->GetCode();					\
	  codeBuffer = ReadBuffer::New(code);				\
	  DISPATCH(PC);							\
      }									\
      return res;							\
    }
    
    Case(seam_tailcall_prim):   SEAM_TAILCALL_PRIM();
    Case(seam_tailcall_prim0):  SEAM_TAILCALL_PRIM(0);
    Case(seam_tailcall_prim1):  SEAM_TAILCALL_PRIM(1);
    Case(seam_tailcall_prim2):  SEAM_TAILCALL_PRIM(2);


    /*****************************
     * return from function call
     *****************************/

#define PREPARE_RETURN()			\
      GET_1I(codeBuffer,PC,nArgs);		\
      Scheduler::SetNArgs(nArgs)
#define PREPARE_RETURN1()			\
    GET_1R(codeBuffer,PC,r0);			\
    SET_SCHEDULER_ARGS1(r0)
#define PREPARE_RETURN2()			\
    GET_2R(codeBuffer,PC,r0,r1);		\
    SET_SCHEDULER_ARGS2(r0,r1)
#define PREPARE_RETURN3()			\
    GET_3R(codeBuffer,PC,r0,r1,r2);		\
    SET_SCHEDULER_ARGS3(r0,r1,r2)
#define PREPARE_RETURN4()			\
    GET_4R(codeBuffer,PC,r0,r1,r2,r3);		\
    SET_SCHEDULER_ARGS4(r0,r1,r2,r3)

#define SEAM_RETURN(NOA) {						\
	PREPARE_RETURN##NOA();						\
	Scheduler::PopFrame(frame->GetSize());				\
	StackFrame *newFrame = Scheduler::GetFrame();			\
	/* test if we can skip the scheduler */				\
	if(newFrame->GetWorker() == this) {				\
	  frame = (ByteCodeFrame*) newFrame;				\
	  frame->GetState(&PC,&CP,&IP);					\
	  code = frame->GetCode();					\
	  codeBuffer = ReadBuffer::New(code);				\
	  DISPATCH(PC);							\
	}								\
	/*preempt check only in call instructions*/			\
        return Worker::CONTINUE;					\
      }

    Case(seam_return):  SEAM_RETURN();
    Case(seam_return1): SEAM_RETURN(1);
    Case(seam_return2): SEAM_RETURN(2);
    Case(seam_return3): SEAM_RETURN(3);
    Case(seam_return4): SEAM_RETURN(4);


      /**************************************
       * closure building and initialization 
       **************************************/

    Case(mk_closure): // reg, code, size
      {
	GET_1R2I(codeBuffer,PC,reg,codeAddr,size);
	Closure *closure = Closure::New(IP->Sel(codeAddr),size);
	SETREG(reg, closure->ToWord());
      }
      DISPATCH(PC);

    Case(init_closure): // r0, r1, index
      {
	GET_2R1I(codeBuffer,PC,r0,r1,index);
	Closure *closure = Closure::FromWord(GETREG(r0));
	closure->Init(index,GETREG(r1));
      }
      DISPATCH(PC);

    Case(spec_closure): Error("spec_closure not yet implemented");

    /********************************
     * handling of external requests
     *******************************/

    Case(await): // reg
      {
	GET_1R(codeBuffer,PC,reg);
	word requestWord = GETREG(reg);
	Transient *transient = Store::WordToTransient(requestWord);
	if( transient != INVALID_POINTER) { 
	  frame->SaveState(PC,CP,IP);				       
	  Scheduler::SetCurrentData(requestWord);
       	  Scheduler::SetNArgs(0);					
	  return Worker::REQUEST; 
	}
      }
      DISPATCH(PC);
  

    /***************************************
     * basic jumps
     **************************************/

    Case(jump): // target
      {
	GET_1I(codeBuffer,PC,jumpTarget);
	PC = jumpTarget;
      }
      DISPATCH(PC);

    Case(loop): // reg, exitTarget
      {
	GET_1R1I(codeBuffer,PC,reg,exitTarget);
	int counter = Store::DirectWordToInt(GETREG(reg));
	if(counter <= 0)
	  PC = exitTarget;
	else 
	  SETREG(reg, Store::IntToWord(counter-1));
      }
      DISPATCH(PC);


      /**********************************
       * integer instructions
       **********************************/

    Case(itest): // r, map
      {
	GET_1R1I(codeBuffer,PC,reg,iaddr);
	IntMap *map = IntMap::FromWordDirect(IP->Sel(iaddr));
	word testVal = GETREG(reg);
	REQUEST_INT(dummy,testVal);
	// this is needed due to possible pointer chains
	testVal = Store::IntToWord(dummy); 
	if(map->IsMember(testVal)) 
	  PC = Store::DirectWordToInt(map->Get(testVal));
      }
      DISPATCH(PC);

    Case(citest): // reg, size, offset
      {
	GET_1R2I(codeBuffer,PC,reg,size,offset);
	REQUEST_INT(relJump,GETREG(reg));
	int index = relJump - offset; 
	if(index < size) {
	  u_int addr = PC + index;
	  GET_1I(codeBuffer,addr,target);
	  PC = target;
	} else {
	  PC += size; // jump over the jump table
	}
      }
      DISPATCH(PC);

    Case(ijump_eq): // r, val, target
      {
	GET_1R2I(codeBuffer,PC,reg,number,jumpTarget);
	REQUEST_INT(cmpNumber,GETREG(reg));
	if(cmpNumber == number) 
	  PC = jumpTarget;
      }
      DISPATCH(PC);

    Case(isub): // r0, r1, r2
      {
	GET_3R(codeBuffer,PC,r0,r1,r2);		
	REQUEST_INT(x,GETREG(r1));
	REQUEST_INT(y,GETREG(r2));
	s_int diff = x-y;
	if (diff < MIN_VALID_INT || diff > MAX_VALID_INT) {
	  RAISE(PrimitiveTable::General_Overflow);
	}	  
	else
	  SETREG(r0, Store::IntToWord(diff));
      }
      DISPATCH(PC);
	
    Case(iadd): // r0, r1, r2
      {
	GET_3R(codeBuffer,PC,r0,r1,r2);
	REQUEST_INT(x,GETREG(r1));
	REQUEST_INT(y,GETREG(r2));
	s_int sum = x + y;
	if (sum < MIN_VALID_INT || sum > MAX_VALID_INT) 
	  RAISE(PrimitiveTable::General_Overflow) 	
	else
	  SETREG(r0, Store::IntToWord(sum));
      }
      DISPATCH(PC);


      /**********************************
       * real instructions
       **********************************/

    Case(rtest): // r, map
      {
	GET_1R1I(codeBuffer,PC,reg,iaddr);
	ChunkMap *map = ChunkMap::FromWordDirect(IP->Sel(iaddr));
	word testVal = GETREG(reg);
	REQUEST_WORD(Real,dummy,testVal);
	testVal = dummy->ToWord();
	if(map->IsMember(testVal)) 
	  PC = Store::DirectWordToInt(map->Get(testVal));
      }
      DISPATCH(PC);

    Case(rjump_eq): // r, val, target
      {
	GET_1R2I(codeBuffer,PC,reg,iaddr,jumpOffset);
	REQUEST_WORD(Real,cmpValue,GETREG(reg));
	Real *real = Real::FromWordDirect(IP->Sel(iaddr));
	if(cmpValue->GetValue() == real->GetValue()) 
	  PC = (int)jumpOffset;
      }
      DISPATCH(PC);


      /**********************************
       * string instructions
       *********************************/

    Case(stest): // r, map
      {
	GET_1R1I(codeBuffer,PC,reg,iaddr);
	ChunkMap *map = ChunkMap::FromWordDirect(IP->Sel(iaddr));
	word testVal = GETREG(reg);
	REQUEST_WORD(String,dummy,testVal);
	testVal = dummy->ToWord();
	if(map->IsMember(testVal)) 
	  PC = Store::DirectWordToInt(map->Get(testVal));
      }
      DISPATCH(PC);

    Case(sjump_eq): // r, val, target
      {
	GET_1R2I(codeBuffer,PC,reg,iaddr,jumpOffset);
	REQUEST_WORD(String,s2,GETREG(reg));
	String *s1 = String::FromWordDirect(IP->Sel(iaddr));
	u_int s1Size = s1->GetSize();
	if (s1Size == s2->GetSize() &&
	      !std::memcmp(s1->GetValue(), s2->GetValue(), s1Size)) 
	  PC = (int)jumpOffset;
      }
      DISPATCH(PC);


      /******************************************
       * basic register loads
       *****************************************/

    Case(seam_load_sreg): // r, index
      {
	GET_1R1I(codeBuffer,PC,reg,index);
	SETREG(reg, Scheduler::GetCurrentArg(index));
      }
      DISPATCH(PC);

    Case(load_global): // r, i
      {
	GET_1R1I(codeBuffer,PC,reg,index);
	SETREG(reg, CP->Sub(index));
      }
      DISPATCH(PC);

    Case(load_local): // r, i
      {
	GET_1R1I(codeBuffer,PC,reg,index);
	SETREG(reg, frame->GetLocal(index));
      }
      DISPATCH(PC);

    Case(load_immediate): // r, i
      {
	GET_1R1I(codeBuffer,PC,reg,iaddr);
	SETREG(reg, IP->Sel(iaddr));
      }
      DISPATCH(PC);

    Case(load_int): // r, int
      {
	GET_1R1I(codeBuffer,PC,reg,number);
	SETREG(reg, Store::IntToWord((int)number)); 
      }
      DISPATCH(PC);

    Case(load_zero): // r
      {
	GET_1R(codeBuffer,PC,reg);
	SETREG(reg, Store::IntToWord(0));
      }
      DISPATCH(PC);

    Case(load_reg): // r0, r1
      {
	GET_2R(codeBuffer,PC,r0,r1);
	SETREG(r0, GETREG(r1));
      }
      DISPATCH(PC);

      /*******************************
       * basic register sets (stores)
       *******************************/

    // this is equal to load_reg if registers == locals
    Case(set_local): // reg, addr
      {
	GET_1R1I(codeBuffer,PC,reg,addr);
	frame->SetLocal(addr,GETREG(reg));
      }
      DISPATCH(PC);

    Case(set_global): // reg, addr
      {
	GET_1R1I(codeBuffer,PC,reg,addr);
	CP->Init(addr,GETREG(reg)); // or Update ???
      }
      DISPATCH(PC);

    Case(seam_set_nargs): // nargs
      {
	GET_1I(codeBuffer,PC,nargs);
	Scheduler::SetNArgs(nargs);
      }
      DISPATCH(PC);

    Case(seam_set_sreg): // reg, index
      {
	GET_1R1I(codeBuffer,PC,reg,index);
	Scheduler::SetCurrentArg(index,GETREG(reg));
      }
      DISPATCH(PC);


      /****************************
       * reference cells
       ***************************/

    Case(load_cell): // r0, r1
      {
	GET_2R(codeBuffer,PC,r0,r1);
	REQUEST_WORD(Cell,cell,GETREG(r1));
	SETREG(r0, cell->Access());
      }  
      DISPATCH(PC);

    Case(set_cell): // r0, r1
      {
	GET_2R(codeBuffer,PC,r0,r1);
	REQUEST_WORD(Cell,cell,GETREG(r0));
	cell->Assign(GETREG(r1));
      }
      DISPATCH(PC);

    Case(new_cell): // r0, r1
      {
	GET_2R(codeBuffer,PC,r0,r1);
	SETREG(r0, Cell::New(GETREG(r1))->ToWord());
      }
      DISPATCH(PC);


      /*****************************
       * vector instructions
       *****************************/

    Case(new_vec): // r, size
      {
	GET_1R1I(codeBuffer,PC,reg,size);
	SETREG(reg, Vector::New(size)->ToWord());
      }
      DISPATCH(PC);

    Case(init_vec): // r0, r1, index
      {
	GET_2R1I(codeBuffer,PC,r0,r1,index);
	Vector *vec = Vector::FromWord(GETREG(r0));
	vec->Init(index,GETREG(r1));
      }
      DISPATCH(PC);

    Case(load_vec): // r0, r1, index
      {
	GET_2R1I(codeBuffer,PC,r0,r1,index);
	Vector *vec = Vector::FromWord(GETREG(r1));
	SETREG(r0, vec->Sub(index));
      }
      DISPATCH(PC);

    Case(vectest): // r, map
      {
	GET_1R1I(codeBuffer,PC,reg,iaddr);
	IntMap *map = IntMap::FromWord(IP->Sel(iaddr));
	REQUEST_WORD(Vector,vec,GETREG(reg));
	word size = Store::IntToWord(vec->GetLength());
	if(map->IsMember(size)) 
	  PC = Store::DirectWordToInt(map->Get(size));
      }
      DISPATCH(PC);      


      /*****************************
       * tuple instructions
       ****************************/

      // basic instructions

    Case(new_tup): // r, size
      {
	GET_1R1I(codeBuffer,PC,reg,size);
	SETREG(reg, Tuple::New(size)->ToWord());	
      }
      DISPATCH(PC);

    Case(select_tup): // r0, r1, index
      {
	GET_2R1I(codeBuffer,PC,r0,r1,index);
	REQUEST_WORD(Tuple,tuple,GETREG(r1));
	SETREG(r0, tuple->Sel(index));
      }
      DISPATCH(PC);

    Case(init_tup): // r0, r1, index 
      {
	GET_2R1I(codeBuffer,PC,r0,r1,index);
	Tuple *tuple = Tuple::FromWord(GETREG(r0));
	tuple->Init(index,GETREG(r1));
      }
      DISPATCH(PC);

      // specialized instructions

      // TODO: add: new_tup2, new_tup3 


#define SELECT_TUP(index) {			\
	GET_2R(codeBuffer,PC,r1,r2);		\
	REQUEST_WORD(Tuple,tuple,GETREG(r2));	\
	SETREG(r1, tuple->Sel(index));		\
	DISPATCH(PC);				\
      }

    Case(select_tup0): SELECT_TUP(0);
    Case(select_tup1): SELECT_TUP(1);
    Case(select_tup2): SELECT_TUP(2);

      // TODO: add a deconstr_tup instruction

      /****************************
       * tagval instructions
       ****************************/

#define NEW_TAGVAL(TagValType) {			\
	GET_1R2I(codeBuffer,PC,reg,size,tag);		\
	TagValType *tagVal = TagValType::New(tag,size);	\
	SETREG(reg, tagVal->ToWord());			\
	DISPATCH(PC);					\
      }

    Case(new_tagval):    NEW_TAGVAL(TagVal);
    Case(new_bigtagval): NEW_TAGVAL(BigTagVal);

#define INIT_TAGVAL(TagValType) {				\
	GET_2R1I(codeBuffer,PC,r0,r1,index);			\
	TagValType *tagVal = TagValType::FromWord(GETREG(r0));	\
	tagVal->Init(index,GETREG(r1));				\
	DISPATCH(PC);						\
      }

    Case(init_tagval):    INIT_TAGVAL(TagVal);
    Case(init_bigtagval): INIT_TAGVAL(BigTagVal);

#define LOAD_TAGVAL(TagValType) {				\
	GET_2R1I(codeBuffer,PC,r0,r1,index);			\
	TagValType *tagVal = TagValType::FromWord(GETREG(r1));	\
	SETREG(r0, tagVal->Sel(index));				\
	DISPATCH(PC);						\
      }

    Case(load_tagval):    LOAD_TAGVAL(TagVal);
    Case(load_bigtagval): LOAD_TAGVAL(BigTagVal);

#define TAGTEST(TagValType) {					\
	GET_1R1I(codeBuffer,PC,reg,iaddr);			\
	IntMap *map = IntMap::FromWordDirect(IP->Sel(iaddr));	\
	word testVal = GETREG(reg);				\
	TagValType *tagVal = TagValType::FromWord(testVal);	\
	word tag;						\
	if(tagVal == INVALID_POINTER) {				\
	  REQUEST_INT(dummy,testVal);				\
	  tag = Store::IntToWord(dummy);			\
	}							\
	else {							\
	  tag = Store::IntToWord(tagVal->GetTag());		\
	}							\
	if(map->IsMember(tag))					\
	  PC = Store::DirectWordToInt(map->Get(tag));		\
	DISPATCH(PC);						\
      }

    Case(tagtest):    TAGTEST(TagVal);
    Case(bigtagtest): TAGTEST(BigTagVal);
      
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
	PC = target;						\
      DISPATCH(PC);						\
    }

    Case(tagtest1):    TAGTEST1(TagVal);
    Case(bigtagtest1): TAGTEST1(BigTagVal);

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
	u_int addr = PC + tag;					\
	GET_1I(codeBuffer,addr,target);				\
	PC = target;						\
      } else {							\
	PC += size; /* jump over the jump table */		\
      }								\
      DISPATCH(PC);						\
    }

    Case(ctagtest): CTAGTEST(TagVal);
    Case(cbigtagtest): CTAGTEST(BigTagVal);

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
      u_int addr = PC+tag;					\
      GET_1I(codeBuffer,addr,target);				\
      PC = target;						\
      DISPATCH(PC);						\
    }								
								
    Case(ctagtest_direct): CTAGTEST_DIRECT(TagVal);		
    Case(cbigtagtest_direct): CTAGTEST_DIRECT(BigTagVal);


      /*****************************
       * constructor instructions
       *****************************/

    Case(new_con): // r, name
      {
	GET_1R1I(codeBuffer,PC,r,nameAddr);
	Constructor *constructor = 
	  Constructor::New(String::FromWordDirect(IP->Sel(nameAddr)));
	SETREG(r, constructor->ToWord());
      }
      DISPATCH(PC);

    Case(prepare_con): // r0, r1, size
      {
	GET_2R1I(codeBuffer,PC,r0,r1,size);
	ConVal *conVal = ConVal::New(Store::WordToBlock(GETREG(r1)), size);
	SETREG(r0, conVal->ToWord());
      }
      DISPATCH(PC);

    Case(init_con): // r0, r1, index
      {
	GET_2R1I(codeBuffer,PC,r0,r1,index);
	ConVal *conVal = ConVal::FromWord(GETREG(r0)); // or Direct ???
	conVal->Init(index,GETREG(r1));
      }
      DISPATCH(PC);

    Case(load_con): // r0, r1, index
      {
	GET_2R1I(codeBuffer,PC,r0,r1,index);
	ConVal *conVal = ConVal::FromWord(GETREG(r1));
	SETREG(r0, conVal->Sel(index));
      }
      DISPATCH(PC);

    Case(contest): // r0, r1, target
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
	    PC = target;
      }
      DISPATCH(PC);


      // check if we can bundle new/prepare/set_con(arg) into one instruction

      /****************************
       * polymorphic records
       ***************************/

    Case(new_polyrec): // reg, labelVec
      {
	GET_1R1I(codeBuffer,PC,reg,labelsAddr);
	Vector *labels = Vector::FromWordDirect(IP->Sel(labelsAddr));
	Record *record = Record::New(labels);
	SETREG(reg, record->ToWord());
      }
      DISPATCH(PC);

    Case(init_polyrec): // r0,r1,index (according to labelVec from new_polyrec)
      {
	GET_2R1I(codeBuffer,PC,r0,r1,index);
	Record *record = Record::FromWord(GETREG(r0));
	record->Init(index,GETREG(r1));
      }
      DISPATCH(PC);

    Case(lazyselect_polyrec): // r0, r1, labelAddr
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


      // specialized instructions
      // add instruction to select several fields at once

      /*****************************
       * exceptions
       ****************************/

    Case(install_handler): // handler
       {
	GET_1I(codeBuffer,PC,handlerAddr);
	Scheduler::PushHandler(Store::IntToWord(handlerAddr)); 
       }
       DISPATCH(PC);      

    Case(remove_handler):
       {
	Scheduler::PopHandler();
       }
       DISPATCH(PC);      
      
    Case(raise_normal): // reg
       {
	GET_1R(codeBuffer,PC,reg);
	word val = GETREG(reg);
	if(Store::WordToTransient(val) != INVALID_POINTER)
	  REQUEST(val);
	Scheduler::SetCurrentData(val);
	Scheduler::SetCurrentBacktrace(Backtrace::New(frame->Clone()));
	frame->SaveState(PC,CP,IP);
	return Worker::RAISE;
       }
       DISPATCH(PC);      
      
     Case(raise_direct): // reg
       {
	GET_1R(codeBuffer,PC,reg);
	Tuple *package = Tuple::FromWordDirect(GETREG(reg));
	Scheduler::SetCurrentData(package->Sel(0));
	Scheduler::SetCurrentBacktrace
	  (Backtrace::FromWordDirect(package->Sel(1)));
	frame->SaveState(PC,CP,IP);
	return Worker::RAISE;
       }
       DISPATCH(PC);      


      /***************************************
       * some inlined primitives
       ***************************************/

     Case(inlined_future_byneed): // r0, r1
       {
	 GET_2R(codeBuffer,PC,r0,r1);
	 SETREG(r0, Byneed::New(GETREG(r1))->ToWord());
       }
       DISPATCH(PC);

     Case(inlined_hole_hole): // reg
       {
	 GET_1R(codeBuffer,PC,reg);
	 SETREG(reg, Hole::New()->ToWord());
       }
       DISPATCH(PC);

     Case(inlined_hole_fill): // r0, r1
       {
	 GET_2R(codeBuffer,PC,r0,r1);
	 Transient *transient = Store::WordToTransient(GETREG(r0));
	 if (transient == INVALID_POINTER 
	     || transient->GetLabel() != HOLE_LABEL)
	   RAISE(PrimitiveTable::Hole_Hole);
	 Hole *hole = STATIC_CAST(Hole *, transient);
	 if (!hole->Fill(GETREG(r1))) {
	   RAISE(PrimitiveTable::Future_Cyclic);
	 }
       }
       DISPATCH(PC);

      /****************************************
       * debug support
       ***************************************/

    Case(debug_msg):
      {
	GET_1I(codeBuffer,PC,iaddr);
	String *s = String::FromWordDirect(IP->Sel(iaddr));
	fprintf(stderr,"VM DEBUG: %s\n", s->ExportC());
      }
      DISPATCH(PC);

#ifndef THREADED
    default:
      {
	fprintf(stderr, "BCI: instr number %d unkown\n",instr);
	return Worker::CONTINUE;
      }
    }
  }
#endif
}


// TODO: check if this is correct
void ByteCodeInterpreter::PushCall(Closure *closure) { 
  BCI_DEBUG("BCI::PushCall(%p) current frame=%p --> ",
	    closure->ToWord(),Scheduler::GetFrame());
  ByteConcreteCode *concreteCode =
    ByteConcreteCode::FromWord(closure->GetConcreteCode());
  Assert(concreteCode->GetInterpreter() == ByteCodeInterpreter::self);
  u_int nLocals        = concreteCode->GetNLocals();
  Chunk *code          = concreteCode->GetByteCode();
  Tuple *immediateArgs = concreteCode->GetImmediateArgs();
  ByteCodeFrame * frame =
    ByteCodeFrame::New(ByteCodeInterpreter::self,
		       code,
		       0, closure, immediateArgs,
		       nLocals);
  BCI_DEBUG(" new frame=%p\n",frame);  
}

void ByteCodeInterpreter::DumpFrame(StackFrame *sFrame) {
  //  fprintf(stderr,"ByteCodeInterpreter::DumpFrame not yet implemented\n");
  ByteCodeFrame *codeFrame = STATIC_CAST(ByteCodeFrame *, sFrame);
  Assert(sFrame->GetWorker() == this);
  const char *frameType;
  frameType = "function";
  // to be done: frameType = "handler";
  // Print closure information
  Closure *closure = codeFrame->GetCP();
  ByteConcreteCode *concreteCode =
    ByteConcreteCode::FromWord(closure->GetConcreteCode());
  Transform *transform =
    STATIC_CAST(Transform *, concreteCode->GetAbstractRepresentation());
  TagVal *abstractCode = TagVal::FromWordDirect(transform->GetArgument());
  Tuple *coord         = Tuple::FromWord(abstractCode->Sel(0));
  String *name         = String::FromWord(coord->Sel(0));
  std::fprintf(stderr, //"Alice native %s %.*s, line %d, column %d\n",
	       "ByteCode %.*s:%d.%d frame %p\n",
	       /*frameType,*/ (int) name->GetSize(), name->GetValue(),
	       Store::WordToInt(coord->Sel(1)),
	       Store::WordToInt(coord->Sel(2)),
	       sFrame);
}

#if PROFILE
  // Profiling
word ByteCodeInterpreter::GetProfileKey(StackFrame *sFrame) {
  ByteCodeFrame *frame = STATIC_CAST(ByteCodeFrame *, sFrame);
  fprintf(stderr,"ByteCodeInterpreter::GetProfileKey not yet implemented\n");
  return NULL;
}

String *ByteCodeInterpreter::GetProfileName(StackFrame *sFrame) {
  ByteCodeFrame *frame = STATIC_CAST(ByteCodeFrame *, sFrame);
  fprintf(stderr,"ByteCodeInterpreter::GetProfileName not yet implemented\n");
  return NULL;
}

word ByteCodeInterpreter::GetProfileKey(ConcreteCode *concreteCode) {
  fprintf(stderr,"ByteCodeInterpreter::GetProfileKey not yet implemented\n");
  return NULL;
}

String *ByteCodeInterpreter::GetProfileName(ConcreteCode *concreteCode) {
  fprintf(stderr,"ByteCodeInterpreter::GetProfileName not yet implemented\n");
  return NULL;
}

#endif
