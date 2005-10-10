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

#ifndef __ALICE_BYTE_CODE_INTERPRETER_HH__
#define __ALICE_BYTE_CODE_INTERPRETER_HH__

#if defined(INTERFACE)
#pragma inferface "alice/ByteCodeInterpreter.hh"
#endif

#include "alice/Base.hh"

#define GETREG(x) frame->GetLocal(x)
#define SETREG(x,w) frame->SetLocal(x,w)

// switch optimizing rewriting on and off
//#define DO_REWRITING
#undef DO_REWRITING

// debug output

#define BCI_DEBUG(s,...)
#define DEBUG_INSTR()

// extern u_int invocations;
// #define BCI_DEBUG(s,...) /*if(invocations > 600)*/ fprintf(stderr,s, ##__VA_ARGS__)
// #define DEBUG_INSTR() /*if(invocations > 600) */ByteCode::DisassembleOne(stderr,PC,code,IP)

class ByteCodeFrame;
class ReadBuffer;

class AliceDll ByteCodeInterpreter : public Interpreter {
private:
  ByteCodeInterpreter() : Interpreter() {}

#ifdef THREADED
  void **instrTable;
#endif

public:
  static ByteCodeInterpreter *self;
  
  static void Init();

  virtual Transform *GetAbstractRepresentation(ConcreteRepresentation *);

  virtual u_int GetFrameSize(StackFrame *sFrame);
  virtual Result Handle(word data);
  virtual Result Run(StackFrame *sFrame);
  virtual u_int GetInArity(ConcreteCode *concreteCode);
  virtual u_int GetOutArity(ConcreteCode *concreteCode);
  virtual ByteCodeFrame* DupFrame(ByteCodeFrame *bcFrame);
  virtual void PushCall(Closure *closure);
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);

#if PROFILE
  // Profiling
  virtual word GetProfileKey(ByteCodeFrame *frame);
  virtual String *GetProfileName(ByteCodeFrame *frame);
  virtual word GetProfileKey(ConcreteCode *concreteCode);
  virtual String *GetProfileName(ConcreteCode *concreteCode);
#endif

  // copy CCC from seam/generic/Worker.cc
  // this avoids the virtual call and allows inlining
  
  void Construct() {
    u_int nArgs = Scheduler::GetNArgs();
    BCI_DEBUG("Construct nArgs %d\n",nArgs);
    switch (nArgs) {
    case 0:
      Scheduler::SetNArgs(1);
      Scheduler::SetCurrentArg(0, Store::IntToWord(0));
      break;
    case 1:
      return;
    default:
      {
	Tuple *tuple = Tuple::New(nArgs);
	BCI_DEBUG("Construct build tuple %p\n",tuple);	
	for (u_int i = 0; i<nArgs; i++ ) {
// 	  Block *b = Store::WordToBlock(Scheduler::GetCurrentArg(i));
// 	  BCI_DEBUG("val %p - label %d\n",Scheduler::GetCurrentArg(i),
// 		    b == INVALID_POINTER ? -1 : b->GetLabel());
	  tuple->Init(i, Scheduler::GetCurrentArg(i));
	}
	Scheduler::SetNArgs(1);
	Scheduler::SetCurrentArg(0, tuple->ToWord());
      }
      break;
    }
  }
  
  u_int Deconstruct() {
    switch (Scheduler::GetNArgs()) {
    case 0:
      return 0;
    case 1:
      {
	word arg = Scheduler::GetCurrentArg(0);
	Transient *t = Store::WordToTransient(arg);
	if (t == INVALID_POINTER) { // is determined
	  Tuple *tuple = Tuple::FromWord(arg);
	  Assert(tuple != INVALID_POINTER);
	  Scheduler::SetNArgs
	    (Store::DirectWordToBlock(tuple->ToWord())->GetSize()); //--**
	  for (u_int i = Scheduler::GetNArgs(); i--; )
	    Scheduler::SetCurrentArg(i, tuple->Sel(i));
	  return 0;
	} else { // need to request
	  Scheduler::SetCurrentData(arg);
	  return 1;
	}
      }
    default:
      return 0;
    }
  }

};

#endif
