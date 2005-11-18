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

#ifndef __HOTSPOT_CONCRETECODE_HH__
#define __HOTSPOT_CONCRETECODE_HH__

#if defined(INTERFACE)
#pragma interface "alice/HotSpotConcreteCode.hh"
#endif

#include "Seam.hh"
#include "alice/Base.hh"
#include "alice/Data.hh"

class AliceDll HotSpotConcreteCode : private ConcreteCode {
protected:
  using ConcreteCode::Replace;

  enum { STATE, CODE, COUNTER, SIZE };

public:
  using Block::ToWord;
  using ConcreteCode::GetInterpreter;

  static word New(TagVal *abstractCode);

  u_int GetCounter() { return Store::DirectWordToInt(Get(COUNTER)); }
  u_int GetState() { return Store::DirectWordToInt(Get(STATE)); }
  word GetCode() { return Get(CODE); }

  void DecCounter() {
    u_int counter = GetCounter();
    Replace(COUNTER, Store::IntToWord(--counter));
  }
  void SetState(u_int state) {
    Replace(STATE, Store::IntToWord(state));
  }
  void SetCode(word code) { Replace(CODE, code); }
  

 // HotSpotConcreteCode Untagging
  static HotSpotConcreteCode *FromWord(word code) {
    ConcreteCode *concreteCode = ConcreteCode::FromWord(code);
    Assert(concreteCode == INVALID_POINTER ||
	   concreteCode->GetInterpreter() == HotSpotInterpreter::self);
    return STATIC_CAST(HotSpotConcreteCode *, concreteCode);
  }
  static HotSpotConcreteCode *FromWordDirect(word code) {
    ConcreteCode *concreteCode = ConcreteCode::FromWordDirect(code);
    Assert(concreteCode->GetInterpreter() == HotSpotInterpreter::self);
    return STATIC_CAST(HotSpotConcreteCode *, concreteCode);
  }
};

class AliceDll HotSpotInterpreter : public Interpreter {
private:
  HotSpotInterpreter() : Interpreter() {}
public:
  static HotSpotInterpreter *self;
  
  static void Init();

  virtual u_int GetFrameSize(StackFrame *sFrame);
  virtual Worker::Result Run(StackFrame *sFrame);
  virtual u_int GetInArity(ConcreteCode *concreteCode);
  virtual u_int GetOutArity(ConcreteCode *concreteCode);
  virtual void PushCall(Closure *closure);
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);
};

#endif
