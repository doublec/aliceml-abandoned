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

#include "alice/Base.hh"
#include "alice/Data.hh"
#include "alice/AliceLanguageLayer.hh"
#include "alice/ByteCodeInliner.hh"

/*
 * The idea is that HotSpotConcrete (HSC) is a wrapper around the real concrete
 * code. The handlers, e.g. HotSpotInterpreter, implement the transisitions
 * from one code into another. In every transition the current HSC is converted
 * in-place into another concrete code. As soon as we are in the final state, 
 * there is no indirection anymore.
 * ATTENTION: Since the HSC is converted in-place, you have to allocate 
 * enough space. So the size of the HSC in the start state must be the maximum
 * size over all states.
 */

//
// abstract states of hot spot code
//

class AliceDll HotSpotCode : protected ConcreteCode {
protected:
  enum { TRANSFORM, INLINE_INFO, CODE, COUNTER, SIZE };
  
public:
  using ConcreteCode::ToWord;
  using ConcreteCode::GetInterpreter;

  Transform *GetAbstractRepresentation() {
    return Transform::FromWordDirect(Get(TRANSFORM));    
  }

  TagVal *GetAbstractCode(){
    return TagVal::FromWordDirect(GetAbstractRepresentation()->GetArgument());
  }
  
  u_int GetCounter() { return Store::DirectWordToInt(Get(COUNTER)); }
  word GetCode() { return Get(CODE); }
  TagVal *GetInlineInfoOpt() { return TagVal::FromWord(Get(INLINE_INFO));}

  void DecCounter() {
    u_int counter = GetCounter();
    Replace(COUNTER, Store::IntToWord(--counter));
  }
  void SetCode(word code) { Replace(CODE, code); }  
  void SetInlineInfo(InlineInfo *info) {
    TagVal *some = TagVal::New(Types::SOME, 1);
    some->Init(0, info->ToWord());
    Replace(INLINE_INFO, some->ToWord());
  }
};


class AliceDll HotSpot_State : public HotSpotCode {
public:  
  // replaces the interpreter, code and counter
  static void Convert(HotSpotCode *hsc,
		      Interpreter *interpreter,
		      concrete_constructor construct,
		      int counter) {
    Error("not yet implemented");
  }
};

class AliceDll HotSpot_StartState : public HotSpotCode {
public:
  static word New(Interpreter *interpreter, 
		  TagVal *abstractCode,
		  int size, 
		  concrete_constructor construct,
		  int counter) {
    Assert(size >= HotSpotCode::SIZE);
    // reserve enough space for final state
    ConcreteCode *concreteCode = ConcreteCode::New(interpreter, size);
    // create transform
    Chunk *name =
      Store::DirectWordToChunk(AliceLanguageLayer::TransformNames::function);
    Transform *transform = Transform::New(name, abstractCode->ToWord());
    concreteCode->Init(TRANSFORM, transform->ToWord());
    // construct code
    concreteCode->Init(CODE, construct(abstractCode));
    concreteCode->Init(COUNTER, Store::IntToWord(counter));
    concreteCode->Init(INLINE_INFO, Store::IntToWord(Types::NONE));
    // set all other arguments to 0
    word zero = Store::IntToWord(0);
    for(u_int i=SIZE; i<size; i++)
      concreteCode->Init(i, zero);
    return concreteCode->ToWord();
  }
};

//
// the real hot spot concrete code
//

class AliceDll HotSpotInterpreter : public Interpreter {
private:
  HotSpotInterpreter() : Interpreter() {}
public:
  static HotSpotInterpreter *self;
  
  static void Init();

  virtual Transform *GetAbstractRepresentation(ConcreteRepresentation *b);

  virtual u_int GetFrameSize(StackFrame *sFrame);
  virtual Worker::Result Run(StackFrame *sFrame);
  virtual u_int GetInArity(ConcreteCode *concreteCode);
  virtual u_int GetOutArity(ConcreteCode *concreteCode);
  virtual void PushCall(Closure *closure);
  virtual const char *Identify();
  virtual void DumpFrame(StackFrame *sFrame);

#if PROFILE
  virtual word GetProfileKey(StackFrame *frame);
  virtual String *GetProfileName(StackFrame *frame);
  virtual word GetProfileKey(ConcreteCode *concreteCode);
  virtual String *GetProfileName(ConcreteCode *concreteCode);
#endif
  
  void Request();
};

class AliceDll HotSpotConcreteCode : public HotSpot_StartState {
public:
  static word New(TagVal *abstractCode);

  // Untagging
  static HotSpotConcreteCode *FromWord(word code) {
    ConcreteCode *concreteCode = ConcreteCode::FromWord(code);
    Assert(concreteCode == INVALID_POINTER ||
	   concreteCode->GetInterpreter() == HotSpotInterpreter::self);
    return static_cast<HotSpotConcreteCode *>(concreteCode);
  }
  static HotSpotConcreteCode *FromWordDirect(word code) {
    ConcreteCode *concreteCode = ConcreteCode::FromWordDirect(code);
    Assert(concreteCode->GetInterpreter() == HotSpotInterpreter::self);
    return static_cast<HotSpotConcreteCode *>(concreteCode);
  }
};

#endif
