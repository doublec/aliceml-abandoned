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
#pragma implementation "alice/ByteCodeConstProp.hh"
#endif

#include "alice/AbstractCode.hh"
#include "alice/AbstractCodeInterpreter.hh"
#include "alice/ByteCodeConstProp.hh"
#include "alice/Types.hh"
#include "alice/LazySelInterpreter.hh"


namespace {

  class ControlStack {
  private:
    u_int *stack;
    u_int size;
    s_int top;
    
    void Push(u_int item) {
      if (++top >= size) {
	u_int oldSize = size;
	size = size * 3 / 2;
	u_int *newStack = new u_int[size];
	memcpy(newStack, stack, oldSize * sizeof(u_int));
	delete[] stack;
	stack = newStack;
      }
      stack[top] = item;
    }
    
    u_int Pop() {
      return stack[top--];
    }
    
  public:
    
    enum { VISIT, INLINE_EXIT, STOP };
    
    ControlStack(u_int s = 200) {
      size = s;
      top = -1;
      stack = new u_int[size];
    }
    
    ~ControlStack() {
      delete[] stack;
    }
    
    u_int PopCommand() {
      return Pop();
    }
    
    TagVal *PopInstr() {
      return reinterpret_cast<TagVal *>(Pop());
    }
      
    void PopInlineExit(u_int *localOffset, Vector **subst, Vector **inlineReturnIdDefs, IntMap **sharedInArity, InlineInfo **inlineInfo, ConstPropInfo **constPropInfo) {
      *constPropInfo = reinterpret_cast<ConstPropInfo*>(Pop());
      *inlineInfo = reinterpret_cast<InlineInfo*>(Pop());
      *sharedInArity = reinterpret_cast<IntMap*>(Pop());
      *inlineReturnIdDefs = reinterpret_cast<Vector*>(Pop());
      *subst = reinterpret_cast<Vector*>(Pop());
      *localOffset = Pop();
    }
    
    void PushInstr(TagVal *instr) {
      Push(reinterpret_cast<u_int>(instr));
      Push(VISIT);
    }
    
    void PushInstr(word instr) {
      PushInstr(TagVal::FromWordDirect(instr));
    }
    
    void PushInlineExit(u_int localOffset, Vector *subst, Vector *inlineReturnIdDefs, IntMap *sharedInArity, InlineInfo *inlineInfo, ConstPropInfo *constPropInfo) {
      Push(localOffset);
      Push(reinterpret_cast<u_int>(subst));
      Push(reinterpret_cast<u_int>(inlineReturnIdDefs));
      Push(reinterpret_cast<u_int>(sharedInArity));
      Push(reinterpret_cast<u_int>(inlineInfo));
      Push(reinterpret_cast<u_int>(constPropInfo));
      Push(INLINE_EXIT);
    }
    
    void PushStop() {
      Push(STOP);
    }
    
    bool Empty() {
      return top == -1;
    }
    
    u_int GetSize() {
      return top;
    }
  };


  class ConstPropAnalyser {
  private:
    
    enum const_kind {
      KindNotYetAssigned, /** variable has not yet been assigned in the analysis */
      KindInt,            /** so far the variable appears to hold a constant int */
      KindTagVal,         /** so far the variable appears to hold a TagVal with constant tag */
      KindUnknown         /** the variable has been assigned but we dont know more than that */
    };
    
    /**
    * Holds information about local variable values. each entry is
    * either ConstNotYetAssigned(), ConstUnknown(), or a TagVal with a tag which is
    * either KindInt or KindTagVal plus a value which is the constant
    * int or tag.
    */
    Array *constants;
    
    Vector *subst;
    TagVal *abstractCode;
    u_int localOffset;
    u_int inlineDepth;
    
    /**
    * IdDef Vector to bind with the result when the current inlined
    * function returns, or INVALID_POINTER
    */
    Vector *inlineReturnIdDefs;
    
    InlineInfo *inlineInfo;
    ConstPropInfo *constPropInfo;

    
    u_int IdToId(word id) {
      return Store::DirectWordToInt(id) + localOffset;
    }
    
    
    word ConstNotYetAssigned(){
      return Store::IntToWord(KindNotYetAssigned);
    }
    
    
    word ConstUnknown() {
      return Store::IntToWord(KindUnknown);
    }
    
    
    word IntToConst(s_int i) {
      TagVal *c = TagVal::New(KindInt, 1);
      c->Init(0, Store::IntToWord(i));
      return c->ToWord();
    }
    
    
    word TagToConst(u_int tag) {
      TagVal *c = TagVal::New(KindTagVal, 1);
      c->Init(0, Store::IntToWord(tag));
      return c->ToWord();
    }
    
    
    word ValueToConst(word value) {
      value = LazySelInterpreter::Deref(value);
      
      if (PointerOp::IsInt(value)) {
	return IntToConst(Store::WordToInt(value));
      }
      
      Block *b = Store::WordToBlock(value);
      if (b != INVALID_POINTER) {
	
	if (Alice::IsTag(b->GetLabel())) {
	  TagVal *tv = reinterpret_cast<TagVal*>(b);
	  return TagToConst(tv->GetTag());
	}
      
	if (b->GetLabel() == Alice::BIG_TAG) {
	  BigTagVal *btv = reinterpret_cast<BigTagVal*>(b);
	  return TagToConst(btv->GetTag());
	}
      }
      
      return ConstUnknown();
    }
    
    
    word IdRefToConst(word wIdRef) {
      TagVal *idRef = TagVal::FromWordDirect(wIdRef);

      switch(AbstractCode::GetIdRef(idRef)) {
	case AbstractCode::LastUseLocal:
	case AbstractCode::Local: {
	  return constants->Sub(IdToId(idRef->Sel(0)));
	}
	case AbstractCode::Global: {
	  u_int index = Store::DirectWordToInt(idRef->Sel(0));
	  TagVal *valOpt = TagVal::FromWord(subst->Sub(index));
	  if(valOpt != INVALID_POINTER) {
	    return ValueToConst(valOpt->Sel(0));
	  }
	  else {
	    return ConstUnknown();
	  }
	}
	case AbstractCode::Immediate: {
	  return ValueToConst(idRef->Sel(0));
	}
      }
    }
    
    
    void AssignConst(word cons, u_int dest) {
      word cur = constants->Sub(dest);
      
      if (cur == ConstUnknown() || cons == ConstNotYetAssigned()) {
	return;
      }
      if (cur == ConstNotYetAssigned()) {
	constants->Update(dest, cons);
	return;
      }
      if (cons == ConstUnknown()) {
	constants->Update(dest, ConstUnknown());
	return;
      }
      
      TagVal *tvCur  = TagVal::FromWordDirect(cur);
      TagVal *tvCons = TagVal::FromWordDirect(cons);
      
      if (tvCons->GetTag() != tvCur->GetTag() || tvCons->Sel(0) != tvCur->Sel(0)) {
	constants->Update(dest, ConstUnknown());
      }
    }
    
    
    void AssignValue(word value, u_int dest) {
      AssignConst(ValueToConst(value), dest);
    }
    
    
    void AssignFromLocal(u_int src, u_int dest) {
      AssignConst(constants->Sub(src), dest);
    }
    
    
    void AssignFromIdRef(word idRef, u_int dest) {
      AssignConst(IdRefToConst(idRef), dest);
    }
    
    
    void AssignIdDefUnknown(word wIdDef) {
      TagVal *idDef = TagVal::FromWord(wIdDef);
      if (idDef != INVALID_POINTER) {
	AssignConst(ConstUnknown(), IdToId(idDef->Sel(0)));
      }
    }
    
	
    void AssignIdDefsUnknown(Vector *idDefs) {
      for (u_int i=idDefs->GetLength(); i--; ) {
	AssignIdDefUnknown(idDefs->Sub(i));
      }
    }
    
    
    void AssignIdsUnknown(Vector *ids) {
      for (u_int i=ids->GetLength(); i--; ) {
	AssignConst(ConstUnknown(), IdToId(ids->Sub(i)));
      }
    }


    Vector *ShiftIdDefs(Vector *srcs, s_int offset) {
      u_int size = srcs->GetLength();
      Vector *dsts = Vector::New(size);
      for(u_int i = size; i--; ) {
	TagVal *argOpt = TagVal::FromWord(srcs->Sub(i));
	if(argOpt != INVALID_POINTER) {
	  u_int id = Store::DirectWordToInt(argOpt->Sel(0)) + offset;
	  TagVal *newOpt = TagVal::New(Types::SOME, 1);
	  newOpt->Init(0, Store::IntToWord(id));
	  dsts->Init(i, newOpt->ToWord());
	} else {
	  dsts->Init(i, srcs->Sub(i));
	}
      }
      return dsts;
    }
    
    
    void NewTagTestInfo(TagVal *instr, word continuation, Vector *idDefs) {
      Tuple *info = Tuple::New(2);
      info->Init(0, (idDefs == INVALID_POINTER ? Vector::New(0) : idDefs)->ToWord());
      info->Init(1, continuation);
      constPropInfo->GetTagTestInfo()->Put(instr->ToWord(), info->ToWord());
    }
    
    
    void InlineCCC(Vector *srcIdRefs, Vector *destIdDefs);
    
  public:
    ConstPropAnalyser(TagVal *abstractCode, InlineInfo *inlineInfo) {
      this->abstractCode = abstractCode;
      this->inlineInfo = inlineInfo;
      localOffset = 0;
      inlineDepth = 0;
      subst = Vector::FromWordDirect(abstractCode->Sel(1));
      constants = Array::New(inlineInfo->GetNLocals(), ConstNotYetAssigned());
      inlineReturnIdDefs = INVALID_POINTER;
      constPropInfo = ConstPropInfo::New();
    }
    void Run();
    ConstPropInfo *GetConstPropInfo() { return constPropInfo; }
  };


  void ConstPropAnalyser::InlineCCC(Vector *srcIdRefs, Vector *destIdDefs) {
    if(destIdDefs == INVALID_POINTER)
      return;
    
    u_int nSrc = srcIdRefs->GetLength();
    u_int nDest = destIdDefs->GetLength();
    
    if(nSrc == nDest) {
      for(u_int i = nSrc; i--; ) {
	TagVal *idDef = TagVal::FromWord(destIdDefs->Sub(i));
	if(idDef != INVALID_POINTER) {
	  u_int dest = IdToId(idDef->Sel(0));
	  AssignFromIdRef(srcIdRefs->Sub(i), dest);
	}
      }
    }
    else {
      AssignIdDefsUnknown(destIdDefs);
    }
  }


  void ConstPropAnalyser::Run() {
    
    IntMap *sharedInArity = AbstractCode::SharedInArity(abstractCode);
    ControlStack stack;
    
    stack.PushStop();
    AssignIdDefsUnknown(Vector::FromWordDirect(abstractCode->Sel(3)));
    stack.PushInstr(TagVal::FromWordDirect(abstractCode->Sel(5)));
    while (true) {
      switch(stack.PopCommand()) {
      case ControlStack::STOP: {
	return;
      }
      case ControlStack::INLINE_EXIT: {
	stack.PopInlineExit(&localOffset, &subst, &inlineReturnIdDefs, &sharedInArity, &inlineInfo, &constPropInfo);
	inlineDepth--;
	break;
      }
      case ControlStack::VISIT: {
	TagVal *instr = stack.PopInstr();
	AbstractCode::instr instrOp = AbstractCode::GetInstr(instr);
	switch (instrOp) {
	  case AbstractCode::Raise:
	  case AbstractCode::Reraise:
	    break;
	  case AbstractCode::Return: {
	    // exit from an inlined callee
	    if(inlineDepth > 0) {
	      Vector *returnIdRefs = Vector::FromWordDirect(instr->Sel(0));
	      InlineCCC(returnIdRefs, inlineReturnIdDefs);
	    }
	    break;
	  }
	  case AbstractCode::Entry:
	  case AbstractCode::Exit:
	  case AbstractCode::EndHandle:
	  case AbstractCode::EndTry:
	  case AbstractCode::Kill: {
	    u_int cp = AbstractCode::GetContinuationPos(instrOp);
	    stack.PushInstr(instr->Sel(cp)); 
	    break;
	  }
	  case AbstractCode::GetRef:
	  case AbstractCode::PutNew:
	  case AbstractCode::PutRef:
	  case AbstractCode::PutTup:
	  case AbstractCode::PutVec:
	  case AbstractCode::Close:
	  case AbstractCode::Specialize:
	  case AbstractCode::PutCon:
	  case AbstractCode::PutPolyRec:
	  case AbstractCode::Sel: {
	    AssignConst(ConstUnknown(), IdToId(instr->Sel(0)));
	    u_int cp = AbstractCode::GetContinuationPos(instrOp);
	    stack.PushInstr(instr->Sel(cp));
	    break;
	  }
	  case AbstractCode::GetTup: {
	    AssignIdDefsUnknown(Vector::FromWordDirect(instr->Sel(0)));
	    stack.PushInstr(instr->Sel(2));
	    break;
	  }
	  case AbstractCode::LazyPolySel: {
	    AssignIdsUnknown(Vector::FromWordDirect(instr->Sel(0)));
	    stack.PushInstr(instr->Sel(3));
	    break;
	  }
	  case AbstractCode::PutVar: {
	    AssignFromIdRef(instr->Sel(1), IdToId(instr->Sel(0)));
	    stack.PushInstr(instr->Sel(2));
	    break;
	  }
	  case AbstractCode::PutTag: {
	    u_int tag = Store::DirectWordToInt(instr->Sel(2));
	    AssignConst(TagToConst(tag), IdToId(instr->Sel(0)));
	    stack.PushInstr(TagVal::FromWordDirect(instr->Sel(4))); 
	    break;
	  }
	  case AbstractCode::AppPrim: {
	    TagVal *contOpt = TagVal::FromWord(instr->Sel(2));
	    if(contOpt != INVALID_POINTER) {
	      Tuple *cont = Tuple::FromWordDirect(contOpt->Sel(0));
	      AssignIdDefUnknown(cont->Sel(0));
	      stack.PushInstr(cont->Sel(1));
	    }
	    else if (inlineReturnIdDefs != INVALID_POINTER) {
	      AssignIdDefsUnknown(inlineReturnIdDefs);
	    }
	    break;
	  }
	  case AbstractCode::AppVar: {
	    word inlineItem = inlineInfo->GetInlineMap()->CondGet(instr->ToWord());
	    if (inlineItem != INVALID_POINTER) {
	      // function is being inlined
	      
	      Tuple *funInfo = Tuple::FromWordDirect(inlineItem);
	      TagVal *abstractCode = TagVal::FromWordDirect(funInfo->Sel(0));
	      
	      TagVal *contOpt = TagVal::FromWord(instr->Sel(3));
	      if(contOpt != INVALID_POINTER) {
		Tuple *cont = Tuple::FromWordDirect(contOpt->Sel(0));
		stack.PushInstr(cont->Sel(1));
	      }
	      
	    // String *name = AbstractCodeInterpreter::MakeProfileName(abstractCode);
//	      fprintf(stderr,"inline at depth %d: ???\n", inlineDepth);
  // 	    AbstractCode::Disassemble(stderr, TagVal::FromWordDirect(abstractCode->Sel(5)));
	      
	      stack.PushInlineExit(localOffset, subst, inlineReturnIdDefs, sharedInArity, inlineInfo, constPropInfo);
	      stack.PushInstr(TagVal::FromWordDirect(abstractCode->Sel(5)));
	      u_int offset = Store::DirectWordToInt(funInfo->Sel(2));
	      
	      Vector *argsIdRefs = Vector::FromWordDirect(instr->Sel(1));
	      Vector *argsIdDefs = ShiftIdDefs(Vector::FromWordDirect(abstractCode->Sel(3)), offset);
	      InlineCCC(argsIdRefs, argsIdDefs);
	      localOffset += offset;
	      subst = Vector::FromWordDirect(funInfo->Sel(1)); // TODO: use the closure for the subst? (does it sometimes have more specific data)?
	      inlineDepth++;
	      inlineInfo = InlineInfo::FromWordDirect(funInfo->Sel(3));
	      Map *inlineMap = constPropInfo->GetInlineMap();
	      constPropInfo = ConstPropInfo::New();
	      inlineMap->Put(instr->ToWord(), constPropInfo->ToWord()); //TODO: could minimization make this kind of map unsound?
	      sharedInArity = AbstractCode::SharedInArity(abstractCode); // TODO: cache shared-in-arities (on the CC?) and clone as needed (possibly move function to AbstractCode class?)
	      
	      if (contOpt != INVALID_POINTER) {
		Tuple *cont = Tuple::FromWordDirect(contOpt->Sel(0));
		inlineReturnIdDefs = ShiftIdDefs(Vector::FromWordDirect(cont->Sel(0)), -offset);
	      }
	      else if (inlineReturnIdDefs != INVALID_POINTER) {
		inlineReturnIdDefs = ShiftIdDefs(inlineReturnIdDefs, -offset);
	      }
	    }
	    else {
	      // function is not being inlined
	      TagVal *contOpt = TagVal::FromWord(instr->Sel(3));
	      if(contOpt != INVALID_POINTER) {
		Tuple *cont = Tuple::FromWordDirect(contOpt->Sel(0));
		AssignIdDefsUnknown(Vector::FromWordDirect(cont->Sel(0)));
		stack.PushInstr(cont->Sel(1));
	      }
	      else if (inlineReturnIdDefs != INVALID_POINTER) {
		AssignIdDefsUnknown(inlineReturnIdDefs);
	      }
	    }
	    break;
	  }
	  case AbstractCode::Try: {
	    stack.PushInstr(instr->Sel(0));
	    AssignIdDefUnknown(instr->Sel(1));
	    AssignIdDefUnknown(instr->Sel(2));
	    stack.PushInstr(instr->Sel(3));
	    break;
	  }
	  case AbstractCode::IntTest:
	  case AbstractCode::RealTest:
	  case AbstractCode::StringTest: {
	    Vector *tests = Vector::FromWordDirect(instr->Sel(1));
	    for (u_int i=tests->GetLength(); i--; ) {
	      Tuple *test = Tuple::FromWordDirect(tests->Sub(i));
	      stack.PushInstr(test->Sel(1));
	    }
	    stack.PushInstr(instr->Sel(2));
	    break;
	  }
	  case AbstractCode::VecTest: {
	    Vector *tests = Vector::FromWordDirect(instr->Sel(1));
	    for (u_int i=tests->GetLength(); i--; ) {
	      Tuple *test = Tuple::FromWordDirect(tests->Sub(i));
	      AssignIdDefsUnknown(Vector::FromWordDirect(test->Sel(0)));
	      stack.PushInstr(test->Sel(1));
	    }
	    stack.PushInstr(instr->Sel(2));
	    break;
	  }
	  case AbstractCode::CompactIntTest: {
	    Vector *tests = Vector::FromWordDirect(instr->Sel(2));
	    for (u_int i=tests->GetLength(); i--; ) {
	      stack.PushInstr(tests->Sub(i));
	    }
	    stack.PushInstr(instr->Sel(3));
	    break;
	  }
	  case AbstractCode::ConTest: {
	    Vector *tests0 = Vector::FromWordDirect(instr->Sel(1));
	    for(u_int i=tests0->GetLength(); i--; ) {
	      Tuple *test = Tuple::FromWordDirect(tests0->Sub(i));
	      stack.PushInstr(test->Sel(1));
	    }
	    Vector *testsN = Vector::FromWordDirect(instr->Sel(2));
	    for(u_int i=testsN->GetLength(); i--; ) {
	      Tuple *test = Tuple::FromWordDirect(testsN->Sub(i));
	      AssignIdDefsUnknown(Vector::FromWordDirect(test->Sel(1)));
	      stack.PushInstr(test->Sel(2));
	    }
	    stack.PushInstr(instr->Sel(3));
	    break;
	  }
	  case AbstractCode::TagTest: {
	    word cons = IdRefToConst(instr->Sel(0));
	    Vector *tests0 = Vector::FromWordDirect(instr->Sel(2));
	    Vector *testsN = Vector::FromWordDirect(instr->Sel(3));
	    word els = instr->Sel(4);
	    
	    if (cons == ConstUnknown() || cons == ConstNotYetAssigned()) {
	      
	      for (u_int i=tests0->GetLength(); i--; ) {
		Tuple *test = Tuple::FromWordDirect(tests0->Sub(i));
		stack.PushInstr(test->Sel(1));
		// TODO: if tested value is a local, record its tagval inside this branch
	      }
	      for (u_int i=testsN->GetLength(); i--; ) {
		Tuple *test = Tuple::FromWordDirect(testsN->Sub(i));
		AssignIdDefsUnknown(Vector::FromWordDirect(test->Sel(1)));
		stack.PushInstr(test->Sel(2));
		// TODO: if tested value is a local, record its tagval inside this branch
	      }
	      stack.PushInstr(els);
	      
	    }
	    else {
	      
	      TagVal *consTv = TagVal::FromWordDirect(cons);
	      const_kind kind = static_cast<const_kind>(consTv->GetTag());
	      word tag = consTv->Sel(0);
	      word cont = INVALID_POINTER;
	      Vector *idDefs = INVALID_POINTER;
	      
	      if (kind == KindInt) {
		for (u_int i=tests0->GetLength(); i--; ) {
		  Tuple *test = Tuple::FromWordDirect(tests0->Sub(i));
		  if (test->Sel(0) == tag) {
		    cont = test->Sel(1);
		    break;
		  }
		}
	      }
	      else { // kind == KindTagVal
		for (u_int i=testsN->GetLength(); i--; ) {
		  Tuple *test = Tuple::FromWordDirect(testsN->Sub(i));
		  if (test->Sel(0) == tag) {
		    idDefs = Vector::FromWordDirect(test->Sel(1));
		    AssignIdDefsUnknown(idDefs);
		    cont = test->Sel(2);
		    break;
		  }
		}
	      }
	      if(cont == INVALID_POINTER) {
		cont = els;
	      }
	      stack.PushInstr(cont);
	      
	      NewTagTestInfo(instr, cont, idDefs);
	    }
	    break;
	  }
	  case AbstractCode::CompactTagTest: {
	    word cons = IdRefToConst(instr->Sel(0));
	    Vector *tests = Vector::FromWordDirect(instr->Sel(2));
	    TagVal *elseOpt = TagVal::FromWord(instr->Sel(3));
	    
	    if (cons == ConstUnknown() || cons == ConstNotYetAssigned()) {
	      
	      for (u_int i=tests->GetLength(); i--; ) {
		Tuple *test = Tuple::FromWordDirect(tests->Sub(i));
		TagVal *idDefsOpt = TagVal::FromWord(test->Sel(0));
		if (idDefsOpt != INVALID_POINTER) {
		  AssignIdDefsUnknown(Vector::FromWordDirect(idDefsOpt->Sel(0)));
		}
		// TODO: if tested value is a local, record its tagval inside this branch
		stack.PushInstr(test->Sel(1));
	      }
	      if (elseOpt != INVALID_POINTER) {
		stack.PushInstr(elseOpt->Sel(0));
	      }
	    
	    }
	    else {
	      
	      TagVal *consTv = TagVal::FromWordDirect(cons);
	      u_int tag = Store::DirectWordToInt(consTv->Sel(0));
	      word cont = INVALID_POINTER;
	      Vector *idDefs = INVALID_POINTER;
	      
	      if (tag < tests->GetLength()) {
		Tuple *test = Tuple::FromWordDirect(tests->Sub(tag));
		TagVal *idDefsOpt = TagVal::FromWord(test->Sel(0));
		if (idDefsOpt != INVALID_POINTER) {
		  Assert(consTv->GetTag() == KindTagVal);
		  idDefs = Vector::FromWordDirect(idDefsOpt->Sel(0));
		  AssignIdDefsUnknown(idDefs);
		}
		else{
		  Assert(consTv->GetTag() == KindInt);
		}
		cont = test->Sel(1);
	      }
	      else {
		cont = elseOpt->Sel(0);
	      }
	      stack.PushInstr(cont);
	      
	      NewTagTestInfo(instr, cont, idDefs);
	    }
	    break;
	  }
	  case AbstractCode::Shared: {
	    word stamp = instr->Sel(0);
	    u_int inArity = Store::DirectWordToInt(sharedInArity->Get(stamp));
	    sharedInArity->Put(stamp, Store::IntToWord(--inArity));
	    if (inArity == 0) {
	      stack.PushInstr(instr->Sel(1));
	    }
	    break;
	  }
	  default: {
	    Assert(false);
	  }
	}
	break;
      }
      default: {
	Assert(false);
      }
      }
    }
  }

}

ConstPropInfo *ByteCodeConstProp::Analyse(TagVal *abstractCode, InlineInfo *inlineInfo) {
   static u_int c = 0;
   Tuple *coord = Tuple::FromWordDirect(abstractCode->Sel(0));
   /*
   std::fprintf(stderr, "%"U_INTF". do constant propagation for %p %s:%"S_INTF".%"S_INTF"\n",
 	       ++c,
 	       abstractCode,
 	       String::FromWordDirect(coord->Sel(0))->ExportC(),
 	       Store::DirectWordToInt(coord->Sel(1)),
 	       Store::DirectWordToInt(coord->Sel(2))); 
   */
//  AbstractCode::Disassemble(stderr, TagVal::FromWordDirect(abstractCode->Sel(5)));
  ConstPropAnalyser mainPass(abstractCode, inlineInfo);
  mainPass.Run();
  return mainPass.GetConstPropInfo();
}
