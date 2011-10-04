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
#pragma implementation "alice/ByteCodeInliner.hh"
#endif

#include <stack>
#include "alice/ByteCodeJitter.hh"
#include "alice/ByteCodeInliner.hh"
#include "alice/AliceConcreteCode.hh"
#include "alice/NativeConcreteCode.hh"
#include "alice/ByteConcreteCode.hh"
#include "alice/LazySelInterpreter.hh"
#include "alice/HotSpotConcreteCode.hh"

#define INLINE_LIMIT 10


namespace {

  InlineInfo *AnalyseRecursively(TagVal *abstractCode, Map* parentRootInstrs);

  class LivenessContainer {
  private:
    u_int size;
    u_int top;
    Tuple *container;
    u_int flattenedSize;
  public:
    LivenessContainer() : size(10), top(0), flattenedSize(0) {
      container = Tuple::New(size); 
    }
    void Append(word item, u_int itemSize) {
      if(top >= size) {
	u_int newSize = size * 3 / 2;
	Tuple *newContainer = Tuple::New(newSize);
	for(u_int i=size; i--; ) 
	  newContainer->Init(i,container->Sel(i));
	size = newSize;
	container = newContainer;
      }
      container->Init(top++,item);
      flattenedSize += itemSize;
    }
    word Sub(u_int i) { return container->Sel(i); }
    u_int GetLength() { return top; }
    u_int GetFlattenedLength() { return flattenedSize; }
  };

  class InlineAnalyser {
  private:
    u_int counter;
    u_int nLocals;
    Vector *subst;
    Vector *endPoints;
    TagVal *abstractCode;
    Vector *liveness;
    Map *inlineMap;
    u_int callerMaxPP;
    Map *parentRootInstrs;
    LivenessContainer livenessInfo;  
    u_int GetEndPoint(u_int id) {
      return Store::DirectWordToInt(endPoints->Sub(id));
    }
    void Append(word key, TagVal *instr, u_int appVarPP,
		TagVal *acc, Closure *closure,
		InlineInfo *inlineInfo);
    bool IsAlias(Array *aliases, 
		u_int identifier, u_int offset, u_int startPoint,
		Vector *formalArgs, Vector *args);
    Tuple *MergeLiveness();
  public:
    InlineAnalyser(TagVal *ac, Map* map)
      : abstractCode(ac), counter(0), parentRootInstrs(map) {
      subst = Vector::FromWordDirect(abstractCode->Sel(1));
      liveness = Vector::FromWordDirect(abstractCode->Sel(6));
      inlineMap = Map::New(20); 
      nLocals = AbstractCode::GetNumberOfLocals(abstractCode);
      // prepare end points lookup
      endPoints = Vector::New(nLocals);
      u_int livenessLength = liveness->GetLength();
      for(u_int i=0, j=0; i<livenessLength; i+=3) {
	u_int identifier = Store::DirectWordToInt(liveness->Sub(i));
	endPoints->Init(identifier, liveness->Sub(i+2));
      }
    }
    void SetMaxPP(u_int pp) { callerMaxPP = pp; }
    // This functions breaks an inline analysis cycle introduced by 
    // mutual recursive functions.
    bool CheckCycle(TagVal *acc) {
      return parentRootInstrs->IsMember(acc->Sel(5));
    }
    void Count(TagVal *instr);
    void AnalyseAppVar(TagVal *instr, u_int pp);
    InlineInfo *ComputeInlineInfo() {
      Assert(counter >= 0);
      Tuple *pair = MergeLiveness();
      Vector *liveness = Vector::FromWordDirect(pair->Sel(0));
      Array *aliases = Array::FromWordDirect(pair->Sel(1));
      return InlineInfo::New(inlineMap,liveness,aliases,nLocals,counter);
    }
  };

  void InlineAnalyser::Count(TagVal *instr) {
    Assert(instr != INVALID_POINTER);
    switch(AbstractCode::GetInstr(instr)) {
    case AbstractCode::Kill:
    case AbstractCode::EndHandle:
      break;
    case AbstractCode::GetTup: {
      // Only count this instruction if a real selection is 
      // preformed. Otherwise this instruction forces evaluation.
      // In this case the compiler may detect it and skip the
      // instruction.
      Vector *regs = Vector::FromWordDirect(instr->Sel(0));
      if(regs->GetLength() > 0)
	counter++;
      break;
    }
    default:
      counter++; 
    }
  }

  void InlineAnalyser::AnalyseAppVar(TagVal *instr, u_int appVarPP) {
    Assert(instr != INVALID_POINTER);
    Assert(AbstractCode::GetInstr(instr) == AbstractCode::AppVar);
    
    TagVal *idRef = TagVal::FromWordDirect(instr->Sel(0));
    word wClosure;
    // check whether function to be called is an immediate
    if(AbstractCode::GetIdRef(idRef) == AbstractCode::Global) {
      u_int index = Store::DirectWordToInt(idRef->Sel(0));
      TagVal *valueOpt = TagVal::FromWord(subst->Sub(index));
      if (valueOpt != INVALID_POINTER)
	wClosure = valueOpt->Sel(0);
      else 
	return;
    } else if (AbstractCode::GetIdRef(idRef) == AbstractCode::Immediate) {
      wClosure = idRef->Sel(0);
    } else {
      return;
    }

    // Remember a key to the first selected closure. If the actual closure
    // is hidden inside a record, we do not perform the selection in the 
    // jit compiler again.
    word key = wClosure;

    Closure *closure = Closure::FromWord(LazySelInterpreter::Deref(wClosure));
    if (closure == INVALID_POINTER) {
      return;
    }
    ConcreteCode *cc = ConcreteCode::FromWordDirect(closure->GetConcreteCode());
    Interpreter *interpreter = cc->GetInterpreter();
    
    if(interpreter == ByteCodeInterpreter::self) {
      ByteConcreteCode *bcc = ByteConcreteCode::FromWordDirect(cc->ToWord());
      TagVal *ac = bcc->GetAbstractCode();
      if(CheckCycle(ac)) return; // break inline cycle
      InlineInfo *inlineInfo = bcc->GetInlineInfo();
      u_int nNodes = inlineInfo->GetNNodes();
      if(nNodes <= INLINE_LIMIT) {
	Append(key, instr, appVarPP, ac, closure, inlineInfo);
	counter += nNodes - 1; // adjust counter: substract 1 for AppVar instr
      }
    } else if(interpreter == HotSpotInterpreter::self) {
      HotSpotConcreteCode *hscc = 
	HotSpotConcreteCode::FromWordDirect(cc->ToWord());
      TagVal *ac = hscc->GetAbstractCode();
      if(CheckCycle(ac)) return; // break inline cycle
      TagVal *inlineInfoOpt = hscc->GetInlineInfoOpt();
      InlineInfo *inlineInfo;
      if(inlineInfoOpt == INVALID_POINTER) {
	inlineInfo = AnalyseRecursively(ac, parentRootInstrs);
	hscc->SetInlineInfo(inlineInfo);
      } else {
	inlineInfo = InlineInfo::FromWordDirect(inlineInfoOpt->Sel(0));
      }
      u_int nNodes = inlineInfo->GetNNodes();
      if(nNodes <= INLINE_LIMIT) {
	Append(key, instr, appVarPP, ac, closure, inlineInfo);
	counter += nNodes - 1; // adjust counter
      }
    }
  }

  void InlineAnalyser::Append(word key, TagVal *instr,
			      u_int appVarPP,
			      TagVal *ac, Closure *closure,
			      InlineInfo *inlineInfo) {
    // there can be a strange situation
    // there can be an implicit merge point in the abstract code introduced
    // be an compacttagtest. to me it is not clear which appVarPP i have to
    // choose for the appvar instruction.
    // append liveness
    Vector *calleeLiveness = inlineInfo->GetLiveness();
    word wCalleeLiveness = calleeLiveness->ToWord();
    Tuple *tup = Tuple::New(6);
    tup->Init(0,wCalleeLiveness);
    tup->Init(1, inlineInfo->GetAliases()->ToWord());
    tup->Init(2, Store::IntToWord(nLocals));
    tup->Init(3, Store::IntToWord(appVarPP));
    tup->Init(4, ac->Sel(3)); // formal arguments
    tup->Init(5, instr->Sel(1)); // actual arguments
    livenessInfo.Append(tup->ToWord(),calleeLiveness->GetLength());
    
    // add closure to substitution, i.e. do specialize
    u_int nGlobals = closure->GetSize();
    Vector *newSubst = Vector::New(nGlobals);
    for(u_int i=nGlobals; i--; ) {
      TagVal *idRef = TagVal::New1(AbstractCode::Immediate, closure->Sub(i));
      newSubst->Init(i,idRef->ToWord());
    }
    
    AppVarInfo *avi = AppVarInfo::New(ac, newSubst, nLocals, inlineInfo, closure);
    inlineMap->Put(instr->ToWord(), avi->ToWord());
    // add number of locals
    nLocals += inlineInfo->GetNLocals();
  }

  inline bool InlineAnalyser::IsAlias(Array *aliases,
				      u_int identifier, 
				      u_int offset, 
				      u_int startPoint,
				      Vector *formalArgs, 
				      Vector *actualArgs) {
    // check if identifier is a formal arg
    // formal args are numbers from 0 .. n intermixed with wildcards
    // e.g.: _, _, 0, _, 1
    // so we have to do a lookup
    u_int nFormalArgs = formalArgs->GetLength();
    for(u_int i = identifier; i<nFormalArgs; i++) {
      TagVal *idDef = TagVal::FromWord(formalArgs->Sub(i));
      if(idDef != INVALID_POINTER
	&& Store::DirectWordToInt(idDef->Sel(0)) == identifier) {
	  TagVal *arg = TagVal::FromWordDirect(actualArgs->Sub(i));
	  switch(AbstractCode::GetIdRef(arg)) {
	  case AbstractCode::Local:
	  case AbstractCode::LastUseLocal:
	    {
	      u_int argId = Store::DirectWordToInt(arg->Sel(0));
	      u_int argEndPoint = GetEndPoint(argId);
	      // check if intervals overlap
	      if(argEndPoint > startPoint) {
		aliases->Update(identifier+offset,Store::IntToWord(argId));
		return true;
	      }
	    }
	    break;
	  default:
	    ;
	  }
      }
    }
    return false;
  }

  Tuple *InlineAnalyser::MergeLiveness() {
    u_int size = livenessInfo.GetLength();
    if(size == 0) { // nothing can be inlined
      Tuple *pair = Tuple::New(2);
      pair->Init(0, liveness->ToWord());  
      pair->Init(1, Array::New(0)->ToWord());
      return pair;
    }
    // prepare aliases
    Array *aliases = Array::New(nLocals);
    for(u_int i = nLocals; i--; )
      aliases->Init(i, Store::IntToWord(i));
    // copy intervals of the inlinable functions
    u_int offset = 0;
    u_int offsetTable[callerMaxPP];
    std::memset(offsetTable,0,callerMaxPP*sizeof(u_int));
    u_int l1Length = livenessInfo.GetFlattenedLength();
    u_int liveness1[l1Length];
    u_int index = 0;
    for(u_int i = size; i--; ) {
      Tuple *tup = Tuple::FromWordDirect(livenessInfo.Sub(i));
      Vector *calleeLiveness = Vector::FromWordDirect(tup->Sel(0));
      Array *calleeAliases = Array::FromWordDirect(tup->Sel(1));
      u_int idOffset = Store::WordToInt(tup->Sel(2));
      u_int appVarPP = callerMaxPP - Store::DirectWordToInt(tup->Sel(3)) + 1;
      Vector *formalArgs = Vector::FromWordDirect(tup->Sel(4));
      Vector *actualArgs = Vector::FromWordDirect(tup->Sel(5));
      u_int maxEndPoint = 0;
      // adjust aliases
      u_int uptoId = calleeAliases->GetLength() + idOffset;
      for(u_int i = idOffset; i<uptoId; i++) {
	u_int src = 
	  Store::DirectWordToInt(calleeAliases->Sub(i-idOffset)) + idOffset;
	aliases->Update(i, Store::IntToWord(src));
      }
      // adjust callee intervals
      // do not check for aliasing if a CCC is needed
      if(formalArgs->GetLength() != actualArgs->GetLength()) {
	for(u_int j=0; j<calleeLiveness->GetLength(); j+=3) {
	  u_int identifier = Store::DirectWordToInt(calleeLiveness->Sub(j));
	  u_int startPoint = Store::DirectWordToInt(calleeLiveness->Sub(j+1));
	  u_int endPoint = Store::DirectWordToInt(calleeLiveness->Sub(j+2));      
	  if(maxEndPoint < endPoint) 
	    maxEndPoint = endPoint;
	  liveness1[index++] = identifier + idOffset;
	  liveness1[index++] = startPoint + appVarPP + offset;
	  liveness1[index++] = endPoint + appVarPP + offset; 
	}
      } else {
	// TODO: stop alias checking if all formal args have been visited
	for(u_int j=0; j<calleeLiveness->GetLength(); j+=3) {
	  u_int identifier = Store::DirectWordToInt(calleeLiveness->Sub(j));
	  u_int newStartPoint = 
	    Store::DirectWordToInt(calleeLiveness->Sub(j+1)) + offset + appVarPP;
	  u_int endPoint = Store::DirectWordToInt(calleeLiveness->Sub(j+2));      
	  if(!IsAlias(aliases,identifier,idOffset,newStartPoint,
		      formalArgs,actualArgs)) {
	    if(maxEndPoint < endPoint) 
	      maxEndPoint = endPoint;
	    liveness1[index++] = identifier + idOffset;
	    liveness1[index++] = newStartPoint;
	    liveness1[index++] = endPoint + appVarPP + offset; 
	  }
	}    
      }
      offsetTable[appVarPP] = maxEndPoint + 1;
      offset += maxEndPoint + 1;
    }
    l1Length = index; // set it to the actual length
    // propagate offsets
    for(u_int i=1; i<callerMaxPP; i++) {
      offsetTable[i] += offsetTable[i-1];
    }
    // adjust caller intervals
    u_int l2Length = liveness->GetLength();
    u_int liveness2[l2Length];
    for(u_int i=0, j=0; i<l2Length; i+=3, j+=3) {
      u_int identifier = Store::DirectWordToInt(liveness->Sub(i));
      u_int startPoint = Store::DirectWordToInt(liveness->Sub(i+1));
      u_int endPoint = Store::DirectWordToInt(liveness->Sub(i+2));
      liveness2[j] = identifier;
      liveness2[j+1] = startPoint + offsetTable[startPoint];
      liveness2[j+2] = endPoint + offsetTable[endPoint];
    }
    // merge the adjusted the liveness arrays
    Vector *newLiveness = Vector::New(l1Length + l2Length);
    u_int i1 = 0, i2 = 0, i3 = 0;
    while( i1 < l1Length && i2 < l2Length ) {
      if(liveness1[i1+1] < liveness2[i2+1]) {
	newLiveness->Init(i3++,Store::IntToWord(liveness1[i1++]));
	newLiveness->Init(i3++,Store::IntToWord(liveness1[i1++]));
	newLiveness->Init(i3++,Store::IntToWord(liveness1[i1++]));
      } else {
	newLiveness->Init(i3++,Store::IntToWord(liveness2[i2++]));
	newLiveness->Init(i3++,Store::IntToWord(liveness2[i2++]));
	newLiveness->Init(i3++,Store::IntToWord(liveness2[i2++]));
      }
    }
    while(i1 < l1Length) {
      newLiveness->Init(i3++,Store::IntToWord(liveness1[i1++]));
      newLiveness->Init(i3++,Store::IntToWord(liveness1[i1++]));
      newLiveness->Init(i3++,Store::IntToWord(liveness1[i1++]));
    }
    while(i2 < l2Length) {
      newLiveness->Init(i3++,Store::IntToWord(liveness2[i2++]));
      newLiveness->Init(i3++,Store::IntToWord(liveness2[i2++]));
      newLiveness->Init(i3++,Store::IntToWord(liveness2[i2++]));
    }
    Tuple *pair = Tuple::New(2);
    pair->Init(0, newLiveness->ToWord());  
    pair->Init(1, aliases->ToWord());
    return pair;
  }

  // compute program points of appvar instructions

  class ControlStack {
  private:
    std::stack<u_int> stack;
    
    void Push(u_int item) {
      stack.push(item);
    }
    
    u_int Pop() {
      u_int top = stack.top();
      stack.pop();
      return top;
    }
    
  public:
    enum { VISIT, INC, ANALYSE_APPVAR,STOP };
    u_int PopInt() { return Pop(); }
    u_int PopCommand() { return Pop(); }
    TagVal *PopInstr() { return reinterpret_cast<TagVal *>(Pop()); }
    word PopWord() { return reinterpret_cast<word>(Pop()); }
    TagVal *PopTagVal() { return reinterpret_cast<TagVal *>(Pop()); }
    void PushInstr(word instr) {
      Push(reinterpret_cast<u_int>(TagVal::FromWordDirect(instr)));
      Push(VISIT);
    }
    void PushInc() { Push(1); Push(INC); }
    void PushInc(u_int i) { Push(i); Push(INC); }
    void PushAnalyseAppVar(TagVal *instr) {
      Push(reinterpret_cast<u_int>(instr));
      Push(ANALYSE_APPVAR);
    }
    void PushStop() { Push(STOP); }
    bool Empty() { return stack.empty(); }
  };


  /**
  * Walk the abstract code graph to calculate the program
  * points at each instruction, and analyse each AppVar.
  * 
  * @return The program point at the root (maximum PP).
  */
  u_int AnalyseAppVars(TagVal *rootInstr, InlineAnalyser *analyser) {
      
      // maps shared stamp => program point, for all Shared nodes yet encountered
      IntMap *stamps = IntMap::New(32);
      ControlStack stack;
      u_int programPoint = 0;
      
      stack.PushStop();
      stack.PushInstr(rootInstr->ToWord());
      while (true) {
	switch(stack.PopCommand()) {
	case ControlStack::ANALYSE_APPVAR:
	  {
	    TagVal *instr = stack.PopInstr();
	  analyser->AnalyseAppVar(instr, programPoint);
	}
	break;
      case ControlStack::STOP:
	return programPoint;
      case ControlStack::INC:
	{
	  u_int increment = stack.PopInt();
	  programPoint += increment;
	}
	break;
      case ControlStack::VISIT: 
	{
	  TagVal *instr = stack.PopInstr();
	  AbstractCode::instr instrOp = AbstractCode::GetInstr(instr);
	  analyser->Count(instr);
	  switch(instrOp) {
	  case AbstractCode::EndTry:
	  case AbstractCode::EndHandle:
	  case AbstractCode::Kill:
	  case AbstractCode::PutVar:
	  case AbstractCode::PutNew:
	  case AbstractCode::PutTag:
	  case AbstractCode::PutCon:
	  case AbstractCode::PutRef:
	  case AbstractCode::PutTup:
	  case AbstractCode::PutPolyRec:
	  case AbstractCode::PutVec:
	  case AbstractCode::Close:
	  case AbstractCode::Specialize:
	  case AbstractCode::GetRef:
	  case AbstractCode::GetTup:
	  case AbstractCode::Sel:
	  case AbstractCode::LazyPolySel: {
	    u_int pp = AbstractCode::GetNumProgramPoints(instrOp);
	    u_int cp = AbstractCode::GetContinuationPos(instrOp);
	    stack.PushInc(pp);
	    stack.PushInstr(instr->Sel(cp));
	    break;
	  }
	  case AbstractCode::AppPrim:
	    {
	      TagVal *idDefInstrOpt = TagVal::FromWord(instr->Sel(2));
	      if(idDefInstrOpt == INVALID_POINTER) {
		programPoint++;
	      } else {
		Tuple *idDefInstr = Tuple::FromWordDirect(idDefInstrOpt->Sel(0));
		stack.PushInc(2);
		stack.PushInstr(idDefInstr->Sel(1));
	      }
	    }
	    break;
	  case AbstractCode::AppVar:
	    {
	      TagVal *idDefsInstrOpt = TagVal::FromWord(instr->Sel(3));
	      if(idDefsInstrOpt == INVALID_POINTER) {
		programPoint++;
		stack.PushAnalyseAppVar(instr);
	      } else {
		Tuple *idDefsInstr = 
		  Tuple::FromWordDirect(idDefsInstrOpt->Sel(0));
		stack.PushAnalyseAppVar(instr);
		stack.PushInc(2);
		stack.PushInstr(idDefsInstr->Sel(1));
	      }
	    }
	    break;
	  case AbstractCode::Raise:
	  case AbstractCode::Reraise:
	  case AbstractCode::Return:
	    programPoint++;
	    break;
	  case AbstractCode::Try:
	    stack.PushInstr(instr->Sel(0));
	    stack.PushInc();
	    stack.PushInstr(instr->Sel(3));
	    break;
	  case AbstractCode::CompactIntTest:
	    {
	      stack.PushInc();
	      Vector *tests = Vector::FromWordDirect(instr->Sel(2)); 
	      u_int nTests = tests->GetLength();
	      for(u_int i=0; i<nTests; i++)
		stack.PushInstr(tests->Sub(i));
	      stack.PushInstr(instr->Sel(3));
	    }
	    break;
	  case AbstractCode::IntTest:
	  case AbstractCode::RealTest:
	  case AbstractCode::StringTest:
	    {
	      stack.PushInc();
	      Vector *tests = Vector::FromWordDirect(instr->Sel(1)); 
	      u_int nTests = tests->GetLength();
	      for(u_int i=0; i<nTests; i++) {
		Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
		stack.PushInstr(pair->Sel(1));
	      }
	      stack.PushInstr(instr->Sel(2));
	    }
	    break;
	  case AbstractCode::TagTest:
	    {	  
	      stack.PushInc();
	      Vector *tests0 = Vector::FromWordDirect(instr->Sel(2));
	      u_int nTests0 = tests0->GetLength(); 
	      for(u_int i=0; i<nTests0; i++) {
		Tuple *pair = Tuple::FromWordDirect(tests0->Sub(i));
		stack.PushInstr(pair->Sel(1));
	      }	  
	      Vector *testsN = Vector::FromWordDirect(instr->Sel(3));
	      u_int nTestsN = testsN->GetLength(); 
	      for(u_int i=0; i<nTestsN; i++) {
		Tuple *triple = Tuple::FromWordDirect(testsN->Sub(i));
		stack.PushInc();
		stack.PushInstr(triple->Sel(2));
	      }
	      stack.PushInstr(instr->Sel(4));
	    }
	    break;
	  case AbstractCode::CompactTagTest:
	    {
	      stack.PushInc();
	      Vector *tests = Vector::FromWordDirect(instr->Sel(2));
	      u_int nTests = tests->GetLength(); 
	      for(u_int i = 0; i<nTests; i++) {
		Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
		TagVal *idDefsOpt = TagVal::FromWord(pair->Sel(0));
		if(idDefsOpt != INVALID_POINTER) {
		  stack.PushInc();
		}
		stack.PushInstr(pair->Sel(1));
	      }
	      TagVal *elseInstrOpt = TagVal::FromWord(instr->Sel(3));
	      if(elseInstrOpt != INVALID_POINTER)
		stack.PushInstr(elseInstrOpt->Sel(0));
	    }
	    break;
	  case AbstractCode::ConTest:
	    {
	      stack.PushInc();
	      Vector *tests0 = Vector::FromWordDirect(instr->Sel(1));
	      u_int nTests0 = tests0->GetLength(); 
	      Vector *testsN = Vector::FromWordDirect(instr->Sel(2));
	      u_int nTestsN = testsN->GetLength(); 
	      for(u_int i=0; i<nTests0; i++) {
		Tuple *pair = Tuple::FromWordDirect(tests0->Sub(i));
		stack.PushInstr(pair->Sel(1));
	      }	  
	      for(u_int i=0; i<nTestsN; i++) {
		Tuple *triple = Tuple::FromWordDirect(testsN->Sub(i));
		stack.PushInc();
		stack.PushInstr(triple->Sel(2));
	      }
	      stack.PushInstr(instr->Sel(3));
	    }
	    break;
	  case AbstractCode::VecTest:
	    {
	      stack.PushInc();	  
	      Vector *tests = Vector::FromWordDirect(instr->Sel(1));
	      u_int nTests = tests->GetLength(); 
	      for(u_int i=0; i<nTests; i++) {
		Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
		Vector *idDefs = Vector::FromWordDirect(pair->Sel(0));
		if(idDefs->GetLength() > 0) {
		  stack.PushInc();
		}
		stack.PushInstr(pair->Sel(1));
	      }
	      stack.PushInstr(instr->Sel(2));
	    }
	    break;
	  case AbstractCode::Shared:
	    {
	      word stamp = instr->Sel(0);
	      if(!stamps->IsMember(stamp)) {
		stamps->Put(stamp,Store::IntToWord(programPoint));
		stack.PushInstr(instr->Sel(1));
	      }
	    }
	    break;
	  default:
	    Error("invalid abstract code tag");
	  }
	}
      }
    }
  }
  
  InlineInfo *AnalyseRecursively(TagVal *abstractCode, Map* parentRootInstrs) {
    parentRootInstrs->Put(abstractCode->Sel(5), Store::IntToWord(0));
    InlineAnalyser analyser(abstractCode, parentRootInstrs);
    TagVal *root = TagVal::FromWordDirect(abstractCode->Sel(5));
    u_int maxPP = AnalyseAppVars(root, &analyser);
    analyser.SetMaxPP(maxPP);
    parentRootInstrs->Remove(abstractCode->Sel(5));
    return analyser.ComputeInlineInfo();
  }

}


InlineInfo *ByteCodeInliner::Analyse(TagVal *abstractCode) {
//   static u_int c = 0;
//   Tuple *coord = Tuple::FromWordDirect(abstractCode->Sel(0));
//   std::fprintf(stderr, "%d. analyse inlining for %p %s:%d.%d, nLocals %d\n",
// 	       ++c,
// 	       abstractCode,
// 	       String::FromWordDirect(coord->Sel(0))->ExportC(),
// 	       Store::DirectWordToInt(coord->Sel(1)),
// 	       Store::DirectWordToInt(coord->Sel(2)),
// 	       GetNumberOfLocals(abstractCode)); 
//   AbstractCode::Disassemble(stderr,
// 			    TagVal::FromWordDirect(abstractCode->Sel(5)));
  return AnalyseRecursively(abstractCode, Map::New(16));
}
