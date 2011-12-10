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

#include <vector>
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

  struct AppVarLivenessInfo {
    Vector *calleeLiveness;
    Vector *calleeAliases;
    Vector *calleeIdDefs;
    u_int idOffset;
    u_int appVarPP;
    Vector *appVarIdRefs;
  };
    
  class LivenessContainer {
  private:
    std::vector<AppVarLivenessInfo> items;
    u_int flattenedSize;
  public:
    LivenessContainer() : flattenedSize(0) {}
    
    void Append(AppVarLivenessInfo avli) {
      items.push_back(avli);
      flattenedSize += avli.calleeLiveness->GetLength();
    }
    
    AppVarLivenessInfo Sub(u_int i) {
      return items[i];
    }
    
    u_int GetLength() {
      return items.size();
    }
    
    u_int GetFlattenedLength() {
      return flattenedSize;
    }
  };

  class InlineAnalyser {
  private:
    u_int counter;
    u_int nLocals;
    TagVal *abstractCode;
    Vector *liveness;
    Map *inlineMap;
    u_int callerMaxPP;
    Map *parentRootInstrs;
    LivenessContainer livenessInfo;  
    
    void Append(TagVal *instr, u_int appVarPP, TagVal *acc, Closure *closure, InlineInfo *inlineInfo);
    Vector *MergeLiveness();
    Vector *MergeAliases(Vector *mergedLiveness);

  public:
    
    InlineAnalyser(TagVal *ac, Map* map)
      : abstractCode(ac), counter(0), parentRootInstrs(map) {
      liveness = Vector::FromWordDirect(abstractCode->Sel(6));
      inlineMap = Map::New(10); 
      nLocals = AbstractCode::GetNumberOfLocals(abstractCode);
    }
    
    void SetMaxPP(u_int pp) {
      callerMaxPP = pp;
    }
    
    // This functions breaks an inline analysis cycle introduced by 
    // mutually recursive functions.
    bool CheckCycle(TagVal *acc) {
      return parentRootInstrs->IsMember(acc->Sel(5));
    }
    
    void Count(TagVal *instr);
    
    void AnalyseAppVar(TagVal *instr, u_int pp);
    
    InlineInfo *ComputeInlineInfo() {
      Assert(counter >= 0);
      Vector *newLiveness = MergeLiveness();
      Vector *newAliases = MergeAliases(newLiveness);
      return InlineInfo::New(inlineMap, newLiveness, newAliases, counter);
    }
  };
  

  void InlineAnalyser::Count(TagVal *instr) {
    Assert(instr != INVALID_POINTER);
    switch(AbstractCode::GetInstr(instr)) {
    case AbstractCode::Coord:
    case AbstractCode::Entry:
    case AbstractCode::Exit:
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
    
    Closure *closure = INVALID_POINTER;
    TagVal *idRef = TagVal::FromWordDirect(instr->Sel(0));
    // check whether function to be called is an immediate
    if(AbstractCode::GetIdRef(idRef) == AbstractCode::Global) {
      Vector *subst = Vector::FromWordDirect(abstractCode->Sel(1));
      u_int index = Store::DirectWordToInt(idRef->Sel(0));
      TagVal *valueOpt = TagVal::FromWord(subst->Sub(index));
      if (valueOpt != INVALID_POINTER) {
	closure = Closure::FromWord(LazySelInterpreter::Deref(valueOpt->Sel(0)));
      }
    }
    else if (AbstractCode::GetIdRef(idRef) == AbstractCode::Immediate) {
      closure = Closure::FromWord(LazySelInterpreter::Deref(idRef->Sel(0)));
    }
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
	Append(instr, appVarPP, ac, closure, inlineInfo);
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
	Append(instr, appVarPP, ac, closure, inlineInfo);
	counter += nNodes - 1; // adjust counter: substract 1 for AppVar instr
      }
    }
  }


  void InlineAnalyser::Append(TagVal *instr, u_int appVarPP, TagVal *ac, Closure *closure, InlineInfo *inlineInfo) {
    
    // there can be a strange situation
    // there can be an implicit merge point in the abstract code introduced
    // be an compacttagtest. to me it is not clear which appVarPP i have to
    // choose for the appvar instruction.
    // append liveness
    // ^^^ WHAT DOES THIS COMMENT MEAN!? ^^^
    
    AppVarLivenessInfo avli;
    avli.calleeLiveness = inlineInfo->GetLiveness();
    avli.calleeAliases = inlineInfo->GetAliases();
    avli.calleeIdDefs = Vector::FromWordDirect(ac->Sel(3));
    avli.idOffset = nLocals;
    avli.appVarPP = appVarPP;
    avli.appVarIdRefs = Vector::FromWordDirect(instr->Sel(1));
    livenessInfo.Append(avli);
    
    // add closure to substitution, i.e. do specialize
    Vector *oldSubst = Vector::FromWordDirect(ac->Sel(1));
    Vector *newSubst = Vector::New(oldSubst->GetLength());
    for(u_int i=0, j=0; i<oldSubst->GetLength(); i++) {
      TagVal *valOpt = TagVal::FromWord(oldSubst->Sub(i));
      word val = (valOpt == INVALID_POINTER) ? closure->Sub(j++) : valOpt->Sel(0);
      TagVal *idRef = TagVal::New1(AbstractCode::Immediate, val);
      newSubst->Init(i, idRef->ToWord());
    }
    
    AppVarInfo *avi = AppVarInfo::New(ac, newSubst, nLocals, inlineInfo, closure);
    inlineMap->Put(instr->ToWord(), avi->ToWord());
    nLocals += inlineInfo->GetNLocals();
  }


  Vector *InlineAnalyser::MergeLiveness() {
    u_int size = livenessInfo.GetLength();
    
    if(size == 0) { // nothing can be inlined
     return liveness;
    }
    
    Vector *newLiveness = Vector::New(nLocals * 2);
    u_int offsetTable[callerMaxPP];
    std::memset(offsetTable, 0,  callerMaxPP*sizeof(u_int));
    
    for(u_int i=size, offset=0; i--; ) {
      AppVarLivenessInfo avli = livenessInfo.Sub(i);
      Vector *calleeLiveness  = avli.calleeLiveness;
      u_int idOffset          = avli.idOffset;
      u_int appVarPP          = callerMaxPP - avli.appVarPP + 1;
      
      u_int maxEndPoint = 0;
      // copy liveness from callee
      for(u_int j=0; j<calleeLiveness->GetLength(); j+=2) {

	u_int startPoint = Store::DirectWordToInt(calleeLiveness->Sub(j));
	u_int endPoint = Store::DirectWordToInt(calleeLiveness->Sub(j+1));
	
	u_int newStartPoint = appVarPP + offset + startPoint;
	u_int newEndPoint = appVarPP + offset + endPoint;
	
	newLiveness->Init(idOffset*2+j,   Store::IntToWord(newStartPoint));
	newLiveness->Init(idOffset*2+j+1, Store::IntToWord(newEndPoint));
	
	maxEndPoint = endPoint > maxEndPoint ? endPoint : maxEndPoint;
      }
      
      offsetTable[appVarPP] = maxEndPoint + 1;
      offset += maxEndPoint + 1;
    }
    
    // propagate offsets
    for(u_int i=1; i<callerMaxPP; i++) {
      offsetTable[i] += offsetTable[i-1];
    }
    
    // adjust caller intervals
    for(u_int i=0; i<liveness->GetLength(); i+=2) {
      u_int startPoint = Store::DirectWordToInt(liveness->Sub(i));
      u_int endPoint = Store::DirectWordToInt(liveness->Sub(i+1));
      newLiveness->Init(i,   Store::IntToWord(startPoint + offsetTable[startPoint]));
      newLiveness->Init(i+1, Store::IntToWord(endPoint + offsetTable[endPoint]));
    }
    
    return newLiveness;
  }
  
  
  Vector *InlineAnalyser::MergeAliases(Vector *mergedLiveness) {
    u_int size = livenessInfo.GetLength();
  
    if(size == 0) { // nothing can be inlined
      return Vector::New(0);
    }
    
    // calculate aliases
    Vector *newAliases = Vector::New(nLocals);
    for (u_int i=0; i<nLocals; i++) {
      newAliases->Init(i, Store::IntToWord(i));
    }
    for (u_int i=0; i<size; i++) {
      AppVarLivenessInfo avli = livenessInfo.Sub(i);
      Vector *calleeAliases   = avli.calleeAliases;
      u_int idOffset          = avli.idOffset;
      Vector *idDefs          = avli.calleeIdDefs;
      Vector *idRefs          = avli.appVarIdRefs;
      
      // copy aliases from callee
      for (u_int i=0; i<calleeAliases->GetLength(); i++) {
	u_int src = Store::DirectWordToInt(calleeAliases->Sub(i));
	newAliases->Init(idOffset + i, Store::IntToWord(idOffset + src));
      }
      
      // look for new aliases by matching idDefs with idRefs
      if (idDefs->GetLength() == idRefs->GetLength()) {
        for (u_int j=0; j<idDefs->GetLength(); j++) {
	  
	  TagVal *idRef = TagVal::FromWord(idRefs->Sub(j));
	  AbstractCode::idRef idRefTag = AbstractCode::GetIdRef(idRef);
	  TagVal *idDef = TagVal::FromWord(idDefs->Sub(j));
	  
	  if (idDef != INVALID_POINTER && (idRefTag == AbstractCode::Local || idRefTag == AbstractCode::LastUseLocal)) {
	    u_int ref = Store::DirectWordToInt(idRef->Sel(0));
	    u_int def = Store::DirectWordToInt(idDef->Sel(0));
	    
	    u_int refStart = Store::DirectWordToInt(mergedLiveness->Sub(ref*2));
	    u_int refEnd   = Store::DirectWordToInt(mergedLiveness->Sub(ref*2 + 1));
	    
	    u_int defStart = Store::DirectWordToInt(mergedLiveness->Sub((idOffset+def)*2));
	    u_int defEnd   = Store::DirectWordToInt(mergedLiveness->Sub((idOffset+def)*2 + 1));
	    
	    if (defStart >= refStart && defEnd <= refEnd) {
	      newAliases->Init(idOffset + def, Store::IntToWord(ref));
	    }
	  }
        }
      }
    }
    
    return newAliases;
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
	case ControlStack::ANALYSE_APPVAR: {
          TagVal *instr = stack.PopInstr();
	  analyser->AnalyseAppVar(instr, programPoint);
          break;
	}
	case ControlStack::STOP: {
	  return programPoint;
	}
	case ControlStack::INC: {
	  u_int increment = stack.PopInt();
	  programPoint += increment;
	  break;
	}
	case ControlStack::VISIT: {
          TagVal *instr = stack.PopInstr();
	  AbstractCode::instr instrOp = AbstractCode::GetInstr(instr);
	  analyser->Count(instr);
	  switch(instrOp) {
	  case AbstractCode::Coord:
	  case AbstractCode::Entry:
	  case AbstractCode::Exit:
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
	  case AbstractCode::AppPrim: {
	    TagVal *idDefInstrOpt = TagVal::FromWord(instr->Sel(2));
	    if(idDefInstrOpt == INVALID_POINTER) {
	      programPoint++;
	    } else {
	      Tuple *idDefInstr = Tuple::FromWordDirect(idDefInstrOpt->Sel(0));
	      stack.PushInc(2);
	      stack.PushInstr(idDefInstr->Sel(1));
	    }
	    break;
	  }
	  case AbstractCode::AppVar: {
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
	    break;
	  }
	  case AbstractCode::Raise:
	  case AbstractCode::Reraise:
	  case AbstractCode::Return: {
	    programPoint++;
	    break;
	  }
	  case AbstractCode::Try: {
	    stack.PushInstr(instr->Sel(0));
	    stack.PushInc();
	    stack.PushInstr(instr->Sel(3));
	    break;
	  }
	  case AbstractCode::CompactIntTest: {
	    stack.PushInc();
	    Vector *tests = Vector::FromWordDirect(instr->Sel(2)); 
	    u_int nTests = tests->GetLength();
	    for(u_int i=0; i<nTests; i++)
	      stack.PushInstr(tests->Sub(i));
	    stack.PushInstr(instr->Sel(3));
	    break;
	  }
	  case AbstractCode::IntTest:
	  case AbstractCode::RealTest:
	  case AbstractCode::StringTest: {
	    stack.PushInc();
	    Vector *tests = Vector::FromWordDirect(instr->Sel(1)); 
	    u_int nTests = tests->GetLength();
	    for(u_int i=0; i<nTests; i++) {
	      Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
	      stack.PushInstr(pair->Sel(1));
	    }
	    stack.PushInstr(instr->Sel(2));
	    break;
	  }
	  case AbstractCode::TagTest: {
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
	    break;
	  }
	  case AbstractCode::CompactTagTest: {
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
            break;
	  }
	  case AbstractCode::ConTest: {
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
	    break;
	  }
	  case AbstractCode::VecTest: {
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
	    break;
	  }
	  case AbstractCode::Shared: {
	    word stamp = instr->Sel(0);
	    if(!stamps->IsMember(stamp)) {
	      stamps->Put(stamp,Store::IntToWord(programPoint));
	      stack.PushInstr(instr->Sel(1));
	    }
	    break;
	  }
	  default: {
	    Error("invalid abstract code tag");
	  }
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
//   std::fprintf(stderr, "%d. analyse inlining for %s (%p) %s:%d.%d, nLocals %d\n",
// 	       ++c,
// 	       String::FromWordDirect(coord->Sel(1))->ExportC(),
// 	       abstractCode,
// 	       String::FromWordDirect(coord->Sel(0))->ExportC(),
// 	       Store::DirectWordToInt(coord->Sel(2)),
// 	       Store::DirectWordToInt(coord->Sel(3)),
// 	       GetNumberOfLocals(abstractCode)); 
//   AbstractCode::Disassemble(stderr,
// 			    TagVal::FromWordDirect(abstractCode->Sel(5)));
  return AnalyseRecursively(abstractCode, Map::New(16));
}
