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
    
    // an AppVar that applies a closure that is eliminated by the uncurry optimization
    struct EliminatedAppVar {
      TagVal *instr;
      u_int closureId;
      UncurryInfo *uncurryInfo;
    };
    
    u_int counter;
    u_int nLocals;
    TagVal *abstractCode;
    Vector *liveness;
    Map *appVarPPs;
    Map *inlineMap;
    Map *omittedAppVars;
    Map *uncurriedAppVars;
    u_int callerMaxPP;
    Map *parentRootInstrs;
    LivenessContainer livenessInfo;  
    
    void Append(TagVal *instr, u_int appVarPP, Closure *closure, Vector *idRefs, InlineInfo *inlineInfo);
    Vector *MergeLiveness();
    Vector *MergeAliases(Vector *mergedLiveness);

  public:
    
    InlineAnalyser(TagVal *ac, Map* map)
      : abstractCode(ac), counter(0), parentRootInstrs(map) {
      liveness = Vector::FromWordDirect(abstractCode->Sel(6));
      appVarPPs = Map::New(10);
      inlineMap = Map::New(10);
      omittedAppVars = Map::New(10);
      uncurriedAppVars = Map::New(10);
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
    s_int ExtractSingleIdDef(Vector *idDefs);
    Closure *IdRefToClosure(word wIdRef);
    InlineInfo *GetInlineInfo(ConcreteCode *cc);
    UncurryInfo *GetUncurryInfo(ConcreteCode *cc);
    void RecursiveUncurry(ConcreteCode *cc, TagVal *appVar, std::vector<EliminatedAppVar> &appVars);
    void RemoveLiveness(u_int id);
    void ExtendIdRefLiveness(Vector *idRefs, u_int pp);
    void AnalyseAppVar(TagVal* instr, u_int appVarPP);
    
    InlineInfo *ComputeInlineInfo() {
      Assert(counter >= 0);
      Vector *newLiveness = MergeLiveness();
      Vector *newAliases = MergeAliases(newLiveness);
      return InlineInfo::New(inlineMap, omittedAppVars, uncurriedAppVars, newLiveness, newAliases, counter);
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
      // performed. Otherwise this instruction forces evaluation.
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
  
  
  s_int InlineAnalyser::ExtractSingleIdDef(Vector *idDefs) {
    if(idDefs->GetLength() == 1) {
      TagVal *idDef = TagVal::FromWord(idDefs->Sub(0));
      if (idDef != INVALID_POINTER) {
	return Store::DirectWordToInt(idDef->Sel(0));
      }
    }
    return -1;
  }
  
  
  Closure *InlineAnalyser::IdRefToClosure(word wIdRef) {
    TagVal *idRef = TagVal::FromWordDirect(wIdRef);
    
    if(AbstractCode::GetIdRef(idRef) == AbstractCode::Global) {
      Vector *subst = Vector::FromWordDirect(abstractCode->Sel(1));
      u_int index = Store::DirectWordToInt(idRef->Sel(0));
      TagVal *substIdRef = TagVal::FromWordDirect(subst->Sub(index));
      if (AbstractCode::GetIdRef(substIdRef) == AbstractCode::Immediate) {
	return Closure::FromWord(LazySelInterpreter::Deref(substIdRef->Sel(0)));
      }
    }
    else if (AbstractCode::GetIdRef(idRef) == AbstractCode::Immediate) {
      return Closure::FromWord(LazySelInterpreter::Deref(idRef->Sel(0)));
    }
    return INVALID_POINTER;
  }
  
  
  InlineInfo *InlineAnalyser::GetInlineInfo(ConcreteCode *cc) {
    Interpreter *interpreter = cc->GetInterpreter();
    
    if(interpreter == ByteCodeInterpreter::self) {
      ByteConcreteCode *bcc = ByteConcreteCode::FromWordDirect(cc->ToWord());
      TagVal *ac = bcc->GetAbstractCode();
      if(CheckCycle(ac)) return INVALID_POINTER; // break inline cycle
      return bcc->GetInlineInfo();
    }
    else if(interpreter == HotSpotInterpreter::self) {
      HotSpotConcreteCode *hscc = HotSpotConcreteCode::FromWordDirect(cc->ToWord());
      TagVal *ac = hscc->GetAbstractCode();
      if(CheckCycle(ac)) return INVALID_POINTER; // break inline cycle
      TagVal *inlineInfoOpt = hscc->GetInlineInfoOpt();
      if(inlineInfoOpt == INVALID_POINTER) {
	InlineInfo *inlineInfo = AnalyseRecursively(ac, parentRootInstrs);
	hscc->SetInlineInfo(inlineInfo);
	return inlineInfo;
      }
      return InlineInfo::FromWordDirect(inlineInfoOpt->Sel(0));
    }
    return INVALID_POINTER;
  }
  
  
  UncurryInfo *InlineAnalyser::GetUncurryInfo(ConcreteCode *cc) {
    Interpreter *interpreter = cc->GetInterpreter();
    
    if(interpreter == ByteCodeInterpreter::self) {
      return reinterpret_cast<ByteConcreteCode*>(cc)->GetUncurryInfo();
    }
    else if(interpreter == HotSpotInterpreter::self) {
      return reinterpret_cast<HotSpotConcreteCode*>(cc)->GetUncurryInfo();
    }
    return INVALID_POINTER;
  }

  
  void InlineAnalyser::RecursiveUncurry(ConcreteCode *cc, TagVal *appVar, std::vector<EliminatedAppVar> &appVars) {
    
    UncurryInfo *uncurryInfo = GetUncurryInfo(cc);
    TagVal *contOpt = TagVal::FromWord(appVar->Sel(3));
    if (uncurryInfo == INVALID_POINTER || contOpt == INVALID_POINTER) {
      return;
    }
    Tuple *idDefsInstr = Tuple::FromWordDirect(contOpt->Sel(0));
    s_int closureId = ExtractSingleIdDef(Vector::FromWordDirect(idDefsInstr->Sel(0)));
    if (closureId < 0) {
      return;
    }
    TagVal *instr = TagVal::FromWordDirect(idDefsInstr->Sel(1));
    
    // find the next AppVar in the sequence of applications
    while(true) {
      AbstractCode::instr tag = AbstractCode::GetInstr(instr);
      switch(tag) {
	// instrs which can never reference closureId
	// (but the continuation might)
	case AbstractCode::GetRef:
	case AbstractCode::GetTup:
	case AbstractCode::Sel:
	case AbstractCode::LazyPolySel:
	case AbstractCode::Coord: {
	  u_int cp = AbstractCode::GetContinuationPos(tag);
	  instr = TagVal::FromWordDirect(instr->Sel(cp));
	  break;
	}
	// instrs which might reference closureId
	case AbstractCode::PutVar: {
	  if (AbstractCode::IsId(TagVal::FromWordDirect(instr->Sel(1)), closureId)) {
	    return;
	  }
	  instr = TagVal::FromWordDirect(instr->Sel(2));
	  break;
	}
	case AbstractCode::PutTag: {
	  if (AbstractCode::IdRefsContain(Vector::FromWordDirect(instr->Sel(3)), closureId)) {
	    return;
	  }
	  instr = TagVal::FromWordDirect(instr->Sel(4));
	  break;
	}
	case AbstractCode::PutRef: {
	  if (AbstractCode::IsId(TagVal::FromWordDirect(instr->Sel(1)), closureId)) {
	    return;
	  }
	  instr = TagVal::FromWordDirect(instr->Sel(2));
	  break;
	}
	case AbstractCode::PutTup:
	case AbstractCode::PutVec: {
	  if (AbstractCode::IdRefsContain(Vector::FromWordDirect(instr->Sel(1)), closureId)) {
	    return;
	  }
	  instr = TagVal::FromWordDirect(instr->Sel(2));
	  break;
	}
	case AbstractCode::AppPrim: {
          TagVal *contOpt = TagVal::FromWord(instr->Sel(2));
	  if (AbstractCode::IdRefsContain(Vector::FromWordDirect(instr->Sel(1)), closureId) || contOpt == INVALID_POINTER) {
	    return;
	  }
          Tuple *idDefInstr = Tuple::FromWordDirect(contOpt->Sel(0));
	  instr = TagVal::FromWordDirect(idDefInstr->Sel(1));
	  break;
	}
	// this might be the AppVar we are looking for
	case AbstractCode::AppVar: {
	  TagVal *idRef = TagVal::FromWordDirect(instr->Sel(0));
	  Vector *idRefs = Vector::FromWordDirect(instr->Sel(1));
	  if (AbstractCode::IdRefsContain(idRefs, closureId)) {
	    return;
	  }
	  if (AbstractCode::IsLastUseLocal(idRef, closureId)) { // we have found the next AppVar
            EliminatedAppVar aav = { instr, closureId, uncurryInfo };
	    appVars.push_back(aav);
	    RecursiveUncurry(uncurryInfo->GetConcreteCode(), instr, appVars);
	    return;
	  }
          TagVal *contOpt = TagVal::FromWord(instr->Sel(3));
	  if (contOpt == INVALID_POINTER) {
	    return;
	  }
          Tuple *idDefsInstr = Tuple::FromWordDirect(contOpt->Sel(0));
	  instr = TagVal::FromWordDirect(idDefsInstr->Sel(1));
	}
	// if we reach any other instruction then we have no
	// hope of finding the AppVar we are looking for
	default: {
	  return;
	}
      }
    }
  }
  
  
  void InlineAnalyser::RemoveLiveness(u_int id) {
    // liveness is copied lazily
    Vector *origLiveness = Vector::FromWordDirect(abstractCode->Sel(6));
    if (liveness == origLiveness) {
      liveness = origLiveness->Clone();
    }
    // if start > end then id is considered never alive
    liveness->Init(id*2,   Store::IntToWord(1));
    liveness->Init(id*2+1, Store::IntToWord(0));
  }
  
  
  void InlineAnalyser::ExtendIdRefLiveness(Vector *idRefs, u_int pp){
    for (u_int i=0; i<idRefs->GetLength(); i++) {
      TagVal *idRef = TagVal::FromWordDirect(idRefs->Sub(i));
      AbstractCode::idRef tag = AbstractCode::GetIdRef(idRef);
      if (tag == AbstractCode::Local || tag == AbstractCode::LastUseLocal) {
	u_int id = Store::DirectWordToInt(idRef->Sel(0));
	u_int curEnd = Store::DirectWordToInt(liveness->Sub(id*2+1));
	if (pp > curEnd) {
          // liveness is copied lazily
	  Vector *origLiveness = Vector::FromWordDirect(abstractCode->Sel(6));
          if (liveness == origLiveness) {
            liveness = origLiveness->Clone();
          }
          liveness->Init(id*2+1, Store::IntToWord(pp));
	}
      }
    }
  }
  

  void InlineAnalyser::AnalyseAppVar(TagVal *instr, u_int appVarPP) {
    Assert(AbstractCode::GetInstr(instr) == AbstractCode::AppVar);
    appVarPPs->Put(instr->ToWord(), Store::IntToWord(appVarPP));
    
    Closure *closure = IdRefToClosure(instr->Sel(0));
    if(closure == INVALID_POINTER) {
      return;
    }
    ConcreteCode *cc = ConcreteCode::FromWordDirect(closure->GetConcreteCode());
    
    // try and apply the uncurry optimization
    std::vector<EliminatedAppVar> appVars;
    RecursiveUncurry(cc, instr, appVars);
    if (!appVars.empty()) {

      //String *name = AbstractCodeInterpreter::MakeProfileName(AbstractCodeInterpreter::ConcreteToAbstractCode(cc->ToWord()));
      //static int count = 0;
      //fprintf(stderr, "found candidate #%d! (%s)\n", count++, name->ExportC());
      
      Vector *argsIdRefs   = Vector::New(appVars.size() + 1);
      Vector *argsCCCSizes = Vector::New(appVars.size() + 1);
      EliminatedAppVar lastAppVar = appVars.back();
      u_int idEndPP = Store::DirectWordToInt(liveness->Sub(lastAppVar.closureId * 2 + 1));
      
      Vector *idRefs = Vector::FromWordDirect(instr->Sel(1));
      argsIdRefs->Init(0, idRefs->ToWord());
      argsCCCSizes->Init(0, Store::IntToWord(cc->GetInterpreter()->GetInArity(cc)));
      ExtendIdRefLiveness(idRefs, idEndPP);
      TagVal *contOpt = TagVal::FromWord(instr->Sel(3));
      Tuple *idDefsInstr = Tuple::FromWordDirect(contOpt->Sel(0));
      omittedAppVars->Put(instr->ToWord(), idDefsInstr->Sel(1));
      
      for(u_int i=0; i<appVars.size(); i++) {
	RemoveLiveness(appVars[i].closureId);
	UncurryInfo *ui = appVars[i].uncurryInfo;
	TagVal *instr = appVars[i].instr;
	
	argsIdRefs->Init(i+1, instr->Sel(1));
	argsCCCSizes->Init(i+1, Store::IntToWord(ui->GetInArity() - ui->GetChildArgsOffset()));
	
	if (i < appVars.size()-1) {
	  ExtendIdRefLiveness(Vector::FromWordDirect(instr->Sel(1)), idEndPP);
	  TagVal *idDefsInstrOpt = TagVal::FromWordDirect(instr->Sel(3));
	  Tuple *idDefsInstr = Tuple::FromWordDirect(idDefsInstrOpt->Sel(0));
	  omittedAppVars->Put(instr->ToWord(), idDefsInstr->Sel(1));
	}
      }
      
      ConcreteCode *newCC = lastAppVar.uncurryInfo->GetConcreteCode();
      Closure *newClosure = Closure::New(newCC->ToWord(), closure->GetSize());
      for (u_int i=0; i<closure->GetSize(); i++) {
	newClosure->Init(i, closure->Sub(i));
      }
      UncurriedAppVarInfo *uavi = UncurriedAppVarInfo::New(newClosure, argsIdRefs, argsCCCSizes);
      
      // sometimes the uncurried function can itself be inlined
      if (uavi->TrivialCCC()) {
	InlineInfo *inlineInfo = GetInlineInfo(newCC);
	if (inlineInfo != INVALID_POINTER) {
          u_int nNodes = inlineInfo->GetNNodes();
          if(nNodes <= INLINE_LIMIT) {
	    u_int appVarPP = Store::DirectWordToInt(appVarPPs->Get(lastAppVar.instr->ToWord()));
	    Append(lastAppVar.instr, appVarPP, newClosure, argsIdRefs->Flatten(), inlineInfo);
	    counter += nNodes - 1; // adjust counter: substract 1 for AppVar instr
	    return;
	  }
	}
      }
      
      uncurriedAppVars->Put(lastAppVar.instr->ToWord(), uavi->ToWord());
      return;
    }
    
    InlineInfo *inlineInfo = GetInlineInfo(cc);
    if(inlineInfo != INVALID_POINTER) {
      u_int nNodes = inlineInfo->GetNNodes();
      if(nNodes <= INLINE_LIMIT) {
	Vector *idRefs = Vector::FromWordDirect(instr->Sel(1));
	Append(instr, appVarPP, closure, idRefs, inlineInfo);
	counter += nNodes - 1; // adjust counter: substract 1 for AppVar instr
      }
    }
  }


  void InlineAnalyser::Append(TagVal *instr, u_int appVarPP, Closure *closure, Vector *appVarIdRefs, InlineInfo *inlineInfo) {
    TagVal *ac = AbstractCodeInterpreter::ConcreteToAbstractCode(closure->GetConcreteCode());

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
    avli.appVarIdRefs = appVarIdRefs;
    livenessInfo.Append(avli);
    
    // add closure to substitution, i.e. do specialize
    Vector *oldSubst = Vector::FromWordDirect(ac->Sel(1));
    Vector *newSubst = Vector::New(oldSubst->GetLength());
    for(u_int i=0, j=0; i<oldSubst->GetLength(); i++) {
      TagVal *substIdRef = TagVal::FromWordDirect(oldSubst->Sub(i));
      if (AbstractCode::GetIdRef(substIdRef) == AbstractCode::Global) {
	substIdRef = TagVal::New1(AbstractCode::Immediate, closure->Sub(j++));
      }
      newSubst->Init(i, substIdRef->ToWord());
    }
    
    AppVarInfo *avi = AppVarInfo::New(ac, newSubst, avli.appVarIdRefs, nLocals, inlineInfo, closure);
    inlineMap->Put(instr->ToWord(), avi->ToWord());
    nLocals += inlineInfo->GetNLocals();
  }


  Vector *InlineAnalyser::MergeLiveness() {
    u_int size = livenessInfo.GetLength();
    
    if(size == 0) { // nothing can be inlined
     return liveness;
    }
    
    Vector *newLiveness = Vector::New(nLocals * 2);
    u_int offsetTable[callerMaxPP + 1];
    std::memset(offsetTable, 0,  (callerMaxPP + 1)*sizeof(u_int));
    
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
    for(u_int i=1; i<=callerMaxPP; i++) {
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
	    
	    Assert(defStart <= defEnd);
	    Assert(refStart <= refEnd);
	    Assert(defStart >= refStart);
	    // extend ref liveness to cover def
	    mergedLiveness->Init(ref*2+1, Store::IntToWord(defEnd > refEnd ? defEnd : refEnd));
	    newAliases->Init(idOffset+def, Store::IntToWord(ref));
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


static u_int CountAppVar(word appVar, word wAppVarInfo, u_int count) {
  AppVarInfo *avi = AppVarInfo::FromWord(wAppVarInfo);
  return avi->GetInlineInfo()->GetInlineMap()->Fold(CountAppVar, count+1);
}


u_int InlineInfo::NumInlinedAppVars() {
  return GetInlineMap()->Fold(CountAppVar, static_cast<u_int>(0));
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
