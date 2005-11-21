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

#include "alice/ByteCodeInliner.hh"
#include "alice/AliceConcreteCode.hh"
#include "alice/NativeConcreteCode.hh"
#include "alice/ByteConcreteCode.hh"
#include "alice/LazySelInterpreter.hh"

#define INLINE_LIMIT 10

static inline u_int GetNumberOfLocals(TagVal *abstractCode) {
  TagVal *annotation = TagVal::FromWordDirect(abstractCode->Sel(2));
  switch (AbstractCode::GetAnnotation(annotation)) {
  case AbstractCode::Simple:
    return Store::DirectWordToInt(annotation->Sel(0));
  case AbstractCode::Debug:
    return Vector::FromWordDirect(annotation->Sel(0))->GetLength();
  }
}

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
  TagVal *abstractCode;
  Vector *liveness;
  Map *inlineMap;
  u_int callerMaxPP;
  Map *inlineCandidates;
  LivenessContainer livenessInfo;
  void Append(word key, TagVal *instr, u_int appVarPP,
	      TagVal *acc, Closure *closure,
	      InlineInfo *inlineInfo);
  Vector *MergeLiveness();
public:
  InlineAnalyser(TagVal *ac, Map* map) 
    : abstractCode(ac), counter(0), inlineCandidates(map) {
    subst = Vector::FromWordDirect(abstractCode->Sel(1));
    liveness = Vector::FromWordDirect(abstractCode->Sel(6));
    inlineMap = Map::New(20); 
    nLocals = GetNumberOfLocals(abstractCode);
  }
  void SetMaxPP(u_int pp) { callerMaxPP = pp; }
  // This functions breaks an inline analysis cycle introduced by 
  // mutual recursive functions.
  bool CheckCycle(TagVal *acc) {
    return inlineCandidates->IsMember(acc->Sel(5));
  }  
  void Count(TagVal *instr);
  void AnalyseAppVar(TagVal *instr, u_int pp);
  InlineInfo *ComputeInlineInfo() {
    Assert(counter >= 0);
    return InlineInfo::New(inlineMap,MergeLiveness(),nLocals,counter);
  } 
};

void InlineAnalyser::Count(TagVal *instr) {
  Assert(instr != INVALID_POINTER);
  switch(AbstractCode::GetInstr(instr)) {
  case AbstractCode::Kill:
  case AbstractCode::EndHandle:
    break;
  case AbstractCode::GetTup:
    {
      // Only count this instruction if a real selection is 
      // preformed. Otherwise this instruction forces evaluation.
      // In this case the compiler may detect it and skip the
      // instruction.
      Vector *regs = Vector::FromWordDirect(instr->Sel(0));
      if(regs->GetLength() > 0)
	counter++;
    }
    break;
  case AbstractCode::Close:
    // Avoid to inline Close instruction as this will
    // increase the number of compiler calls 
    // significantly and can cause the system to diverge.
    {
      counter += INLINE_LIMIT + 1;	
    }
    break;
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

  // analyse closure
  // Try to select the closure out of a lazy select closure introduced
  // by the lazy linking mechanism of Alice.
  Closure *closure;
  while ((closure = Closure::FromWord(wClosure)) == INVALID_POINTER) {
    Transient *transient = Store::WordToTransient(wClosure);
    if ((transient != INVALID_POINTER) &&
	(transient->GetLabel() == BYNEED_LABEL)) {
      Closure *byneedClosure = STATIC_CAST(Byneed *, transient)->GetClosure();
      ConcreteCode *concreteCode =
	ConcreteCode::FromWord(byneedClosure->GetConcreteCode());
      if ((concreteCode != INVALID_POINTER) &&
	  (concreteCode->GetInterpreter() == LazySelInterpreter::self)) {
	Record *record = Record::FromWord(byneedClosure->Sub(0));
	if (record != INVALID_POINTER) {
	  UniqueString *label =
	    UniqueString::FromWordDirect(byneedClosure->Sub(1));
	  wClosure = record->PolySel(label);
	  continue;
	}
      }
    }
    return;
  }

  word wcc = closure->GetConcreteCode();
  ConcreteCode *cc = ConcreteCode::FromWord(wcc);
  if(cc != INVALID_POINTER) { // concrete code determined
    Interpreter *interpreter = cc->GetInterpreter();
    if(interpreter == ByteCodeInterpreter::self) {
      ByteConcreteCode *bcc = ByteConcreteCode::FromWord(cc->ToWord());
      Transform *transform =
	STATIC_CAST(Transform *, bcc->GetAbstractRepresentation());
      TagVal *acc = TagVal::FromWordDirect(transform->GetArgument());
      if(CheckCycle(acc)) return; // break inline cycle
      InlineInfo *inlineInfo = bcc->GetInlineInfo();
      u_int nNodes = inlineInfo->GetNNodes();
      if(nNodes <= INLINE_LIMIT) {
	Append(key,instr,appVarPP,acc,closure,inlineInfo);
	// adjust counter
	counter += nNodes - 1; // substract 1 for AppVar instr
      }
    }
  } else { // lazy compile closure
    Transient *transient = Store::WordToTransient(wcc);
    if ((transient != INVALID_POINTER) &&
	(transient->GetLabel() == BYNEED_LABEL)) {
      Closure *byneedClosure =
	STATIC_CAST(Byneed *, transient)->GetClosure();
      wcc = byneedClosure->GetConcreteCode();
      if (wcc == LazyByteCompileInterpreter::concreteCode) {
	LazyByteCompileClosure *lazyBCC =
	  LazyByteCompileClosure::FromWordDirect(byneedClosure->ToWord());
	TagVal *acc = lazyBCC->GetAbstractCode();
	if(CheckCycle(acc)) return; // break inline cycle
	TagVal *inlineInfoOpt = lazyBCC->GetInlineInfoOpt();
	InlineInfo *inlineInfo;
	if(inlineInfoOpt == INVALID_POINTER) {
	  // recursively analyse callee
	  inlineInfo = ByteCodeInliner::AnalyseInlining(acc);
	  lazyBCC->SetInlineInfo(inlineInfo);
	} else
	  inlineInfo = InlineInfo::FromWordDirect(inlineInfoOpt->Sel(0));
	u_int nNodes = inlineInfo->GetNNodes();
	if(nNodes <= INLINE_LIMIT) {
	  Append(key,instr,appVarPP,lazyBCC->GetAbstractCode(),
		 closure,inlineInfo);
	  // adjust counter
	  counter += nNodes - 1;
	}
      }
    }
  }
}

// s_int ExtractPP(TagVal *instr, Vector *liveness) {
//   TagVal *idDefsInstrOpt = TagVal::FromWord(instr->Sel(3));
//   if(idDefsInstrOpt == INVALID_POINTER)
//     return -1;
//   Tuple *idDefsInstr = Tuple::FromWordDirect(idDefsInstrOpt->Sel(0));
//   Vector *fargs = Vector::FromWordDirect(idDefsInstr->Sel(0));
//   for(u_int i = 0; i<fargs->GetLength(); i++) {
//     TagVal *idDefOpt = TagVal::FromWord(fargs->Sub(i));
//     if(idDefOpt != INVALID_POINTER) {
//       u_int id = Store::DirectWordToInt(idDefOpt->Sel(0));
//       for(u_int j = 0; i<liveness->GetLength(); i+=3) {
// 	if(Store::DirectWordToInt(liveness->Sub(j)) == id) {
// 	  return Store::DirectWordToInt(liveness->Sub(j+1));
// 	}
//       }
//     }
//   }
//   return -1;
// }

void InlineAnalyser::Append(word key, TagVal *instr,
			    u_int appVarPP,
			    TagVal *acc, Closure *closure,
			    InlineInfo *inlineInfo) {
  // there can be a strange situation
  // there can be an implicit merge point in the abstract code introduced
  // be an compacttagtest. to me it is not clear which appVarPP i have to
  // choose for the appvar instruction.
  // append liveness
  Vector *calleeLiveness = inlineInfo->GetLiveness();
  word wCalleeLiveness = calleeLiveness->ToWord();
  Tuple *tup = Tuple::New(3);
  tup->Init(0,wCalleeLiveness);
  tup->Init(1,Store::IntToWord(nLocals));
  tup->Init(2,Store::IntToWord(appVarPP));
  livenessInfo.Append(tup->ToWord(),calleeLiveness->GetLength());
  // add closure to substitution, i.e. do specialize
  // TODO: ensure that newSubst matches existing oldSubst
  u_int nGlobals = closure->GetSize();
  Vector *newSubst = Vector::New(nGlobals);
  for(u_int i=nGlobals; i--; ) {
    TagVal *idRef = TagVal::New(AbstractCode::Immediate, 1);
    idRef->Init(0, closure->Sub(i));
    newSubst->Init(i,idRef->ToWord());
  }
  // register inline candidate
  Tuple *info = Tuple::New(4);
  info->Init(0,acc->ToWord());
  info->Init(1,newSubst->ToWord());
  info->Init(2,Store::IntToWord(nLocals));
  info->Init(3,inlineInfo->ToWord());
  //  inlineMap->Put(key,info->ToWord());
  inlineMap->Put(instr->ToWord(),info->ToWord());
  // add number of locals
  nLocals += inlineInfo->GetNLocals();
}

Vector *InlineAnalyser::MergeLiveness() {
  u_int size = livenessInfo.GetLength();
  if(size == 0) // nothing can be inlined
    return liveness;
  // copy intervals of the inlinable functions
  u_int offset = 0;
  u_int offsetTable[callerMaxPP];
  std::memset(offsetTable,0,callerMaxPP*sizeof(u_int));
  u_int l1Length = livenessInfo.GetFlattenedLength();
  u_int liveness1[l1Length];
  for(u_int i = size, index = 0; i--; ) {
    Tuple *tup = Tuple::FromWordDirect(livenessInfo.Sub(i));
    Vector *calleeLiveness = Vector::FromWordDirect(tup->Sel(0));
    u_int idOffset = Store::WordToInt(tup->Sel(1));
    u_int appVarPP = callerMaxPP - Store::DirectWordToInt(tup->Sel(2)) + 1;
    u_int maxEndPoint = 0;
    for(u_int j=0; j<calleeLiveness->GetLength(); j+=3) {
      u_int identifier =
	Store::DirectWordToInt(calleeLiveness->Sub(j)) + idOffset;      
      u_int startPoint = 
	Store::DirectWordToInt(calleeLiveness->Sub(j+1)) + offset + appVarPP;
      u_int endPoint = Store::DirectWordToInt(calleeLiveness->Sub(j+2)); 
      if(maxEndPoint < endPoint) 
	maxEndPoint = endPoint;
      endPoint += offset + appVarPP;
      liveness1[index++] = identifier;
      liveness1[index++] = startPoint;
      liveness1[index++] = endPoint; 
    }    
    offsetTable[appVarPP] = maxEndPoint + 1;
    offset += maxEndPoint + 1;
  }
  // propagate offsets
  for(u_int i=1; i<callerMaxPP; i++) {
    offsetTable[i] += offsetTable[i-1];
  }
  // copy the caller intervals
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
  return newLiveness;  
}

// compute program points of appvar instructions

class ControlStack {
private:
  u_int *stack;
  u_int size;
  s_int top;
  void Push(u_int item) {
    if( ++top >= size ) {	
      u_int oldSize = size;
      size = size * 3 / 2;
      u_int *newStack = new u_int[size];
      memcpy(newStack,stack,oldSize * sizeof(u_int));
      delete[] stack;
      stack = newStack;
    }
    stack[top] = item;
  }
  u_int Pop() { return stack[top--]; }
public:
  enum { VISIT, INC, ANALYSE_APPVAR,STOP };
  ControlStack(u_int s = 400) : size(s), top(-1) { stack = new u_int[size]; }
  u_int PopInt() { return Pop(); }
  u_int PopCommand() { return Pop(); }
  TagVal *PopInstr() { return (TagVal *) Pop(); }
  word PopWord() { return (word) Pop(); }
  TagVal *PopTagVal() { return (TagVal *) Pop(); }
  void PushInstr(word instr) {
    Push((u_int) (TagVal::FromWordDirect(instr)));
    Push(VISIT);
  }
  void PushInc() { Push(1); Push(INC); }
  void PushInc(u_int i) { Push(i); Push(INC); }
  void PushAnalyseAppVar(TagVal *instr) {
    Push((u_int) instr);
    Push(ANALYSE_APPVAR);
  }
  void PushStop() { Push(STOP); }
  bool Empty() { return top == -1; }
  s_int GetTopIndex() { return top; }
  void SetTopIndex(s_int index) { top = index; }
};

class PPAnalyser {
private:
  u_int programPoint;
  ControlStack stack;
    
public:
  PPAnalyser() : programPoint(0) {}
  void RunAnalysis(TagVal *instr, InlineAnalyser *analyser);
  u_int GetMaxPP() { return programPoint; }
};

void PPAnalyser::RunAnalysis(TagVal *instr, InlineAnalyser *analyser) {
  IntMap *stamps = IntMap::New(100); // remember control flow merge points
  stack.PushStop();
  stack.PushInstr(instr->ToWord());
  for(;;) {
    switch(stack.PopCommand()) {
    case ControlStack::ANALYSE_APPVAR:
      {
	TagVal *instr = stack.PopInstr();
	analyser->AnalyseAppVar(instr,programPoint);
      }
      break;
    case ControlStack::STOP:
      return;
    case ControlStack::INC:
      {
	u_int increment = stack.PopInt();
	programPoint += increment;
      }
      break;	
      // visit nodes and count program points
    case ControlStack::VISIT: 
      {
	TagVal *instr = stack.PopInstr();
	analyser->Count(instr);
	switch(AbstractCode::GetInstr(instr)) {
	case AbstractCode::EndTry:
	case AbstractCode::EndHandle:
	  stack.PushInstr(instr->Sel(0));
	  break;
	case AbstractCode::Kill:
	  stack.PushInstr(instr->Sel(1));
	  break;
	case AbstractCode::PutVar:
	  stack.PushInc(2);
	  stack.PushInstr(instr->Sel(2));
	  break;
	case AbstractCode::PutNew:
	  stack.PushInc();
	  stack.PushInstr(instr->Sel(2));
	  break;
	case AbstractCode::PutTag:
	  stack.PushInc(2);
	  stack.PushInstr(instr->Sel(4)); 
	  break;
	case AbstractCode::PutCon:
	  stack.PushInc(2);
	  stack.PushInstr(instr->Sel(3)); 
	  break;
	case AbstractCode::PutRef:
	  stack.PushInc(2);
	  stack.PushInstr(instr->Sel(2));
	  break;
	case AbstractCode::PutTup:
	  stack.PushInc(2);
	  stack.PushInstr(instr->Sel(2));
	  break;
	case AbstractCode::PutPolyRec:
	  stack.PushInc(2);
	  stack.PushInstr(instr->Sel(3));
	  break;
	case AbstractCode::PutVec:
	  stack.PushInc(2);
	  stack.PushInstr(instr->Sel(2));
	  break;
	case AbstractCode::Close:
	case AbstractCode::Specialize:
	  stack.PushInc(2);
	  stack.PushInstr(instr->Sel(3));
	  break;
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
	case AbstractCode::GetRef:
	  stack.PushInc(2);
	  stack.PushInstr(instr->Sel(2)); 
	  break;
	case AbstractCode::GetTup:
	  stack.PushInc();
	  stack.PushInstr(instr->Sel(2));
	  break;
	case AbstractCode::Sel:
	  stack.PushInc(2);
	  stack.PushInstr(instr->Sel(3)); 
	  break;
	case AbstractCode::LazyPolySel:
	  stack.PushInc(2);
	  stack.PushInstr(instr->Sel(3)); 
	  break;
	case AbstractCode::Raise:
	case AbstractCode::Reraise:
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
	case AbstractCode::Return:
	  {
	    programPoint++;
	  }
	  break;
	default:
	  fprintf(stderr,"invalid abstractCode tag %d\n",
		  (u_int)AbstractCode::GetInstr(instr));
	  return;
	}
      }
    }
  }
}

Map *ByteCodeInliner::inlineCandidates;

void ByteCodeInliner::Driver(TagVal *instr, InlineAnalyser *analyser) {
  PPAnalyser a;
  a.RunAnalysis(instr,analyser);
  analyser->SetMaxPP(a.GetMaxPP());
}

InlineInfo *ByteCodeInliner::AnalyseInlining(TagVal *abstractCode) {
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
  
  inlineCandidates->Put(abstractCode->Sel(5),Store::IntToWord(0));  
  InlineAnalyser inliner(abstractCode,inlineCandidates);
  Driver(TagVal::FromWordDirect(abstractCode->Sel(5)),&inliner);
  inlineCandidates->Remove(abstractCode->Sel(5));
  return inliner.ComputeInlineInfo();
}
