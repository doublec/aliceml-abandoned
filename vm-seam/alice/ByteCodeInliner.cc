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

using namespace ByteCodeInliner_Internal;

// compute program points of appvar instructions

// main control for computing liveness intervals
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
  enum { 
    VISIT, INC, ID, IDS, IDDEF, IDDEFS, IDREF, IDREFS,
    REGISTER_APPVAR_PP,
    STATE, STOP
  };
  ControlStack(u_int s = 400) : size(s), top(-1) { stack = new u_int[size]; }
  u_int PopCommand() { return Pop(); }
  TagVal *PopInstr() { return (TagVal *) Pop(); }
  Vector *PopVector() { return (Vector *) Pop(); }
  word PopWord() { return (word) Pop(); }
  void PopState(IntMap **shared, u_int *programPoint) {
    *programPoint = Pop();
    *shared = (IntMap *) Pop();
  }
  TagVal *PopTagVal() { return (TagVal *) Pop(); }
  void PushInstr(word instr) {
    Push((u_int) (TagVal::FromWordDirect(instr)));
    Push(VISIT);
  }
  void PushInc() { Push(INC); }
  void PushId(word id) { 
    Push((u_int) id);
    Push(ID);
  }
  void PushIds(word ids) {
    Push((u_int) (TagVal::FromWordDirect(ids)));
    Push(IDS);
  }
  void PushIdDef(word idDef) {
    Push((u_int) (TagVal::FromWord(idDef)));
    Push(IDDEF);
  }
  void PushIdDefs(word idDefs) {
    Push((u_int) (Vector::FromWordDirect(idDefs)));
    Push(IDDEFS);
  }
  void PushIdRef(word idRef) {
    Push((u_int) (TagVal::FromWordDirect(idRef)));
    Push(IDREF);
  }
  void PushIdRefs(word idRefs) {
    Push((u_int) (Vector::FromWordDirect(idRefs)));
    Push(IDREFS);
  }
  void PushState(IntMap *shared, u_int programPoint) {
    Push((u_int) shared);
    Push(programPoint);
    //      Push(STATE);
  }
  void PushAppVarPP(TagVal *instr) {
    Push((u_int) instr);
    Push(REGISTER_APPVAR_PP);
  }
  void PushStop() { Push(STOP); }
  bool Empty() { return top == -1; }
  s_int GetTopIndex() { return top; }
  void SetTopIndex(s_int index) { top = index; }
};

class LivenessAnalyser {
private:
  u_int programPoint;
  TagVal *abstractCode;
  u_int nLocals;
  s_int *ranges;
  u_int rangesSize;
  Container appVarPPs;
  ControlStack stack;
  
  void AdjustInterval(word id);
  void RunAnalysis();
  Vector *GetLiveness();
  Map *GetAppVarPPs();
  u_int GetProgramPoint() { return programPoint; }
  
  LivenessAnalyser(TagVal *ac) : programPoint(0), abstractCode(ac) {
    nLocals = GetNumberOfLocals(abstractCode);
    rangesSize = nLocals * 2; 
    ranges = new s_int[rangesSize];
    std::memset(ranges,-1,2 * nLocals * sizeof(s_int));
  }
  ~LivenessAnalyser() {
    delete ranges;
  }
public:
  static Tuple *ComputeLiveness(TagVal *abstractCode);
  static bool Check(Vector *liveness1, Vector *liveness2);
};

Tuple *LivenessAnalyser::ComputeLiveness(TagVal *abstractCode) {
  LivenessAnalyser a(abstractCode);
  a.RunAnalysis();
  Vector *liveness = a.GetLiveness();
  Map *appVarPPs = a.GetAppVarPPs();
  u_int pp = a.GetProgramPoint();
  Tuple *triple = Tuple::New(3);
  triple->Init(0,liveness->ToWord());
  triple->Init(1,appVarPPs->ToWord());
  triple->Init(2,Store::IntToWord(pp));
  return triple;
}

bool LivenessAnalyser::Check(Vector *liveness1, Vector *liveness2) {
  if(liveness1->GetLength() != liveness2->GetLength()) {
    fprintf(stderr,"different number of liveness intervals\n");
    return false;
  }
  for(u_int i = 0; i<liveness1->GetLength(); i += 3) {
    u_int id1 = Store::DirectWordToInt(liveness1->Sub(i));
    u_int s1  = Store::DirectWordToInt(liveness1->Sub(i+1));
    u_int e1  = Store::DirectWordToInt(liveness1->Sub(i+2));
    u_int id2 = Store::DirectWordToInt(liveness2->Sub(i));
    u_int s2  = Store::DirectWordToInt(liveness2->Sub(i+1));
    u_int e2  = Store::DirectWordToInt(liveness2->Sub(i+2));
    if(id1 != id2) {
      fprintf(stderr,"%d. id1 %d != id2 %d\n",i,id1,id2);
      return false;
    }
    if(s1 != s2 || e1 != e2) {
      fprintf(stderr,"%d. [%d,%d] != [%d,%d]\n",id1,s1,e1,s2,e2);
      return false;
    }
  }
  return true;
}

Vector *LivenessAnalyser::GetLiveness() {
  u_int size = 0;
  u_int sizeOfRanges = nLocals * 2;
  u_int cranges[sizeOfRanges * 3 / 2];
  // counting sort
  u_int max = programPoint+1;
  u_int counts[max];
  std::memset(counts,0,max * sizeof(u_int));
  for(u_int i=0; i<sizeOfRanges; i+=2) {
    if(ranges[i] > 0) {
      // adjust
      cranges[size] = i / 2;
      cranges[size + 1] = programPoint - ranges[i + 1];
      cranges[size + 2] = programPoint - ranges[i];
      // count
      counts[cranges[size+1]]++;
      size += 3;
    }
  }
  Vector *liveness = Vector::New(size);
  for(u_int i=1; i<max; i++) counts[i] += counts[i-1];
  for(u_int i=0; i<size; i+=3) {
    u_int id = cranges[i];
    u_int x = cranges[i + 1];
    u_int y = cranges[i + 2];
    u_int index = 3 * (--counts[x]);
    liveness->Init(index,Store::IntToWord(id));
    liveness->Init(index+1,Store::IntToWord(x));
    liveness->Init(index+2,Store::IntToWord(y));
  }
  return liveness;
}

Map *LivenessAnalyser::GetAppVarPPs() {
  u_int length = appVarPPs.GetLength();
  Map *map = Map::New(2 * length);
//   if(length > 0)
//     fprintf(stderr,"appvar program points:\n");
  for (u_int i = length; i--; ) {
    Tuple *pair = Tuple::FromWordDirect(appVarPPs.Sub(i));
    // adjust program point
    u_int pp = programPoint - Store::DirectWordToInt(pair->Sel(1));
//     fprintf(stderr," %p -> %d\n",pair->Sel(0),pp); 
    map->Put(pair->Sel(0),Store::IntToWord(pp));
  }
  return map;
}

inline void LivenessAnalyser::AdjustInterval(word id) {
  u_int index = Store::DirectWordToInt(id) * 2;
  s_int min = ranges[index];
  if(min >= 0) { // is already there
    if(programPoint < min) ranges[index] = programPoint;
    s_int max = ranges[index | 1];
    if(programPoint > max) ranges[index + 1] = programPoint;    
  } else {
    ranges[index] = programPoint;
    ranges[index + 1] = programPoint;
  }
}

void LivenessAnalyser::RunAnalysis() {
  IntMap *stamps = IntMap::New(100); // remember control flow merge points
  stack.PushStop();
  stack.PushIdDefs(abstractCode->Sel(3)); // arguments
  stack.PushInstr(abstractCode->Sel(5));
  for(;;) {
    switch(stack.PopCommand()) {
    case ControlStack::REGISTER_APPVAR_PP:
      {
	TagVal *instr = stack.PopInstr();
	Tuple *pair = Tuple::New(2);
	pair->Init(0,instr->ToWord());
	pair->Init(1,Store::IntToWord(programPoint));
	appVarPPs.Append(pair->ToWord());
      }
      break;
    case ControlStack::STOP:
      return;
    case ControlStack::INC:
      programPoint++;
      break;	
      // adjust invervals
    case ControlStack::ID:
      AdjustInterval(stack.PopWord());
      break;
    case ControlStack::IDS:
      {
	Vector *ids = stack.PopVector();
	u_int nIds = ids->GetLength();
	for(u_int i=0; i<nIds; i++)
	  AdjustInterval(ids->Sub(i));
      }
      break;
    case ControlStack::IDDEF:
      {
	TagVal *idDefOpt = stack.PopTagVal();
	if(idDefOpt != INVALID_POINTER)
	  AdjustInterval(idDefOpt->Sel(0));	
      }
      break;
    case ControlStack::IDDEFS:
      {
	Vector *idDefs = stack.PopVector();
	u_int nIdDefs = idDefs->GetLength();
	for(u_int i=0; i<nIdDefs; i++) {
	  TagVal *idDefOpt = TagVal::FromWord(idDefs->Sub(i));
	  if(idDefOpt != INVALID_POINTER)
	    AdjustInterval(idDefOpt->Sel(0));
	}
      }
      break;
    case ControlStack::IDREF:
      {
	TagVal *idRef = stack.PopTagVal();
	switch(AbstractCode::GetIdRef(idRef)) {
	case AbstractCode::Local:
	case AbstractCode::LastUseLocal:
	  AdjustInterval(idRef->Sel(0));
	  break;
	default:
	  // do nothing
	  // later versions should include liveness for globals and
	  // immediates
	  ;
	}
      }
      break;
    case ControlStack::IDREFS:
      {
	Vector *idRefs = stack.PopVector();
	u_int nIdRefs = idRefs->GetLength();
	for(u_int i=0; i<nIdRefs; i++) {
	  TagVal *idRef = TagVal::FromWordDirect(idRefs->Sub(i));
	  switch(AbstractCode::GetIdRef(idRef)) {
	  case AbstractCode::Local:
	  case AbstractCode::LastUseLocal:
	    AdjustInterval(idRef->Sel(0));
	    break;
	  default:
	    // do nothing
	    // later versions should include liveness for globals and
	    // immediates
	    ;
	  }
	}
      }
      break;
      // visit nodes and count program points
    case ControlStack::VISIT: 
      {
	TagVal *instr = stack.PopInstr();
	switch(AbstractCode::GetInstr(instr)) {
	case AbstractCode::EndTry:
	case AbstractCode::EndHandle:
	  stack.PushInstr(instr->Sel(0));
	  break;
	case AbstractCode::Kill:
	  stack.PushInstr(instr->Sel(1));
	  break;
	case AbstractCode::PutVar:
	  stack.PushIdRef(instr->Sel(1));
	  stack.PushInc();
	  stack.PushId(instr->Sel(0));
	  stack.PushInc();
	  stack.PushInstr(instr->Sel(2));
	  break;
	case AbstractCode::PutNew:
	  stack.PushId(instr->Sel(0));
	  stack.PushInc();
	  stack.PushInstr(instr->Sel(2));
	  break;
	case AbstractCode::PutTag:
	  stack.PushId(instr->Sel(0));
	  stack.PushInc();
	  stack.PushIdRefs(instr->Sel(3));
	  stack.PushInc();
	  stack.PushInstr(instr->Sel(4)); 
	  break;
	case AbstractCode::PutCon:
	  stack.PushId(instr->Sel(0));
	  stack.PushInc();
	  stack.PushIdRef(instr->Sel(1));
	  stack.PushIdRefs(instr->Sel(2));
	  stack.PushInc();
	  stack.PushInstr(instr->Sel(3)); 
	  break;
	case AbstractCode::PutRef:
	  stack.PushId(instr->Sel(0));
	  stack.PushInc();
	  stack.PushIdRef(instr->Sel(1));
	  stack.PushInc();
	  stack.PushInstr(instr->Sel(2));
	  break;
	case AbstractCode::PutTup:
	  stack.PushId(instr->Sel(0));
	  stack.PushInc();
	  stack.PushIdRefs(instr->Sel(1));
	  stack.PushInc();
	  stack.PushInstr(instr->Sel(2));
	  break;
	case AbstractCode::PutPolyRec:
	  stack.PushId(instr->Sel(0));
	  stack.PushInc();
	  stack.PushIdRefs(instr->Sel(2));
	  stack.PushInc();
	  stack.PushInstr(instr->Sel(3));
	  break;
	case AbstractCode::PutVec:
	  stack.PushId(instr->Sel(0));
	  stack.PushInc();
	  stack.PushIdRefs(instr->Sel(1));
	  stack.PushInc();
	  stack.PushInstr(instr->Sel(2));
	  break;
	case AbstractCode::Close:
	case AbstractCode::Specialize:
	  stack.PushId(instr->Sel(0));
	  stack.PushInc();
	  stack.PushIdRefs(instr->Sel(1));
	  stack.PushInc();
	  stack.PushInstr(instr->Sel(3));
	  break;
	case AbstractCode::AppPrim:
	  {
	    TagVal *idDefInstrOpt = TagVal::FromWord(instr->Sel(2));
	    if(idDefInstrOpt == INVALID_POINTER) {
	      programPoint++;
	      stack.PushIdRefs(instr->Sel(1));
	    } else {
	      Tuple *idDefInstr = Tuple::FromWordDirect(idDefInstrOpt->Sel(0));
	      stack.PushIdRefs(instr->Sel(1));
	      stack.PushInc();
	      stack.PushIdDef(idDefInstr->Sel(0));
	      stack.PushInc();
	      stack.PushInstr(idDefInstr->Sel(1));
	    }
	  }
	  break;
	case AbstractCode::AppVar:
	  {
	    TagVal *idDefsInstrOpt = TagVal::FromWord(instr->Sel(3));
	    if(idDefsInstrOpt == INVALID_POINTER) {
	      //	      programPoint++;
	      stack.PushIdRef(instr->Sel(0));
	      stack.PushIdRefs(instr->Sel(1));
	      stack.PushInc();
	      stack.PushAppVarPP(instr);
	    } else {
	      Tuple *idDefsInstr = 
		Tuple::FromWordDirect(idDefsInstrOpt->Sel(0));
	      stack.PushIdRef(instr->Sel(0));
	      stack.PushIdRefs(instr->Sel(1));
	      stack.PushInc();
	      stack.PushIdDefs(idDefsInstr->Sel(0));
	      stack.PushAppVarPP(instr);
	      stack.PushInc();
	      stack.PushInstr(idDefsInstr->Sel(1));
	    }
	  }
	  break;
	case AbstractCode::GetRef:
	  stack.PushIdRef(instr->Sel(1));
	  stack.PushInc();
	  stack.PushId(instr->Sel(0));
	  stack.PushInc();
	  stack.PushInstr(instr->Sel(2)); 
	  break;
	case AbstractCode::GetTup:
	  stack.PushIdDefs(instr->Sel(0));
	  stack.PushIdRef(instr->Sel(1));
	  stack.PushInc();
	  stack.PushInstr(instr->Sel(2));
	  break;
	case AbstractCode::Sel:
	  stack.PushIdRef(instr->Sel(1));
	  stack.PushInc();
	  stack.PushId(instr->Sel(0));
	  stack.PushInc();
	  stack.PushInstr(instr->Sel(3)); 
	  break;
	case AbstractCode::LazyPolySel:
	  stack.PushIds(instr->Sel(0));
	  stack.PushInc();
	  stack.PushIdRef(instr->Sel(1));
	  stack.PushInc();
	  stack.PushInstr(instr->Sel(3)); 
	  break;
	case AbstractCode::Raise:
	case AbstractCode::Reraise:
	  programPoint++;
	  stack.PushIdRef(instr->Sel(0));
	  break;
	case AbstractCode::Try:
	  stack.PushInstr(instr->Sel(0));
	  stack.PushIdDef(instr->Sel(1));
	  stack.PushIdDef(instr->Sel(2));
	  stack.PushInc();
	  stack.PushInstr(instr->Sel(3));
	  break;
	case AbstractCode::CompactIntTest:
	  {
	    stack.PushIdRef(instr->Sel(0));
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
	    stack.PushIdRef(instr->Sel(0));
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
	    stack.PushIdRef(instr->Sel(0));
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
	      stack.PushIdDefs(triple->Sel(1));
	      stack.PushInc();
	      stack.PushInstr(triple->Sel(2));
	    }
	    stack.PushInstr(instr->Sel(4));
	  }
	  break;
	case AbstractCode::CompactTagTest:
	  {
	    stack.PushIdRef(instr->Sel(0));
	    stack.PushInc();
	    Vector *tests = Vector::FromWordDirect(instr->Sel(2));
	    u_int nTests = tests->GetLength(); 
	    for(u_int i = 0; i<nTests; i++) {
	      Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
	      TagVal *idDefsOpt = TagVal::FromWord(pair->Sel(0));
	      if(idDefsOpt != INVALID_POINTER) {
		stack.PushIdDefs(idDefsOpt->Sel(0));
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
	    Vector *tests0 = Vector::FromWordDirect(instr->Sel(1));
	    u_int nTests0 = tests0->GetLength(); 
	    Vector *testsN = Vector::FromWordDirect(instr->Sel(2));
	    u_int nTestsN = testsN->GetLength(); 
	    stack.PushIdRef(instr->Sel(0));
	    for(u_int i=0; i<nTests0; i++) {
	      Tuple *pair = Tuple::FromWordDirect(tests0->Sub(i));
	      stack.PushIdRef(pair->Sel(0));
	    }	  
	    for(u_int i=0; i<nTestsN; i++) {
	      Tuple *triple = Tuple::FromWordDirect(testsN->Sub(i));
	      stack.PushIdRef(triple->Sel(0));
	    }
	    stack.PushInc();
	    // compute program points
	    for(u_int i=0; i<nTests0; i++) {
	      Tuple *pair = Tuple::FromWordDirect(tests0->Sub(i));
	      stack.PushInstr(pair->Sel(1));
	    }	  
	    for(u_int i=0; i<nTestsN; i++) {
	      Tuple *triple = Tuple::FromWordDirect(testsN->Sub(i));
	      stack.PushIdDefs(triple->Sel(1));
	      stack.PushInc();
	      stack.PushInstr(triple->Sel(2));
	    }
	    stack.PushInstr(instr->Sel(3));
	  }
	  break;
	case AbstractCode::VecTest:
	  {
	    stack.PushIdRef(instr->Sel(0));
	    stack.PushInc();	  
	    Vector *tests = Vector::FromWordDirect(instr->Sel(1));
	    u_int nTests = tests->GetLength(); 
	    for(u_int i=0; i<nTests; i++) {
	      Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
	      Vector *idDefs = Vector::FromWordDirect(pair->Sel(0));
	      if(idDefs->GetLength() > 0) {
		stack.PushIdDefs(idDefs->ToWord());
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
	    stack.PushIdRefs(instr->Sel(0));
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

// END

Map *ByteCodeInliner::inlineCandidates;

void PrintLiveness(Vector *liveness) {
  u_int size = liveness->GetLength();
  fprintf(stderr,"size = %d\n",size/3);
  for(u_int i = 0, j = 1; i<size; i+=3, j++) {
    u_int index = Store::DirectWordToInt(liveness->Sub(i));
    u_int start = Store::DirectWordToInt(liveness->Sub(i+1));
    u_int end   = Store::DirectWordToInt(liveness->Sub(i+2));
    fprintf(stderr,"%d. %d -> [%d, %d]\n",j,index,start,end);
  }
}

void ByteCodeInliner::InlineAnalyser::Count(TagVal *instr) {
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

void ByteCodeInliner::InlineAnalyser::AnalyseAppVar(TagVal *instr) {
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
	Append(key,instr,acc,closure,inlineInfo);
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
	  inlineInfo = AnalyseInlining(acc);
	  lazyBCC->SetInlineInfo(inlineInfo);
	} else
	  inlineInfo = InlineInfo::FromWordDirect(inlineInfoOpt->Sel(0));
	u_int nNodes = inlineInfo->GetNNodes();
	if(nNodes <= INLINE_LIMIT) {
	  Append(key,instr,lazyBCC->GetAbstractCode(),closure,inlineInfo);
	  // adjust counter
	  counter += nNodes - 1;
	}
      }
    }
  }
}

bool ByteCodeInliner::InlineAnalyser::CheckCycle(TagVal *acc) {
  return inlineCandidates->IsMember(acc->Sel(5));
}

s_int ExtractPP(TagVal *instr, Vector *liveness) {
  TagVal *idDefsInstrOpt = TagVal::FromWord(instr->Sel(3));
  if(idDefsInstrOpt == INVALID_POINTER)
    return -1;
  Tuple *idDefsInstr = Tuple::FromWordDirect(idDefsInstrOpt->Sel(0));
  Vector *fargs = Vector::FromWordDirect(idDefsInstr->Sel(0));
  for(u_int i = 0; i<fargs->GetLength(); i++) {
    TagVal *idDefOpt = TagVal::FromWord(fargs->Sub(i));
    if(idDefOpt != INVALID_POINTER) {
      u_int id = Store::DirectWordToInt(idDefOpt->Sel(0));
      for(u_int j = 0; i<liveness->GetLength(); i+=3) {
	if(Store::DirectWordToInt(liveness->Sub(j)) == id) {
	  return Store::DirectWordToInt(liveness->Sub(j+1));
	}
      }
    }
  }
  return -1;
}

void ByteCodeInliner::InlineAnalyser::Append(word key, TagVal *instr,
					     TagVal *acc, Closure *closure,
					     InlineInfo *inlineInfo) {
  // check if closure was already added
//   if(inlineMap->IsMember(key))
//     return;

  // append liveness
  Vector *calleeLiveness = inlineInfo->GetLiveness();
  word wCalleeLiveness = calleeLiveness->ToWord();
  Tuple *tup = Tuple::New(3);
  tup->Init(0,wCalleeLiveness);
  tup->Init(1,Store::IntToWord(nLocals));
  u_int appVarPP = Store::DirectWordToInt(appVarPPs->Get(instr->ToWord()));
//   s_int cval = ExtractPP(instr,liveness);
//   if(cval != -1 && cval != appVarPP) {
//     fprintf(stderr,"ERROR: cval %d != appVarPP %d\n",cval,appVarPP);
//     appVarPP = cval;
//   } else if( cval == appVarPP) {
//     fprintf(stderr,"program points match\n");
//   }
  tup->Init(2,Store::IntToWord(appVarPP));
  livenessInfo.Append(appVarPP,tup->ToWord(),calleeLiveness->GetLength());
  u_int offset = livenessInfo.GetFlattenedLength() / 3;
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

Vector *ByteCodeInliner::InlineAnalyser::MergeLiveness() {
  u_int size = livenessInfo.GetLength();
  if(size == 0) // nothing can be inlined
    return liveness;
  u_int flattenedSize = livenessInfo.GetFlattenedLength();
  Vector *newLiveness = Vector::New(liveness->GetLength() + flattenedSize);
  u_int index = flattenedSize;
  u_int maxEndPoint = 0;
  // copy the caller intervals
  for(u_int i=0; i<liveness->GetLength(); i+=3, index+=3) {
    newLiveness->Init(index,liveness->Sub(i));
    newLiveness->Init(index+1,liveness->Sub(i+1));
    newLiveness->Init(index+2,liveness->Sub(i+2));
    u_int endPoint = Store::DirectWordToInt(liveness->Sub(i+2));
    if(maxEndPoint < endPoint) maxEndPoint = endPoint;	
  }
  // copy intervals of the inlinable functions
  index = 0;
  for(u_int i=0; i<size; i++) {
    u_int offset = maxEndPoint + 1; // intervals mustn't overlap
    Tuple *tup = Tuple::FromWordDirect(livenessInfo.Sub(i));
    Vector *calleeLiveness = Vector::FromWordDirect(tup->Sel(0));
    u_int numOffset = Store::WordToInt(tup->Sel(1));
    for(u_int j=0; j<calleeLiveness->GetLength(); j+=3, index+=3) {
      u_int num = 
	Store::DirectWordToInt(calleeLiveness->Sub(j)) + numOffset;      
      u_int startPoint = 0; // hack <-- look for better solution
      u_int endPoint = 
	Store::DirectWordToInt(calleeLiveness->Sub(j+2)) + offset;      
      if(maxEndPoint < endPoint) maxEndPoint = endPoint;
      newLiveness->Init(index,Store::IntToWord(num));
      newLiveness->Init(index+1,Store::IntToWord(startPoint));
      newLiveness->Init(index+2,Store::IntToWord(endPoint));
    }
  }
  return newLiveness;  
}


// Vector *ByteCodeInliner::InlineAnalyser::MergeLiveness() {
//   u_int size = livenessInfo.GetLength();
//   if(size == 0) // nothing can be inlined
//     return liveness;
//   // copy intervals of the inlinable functions
//   u_int offset = 0;
//   u_int offsetTable[callerMaxPP];
//   std::memset(offsetTable,0,callerMaxPP*sizeof(u_int));
//   u_int l1Length = livenessInfo.GetFlattenedLength();
//   u_int liveness1[l1Length];
//   livenessInfo.Sort();
//   for(u_int i=0, index=0; i<size; i++) {
//     Tuple *tup = Tuple::FromWordDirect(livenessInfo.Pop());
//     Vector *calleeLiveness = Vector::FromWordDirect(tup->Sel(0));
//     u_int idOffset = Store::WordToInt(tup->Sel(1));
//     u_int appVarPP = Store::DirectWordToInt(tup->Sel(2));
//     u_int maxEndPoint = 0;
// //     fprintf(stderr,"%d. calleeLiveness, appVarPP %d:\n",i,appVarPP);
// //     PrintLiveness(calleeLiveness);
// //     fprintf(stderr,"offset %d, appVarPP %d\n",offset,appVarPP);
//     for(u_int j=0; j<calleeLiveness->GetLength(); j+=3) {
//       u_int identifier =
// 	Store::DirectWordToInt(calleeLiveness->Sub(j)) + idOffset;      
//       u_int startPoint =
// 	Store::DirectWordToInt(calleeLiveness->Sub(j+1)) + offset + appVarPP; 
//       u_int endPoint = Store::DirectWordToInt(calleeLiveness->Sub(j+2));      
//       if(maxEndPoint < endPoint) 
// 	maxEndPoint = endPoint;
//       endPoint += offset + appVarPP;
//       liveness1[index++] = identifier;
//       liveness1[index++] = startPoint;
//       liveness1[index++] = endPoint; 
//     }    
//     offsetTable[appVarPP] = maxEndPoint+1; // + 1 ???
//     offset += maxEndPoint + 1;
// //     fprintf(stderr,"offset %d, maxEndPoint %d\n",offset,maxEndPoint);
//   }
//   // compute offsets for caller intervals
// //   fprintf(stderr,"compute offsets:\n");
//   for(u_int i=1; i<callerMaxPP; i++) {
//     offsetTable[i] += offsetTable[i-1];
// //     fprintf(stderr,"pp %d -> %d\n",i,offsetTable[i]);
//   }
//   // copy the caller intervals
//   u_int l2Length = liveness->GetLength();
//   u_int liveness2[l2Length];
//   for(u_int i=0, j=0; i<l2Length; i+=3, j+=3) {
//     u_int identifier = Store::DirectWordToInt(liveness->Sub(i));
//     u_int startPoint = Store::DirectWordToInt(liveness->Sub(i+1));
//     u_int endPoint = Store::DirectWordToInt(liveness->Sub(i+2));
//     liveness2[j] = identifier;
//     liveness2[j+1] = startPoint + offsetTable[startPoint];
//     liveness2[j+2] = endPoint + offsetTable[endPoint];
//   }
//   // merge the adjusted the liveness arrays
//   Vector *newLiveness = Vector::New(l1Length + l2Length);
//   u_int i1 = 0, i2 = 0, i3 = 0;
//   while( i1 < l1Length && i2 < l2Length ) {
//     if(liveness1[i1+1] < liveness2[i2+1]) {
//       newLiveness->Init(i3++,Store::IntToWord(liveness1[i1++]));
//       newLiveness->Init(i3++,Store::IntToWord(liveness1[i1++]));
//       newLiveness->Init(i3++,Store::IntToWord(liveness1[i1++]));
//     } else {
//       newLiveness->Init(i3++,Store::IntToWord(liveness2[i2++]));
//       newLiveness->Init(i3++,Store::IntToWord(liveness2[i2++]));
//       newLiveness->Init(i3++,Store::IntToWord(liveness2[i2++]));
//     }
//   }
//   while(i1 < l1Length) {
//     newLiveness->Init(i3++,Store::IntToWord(liveness1[i1++]));
//     newLiveness->Init(i3++,Store::IntToWord(liveness1[i1++]));
//     newLiveness->Init(i3++,Store::IntToWord(liveness1[i1++]));
//   }
//   while(i2 < l2Length) {
//     newLiveness->Init(i3++,Store::IntToWord(liveness2[i2++]));
//     newLiveness->Init(i3++,Store::IntToWord(liveness2[i2++]));
//     newLiveness->Init(i3++,Store::IntToWord(liveness2[i2++]));
//   }
// //   fprintf(stderr,"liveness1:\n");
// //   for(u_int i=0; i<l1Length; i+=3)
// //     fprintf(stderr," %d -> [%d,%d]\n",liveness1[i],liveness1[i+1],liveness1[i+2]);
// //   fprintf(stderr,"\n");
// //   fprintf(stderr,"newLiveness:\n");
// //   PrintLiveness(newLiveness);    
// //   u_int oldStart = 0;
// //   for(u_int i=1; i<l1Length+l2Length; i+=3) {
// //     u_int start = Store::DirectWordToInt(newLiveness->Sub(i));
// //     if(oldStart > start) {
// //       fprintf(stderr,"liveness interval is NOT sorted!\n");
// //     }
// //     oldStart = start;
// //   }
//   return newLiveness;  
// }

class DriverStack {
private:
  TagVal **stack;
  s_int top, size;
public:
  DriverStack(u_int s = 100) : top(-1), size(s) {
    stack = new TagVal*[size];
  }
  ~DriverStack() {
    delete[] stack;
  }
  void Push(TagVal *v) {
    if(v == INVALID_POINTER)
      return;
    if( ++top >= size ) {
      size = size * 3 / 2;
      TagVal **newStack = new TagVal*[size];
      memcpy(newStack,stack,size * sizeof(TagVal*));
      delete[] stack;
      stack = newStack;
    }
    stack[top] = v;
  }
  TagVal *Top() {
    return stack[top];
  }
  TagVal *Pop() {
    return stack[top--];
  }
  bool Empty() {
    return (top < 0);
  }
};

void ByteCodeInliner::Driver(TagVal *instr, InlineAnalyser *analyser) {
  DriverStack stack;
  IntMap *stamps = IntMap::New(100); // remember control flow merge points
  stack.Push(instr);
  while( !stack.Empty() ) {
    instr = stack.Pop();
    analyser->Count(instr);
    switch (AbstractCode::GetInstr(instr)) {
    case AbstractCode::Raise:
    case AbstractCode::Reraise:
    case AbstractCode::Return:
      break;
    case AbstractCode::EndHandle:
    case AbstractCode::EndTry:
      stack.Push(TagVal::FromWordDirect(instr->Sel(0))); 
      break;
    case AbstractCode::Kill:
      stack.Push(TagVal::FromWordDirect(instr->Sel(1)));
      break;
    case AbstractCode::GetRef:
    case AbstractCode::GetTup:
    case AbstractCode::PutNew:
    case AbstractCode::PutRef:
    case AbstractCode::PutTup:
    case AbstractCode::PutVar:
    case AbstractCode::PutVec:
      stack.Push(TagVal::FromWordDirect(instr->Sel(2))); 
      break;
    case AbstractCode::Close:
    case AbstractCode::LazyPolySel:
    case AbstractCode::PutCon:
    case AbstractCode::PutPolyRec:
    case AbstractCode::Sel: 
    case AbstractCode::Specialize:
      stack.Push(TagVal::FromWordDirect(instr->Sel(3))); 
      break;
    case AbstractCode::PutTag:
      stack.Push(TagVal::FromWordDirect(instr->Sel(4))); 
      break;
    case AbstractCode::AppPrim:
      {
	TagVal *contOpt = TagVal::FromWord(instr->Sel(2));
	if(contOpt != INVALID_POINTER) {
	  Tuple *tup = Tuple::FromWordDirect(contOpt->Sel(0));
	  stack.Push(TagVal::FromWordDirect(tup->Sel(1)));
	}
      }
      break;
    case AbstractCode::AppVar:
      {
	analyser->AnalyseAppVar(instr);
	TagVal *contOpt = TagVal::FromWord(instr->Sel(3));
	if(contOpt != INVALID_POINTER) {
	  Tuple *tup = Tuple::FromWordDirect(contOpt->Sel(0));
	  stack.Push(TagVal::FromWordDirect(tup->Sel(1)));
	}
      }
      break;
    case AbstractCode::Try:
      stack.Push(TagVal::FromWordDirect(instr->Sel(3)));
      stack.Push(TagVal::FromWordDirect(instr->Sel(0)));
      break;
    case AbstractCode::IntTest:
    case AbstractCode::RealTest:
    case AbstractCode::StringTest:
    case AbstractCode::VecTest:
      {
	stack.Push(TagVal::FromWordDirect(instr->Sel(2)));
	Vector *tests = Vector::FromWordDirect(instr->Sel(1));
	for (u_int i = tests->GetLength(); i--; ) {
	  Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
	  stack.Push(TagVal::FromWordDirect(pair->Sel(1)));
	}
      }
      break;
    case AbstractCode::CompactIntTest:
      {
	stack.Push(TagVal::FromWordDirect(instr->Sel(3)));
	Vector *tests = Vector::FromWordDirect(instr->Sel(2));
	for (u_int i = tests->GetLength(); i--; ) {
	  stack.Push(TagVal::FromWordDirect(tests->Sub(i)));
	}	
      }
      break;
    case AbstractCode::ConTest:
      {
	stack.Push(TagVal::FromWordDirect(instr->Sel(3)));
	Vector *testsN = Vector::FromWordDirect(instr->Sel(2));
	for(u_int i = testsN->GetLength(); i--; ) {
	  Tuple *triple = Tuple::FromWordDirect(testsN->Sub(i));
	  stack.Push(TagVal::FromWordDirect(triple->Sel(2)));
	}
	Vector *tests0 = Vector::FromWordDirect(instr->Sel(1));
	for(u_int i = tests0->GetLength(); i--; ) {
	  Tuple *pair = Tuple::FromWordDirect(tests0->Sub(i));	  
	  stack.Push(TagVal::FromWordDirect(pair->Sel(1)));
	}
      }
      break;
    case AbstractCode::TagTest:
      {
	stack.Push(TagVal::FromWordDirect(instr->Sel(4)));
	Vector *testsN = Vector::FromWordDirect(instr->Sel(3));
	for(u_int i = testsN->GetLength(); i--; ) {
	  Tuple *triple = Tuple::FromWordDirect(testsN->Sub(i));
	  stack.Push(TagVal::FromWordDirect(triple->Sel(2)));
	}
	Vector *tests0 = Vector::FromWordDirect(instr->Sel(2));
	for(u_int i = tests0->GetLength(); i--; ) {
	  Tuple *pair = Tuple::FromWordDirect(tests0->Sub(i));	  
	  stack.Push(TagVal::FromWordDirect(pair->Sel(1)));
	}
      }
      break;
    case AbstractCode::CompactTagTest:
      {
	TagVal *elseInstrOpt = TagVal::FromWord(instr->Sel(3));
	if(elseInstrOpt != INVALID_POINTER)
	  stack.Push(TagVal::FromWordDirect(elseInstrOpt->Sel(0)));
	Vector *tests = Vector::FromWordDirect(instr->Sel(2));
	for(u_int i = tests->GetLength(); i--; ) {
	  Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
	  stack.Push(TagVal::FromWordDirect(pair->Sel(1)));
	}	
      }
      break;
    case AbstractCode::Shared:
      {
	word stamp = instr->Sel(0);
	if(!stamps->IsMember(stamp)) {
	  stamps->Put(stamp,Store::IntToWord(0)); 
	  stack.Push(TagVal::FromWordDirect(instr->Sel(1)));
	}
      }
      break;
    default:
      fprintf(stderr,"ByteCodeInliner::Driver: invalid abstractCode tag %d\n",
	      (u_int)AbstractCode::GetInstr(instr));
      return;
    }
  }
}

InlineInfo *ByteCodeInliner::AnalyseInlining(TagVal *abstractCode) {
//   Tuple *coord = Tuple::FromWordDirect(abstractCode->Sel(0));
//   std::fprintf(stderr, "analyse inlining for %p %s:%d.%d, nLocals %d\n",
// 	       abstractCode,
// 	       String::FromWordDirect(coord->Sel(0))->ExportC(),
// 	       Store::DirectWordToInt(coord->Sel(1)),
// 	       Store::DirectWordToInt(coord->Sel(2)),
// 	       GetNumberOfLocals(abstractCode)); 
//   AbstractCode::Disassemble(stderr,
// 			    TagVal::FromWordDirect(abstractCode->Sel(5)));

  Tuple *triple = LivenessAnalyser::ComputeLiveness(abstractCode);
  Map *appVarPPs = Map::FromWordDirect(triple->Sel(1));
  u_int maxPP = Store::DirectWordToInt(triple->Sel(2));

//     Vector *liveness2 = LivenessAnalyser::ComputeLiveness(abstractCode);
//     Vector *liveness1 = Vector::FromWordDirect(abstractCode->Sel(6));
//   if(!LivenessAnalyser::Check(liveness1,liveness2)) {
//     fprintf(stderr,"original liveness:\n");
//     PrintLiveness(liveness1);
//     fprintf(stderr,"computed liveness:\n");
//     PrintLiveness(liveness2);
//     fprintf(stderr,"\n\n");
//   }
  
  InlineAnalyser inliner(abstractCode,appVarPPs,maxPP);
  inlineCandidates->Put(abstractCode->Sel(5),Store::IntToWord(0));  
  Driver(TagVal::FromWordDirect(abstractCode->Sel(5)),&inliner);
  inlineCandidates->Remove(abstractCode->ToWord());
  return inliner.ComputeInlineInfo();
}
