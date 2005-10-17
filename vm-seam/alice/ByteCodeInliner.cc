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

#define INLINE_LIMIT 4

using namespace ByteCodeInliner_Internal;

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
  case AbstractCode::Close:
    // Avoid to inline Close instruction as this will
    // increase the number of compiler calls 
    // significantly and can cause the system to diverge.
    counter += INLINE_LIMIT + 1;
    break;
  default:
    counter++; 
  }
}

void ByteCodeInliner::InlineAnalyser::AnalyseAppVar(TagVal *instr) {
  Assert(instr != INVALID_POINTER);
  Assert(AbstractCode::GetInstr(instr) == AbstractCode::AppVar);
  TagVal *idRef = TagVal::FromWordDirect(instr->Sel(0));
  Closure *closure = INVALID_POINTER;
  if(AbstractCode::GetIdRef(idRef) == AbstractCode::Global) {
    u_int index = Store::DirectWordToInt(idRef->Sel(0));
    TagVal *valueOpt = TagVal::FromWord(subst->Sub(index));
    if (valueOpt != INVALID_POINTER) 
      closure = Closure::FromWord(valueOpt->Sel(0));
  } else if (AbstractCode::GetIdRef(idRef) == AbstractCode::Immediate) {
    closure = Closure::FromWord(idRef->Sel(0));       
  } else {
    return;
  }
  // analyse closure
  if(closure != INVALID_POINTER) {
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
	  Append(acc,closure,instr->Sel(3),inlineInfo);
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
	    Append(lazyBCC->GetAbstractCode(),closure,instr->Sel(3),
		   inlineInfo);
	    // adjust counter
 	    counter += nNodes - 1;
	  }
	}
      }
    }
  }
}

bool ByteCodeInliner::InlineAnalyser::CheckCycle(TagVal *acc) {
  return inlineCandidates->IsMember(acc->ToWord());
}

void ByteCodeInliner::InlineAnalyser::Append(TagVal *acc, Closure *closure,
					     word idDefsInstrOpt,
					     InlineInfo *inlineInfo) {
  // append liveness
  Vector *calleeLiveness = inlineInfo->GetLiveness();
  word wCalleeLiveness = calleeLiveness->ToWord();
  Tuple *tup = Tuple::New(3);
  tup->Init(0,wCalleeLiveness);
  tup->Init(1,Store::IntToWord(nLocals));
  tup->Init(2,idDefsInstrOpt);
  livenessInfo.Append(tup->ToWord(),calleeLiveness->GetLength());
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
  inlineMap->Put(closure->ToWord(),info->ToWord());
  // add number of locals
  nLocals += inlineInfo->GetNLocals();
}

// simple version: just append and lift the inlined function
// TODO: investigate a better solution for non-tailcalls
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

  InlineAnalyser inliner(abstractCode);
  inlineCandidates->Put(abstractCode->ToWord(),Store::IntToWord(0));
  Driver(TagVal::FromWordDirect(abstractCode->Sel(5)),&inliner); // start from root
  inlineCandidates->Remove(abstractCode->ToWord());
  return inliner.ComputeInlineInfo();
}
