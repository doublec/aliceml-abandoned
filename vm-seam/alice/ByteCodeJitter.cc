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
#pragma implementation "alice/ByteCodeJitter.hh"
#endif

#include "alice/PrimitiveTable.hh"
#include "alice/ByteCodeJitter.hh"
#include "alice/ByteConcreteCode.hh"
#include "alice/AbstractCode.hh"
#include "alice/ByteCodeAlign.hh"
#include "alice/ByteCodeBuffer.hh"
#include "alice/ByteCode.hh"

#define BCJIT_DEBUG(s,...) /* fprintf(stderr,s, ##__VA_ARGS__) */

#define RELATIVE_JUMP

using namespace ByteCodeInstr;

static inline u_int GetNumberOfLocals(TagVal *abstractCode) {
  TagVal *annotation = TagVal::FromWordDirect(abstractCode->Sel(2));
  switch (AbstractCode::GetAnnotation(annotation)) {
  case AbstractCode::Simple:
      return Store::DirectWordToInt(annotation->Sel(0));
  case AbstractCode::Debug:
      return Vector::FromWordDirect(annotation->Sel(0))->GetLength();
  }
}

static inline word ExtractImmediate(word idRef) {
  TagVal *tagVal = TagVal::FromWordDirect(idRef);
  switch (AbstractCode::GetIdRef(tagVal)) {
  case AbstractCode::Immediate:
    return tagVal->Sel(0);
  default:
    return INVALID_POINTER;		       
  }
}

// Register Allocation
#ifdef DO_REG_ALLOC
// Register Allocation
class RegisterAllocator {
private:
  class Info {
  public:
    u_int reg;
    u_int start;
    u_int end;
    Info(u_int r,u_int s, u_int e) : reg(r), start(s), end(e) {}
    u_int GetKey() { return end; }
  };

  class MinHeap {
  private:
    Info **heap;
    u_int top;
    u_int size;
  public:
    MinHeap(u_int s) : top(0), size(s+1) {
      heap = new Info*[size];
    }
    ~MinHeap() {
      for(u_int i=1; i<=top; i++)
	delete heap[i];
      delete[] heap;
    }
   Info* GetMin() {
      return heap[1];
    }
    void DeleteMin() {
      heap[1] = heap[top--];
      Info *tmp;
      u_int i = 1, left, right;
      u_int smallest = 0;
      for(;;) {
	left = i << 1;
	right = left | 1;
	// choose max(i,left,right)
	if(left <= top && heap[left]->GetKey() < heap[i]->GetKey()) 
	  smallest = left;
	else
	  smallest = i;
	if(right <= top && heap[right]->GetKey() < heap[smallest]->GetKey()) 
	  smallest = right;
	if(smallest == i) return;
	// push the smallest element upwards
	tmp = heap[smallest];
	heap[smallest] = heap[i];
	heap[i] = tmp;
	i = smallest;
      }
    }
    void Insert(Info *item) {
      heap[++top] = item;
      Info *tmp;
      u_int i = top;
      u_int parent = i >> 1;
      while(i > 1 && heap[parent]->GetKey() > heap[i]->GetKey()) {
	tmp = heap[parent];
	heap[parent] = heap[i];
	heap[i] = tmp;
	i = parent;
	parent >>= 1;
      }
    }
  };

public:
  static void Run(Vector *liveness, u_int mapping[], u_int *nLocals) {
    MinHeap heap(*nLocals);
    u_int regs = 0;
    u_int size = liveness->GetLength();
    for(u_int i=0; i<size; i+=3) {
      u_int index = Store::DirectWordToInt(liveness->Sub(i));
      u_int start = Store::DirectWordToInt(liveness->Sub(i+1));
      u_int end   = Store::DirectWordToInt(liveness->Sub(i+2));
      Info *info = new Info(regs,start,end);
      heap.Insert(info);
      Info *min = heap.GetMin();
      if(min->end < start) {
	mapping[index] = min->reg;
	info->reg = min->reg;
	heap.DeleteMin();
      } else {
	mapping[index] = regs++;
      }
    }
    *nLocals = regs;
  }
};
#endif // DO_REG_ALLOC

// instruction helpers
u_int ByteCodeJitter::LoadIdRefKill(word idRef, bool keepScratch = false) {
  TagVal *tagVal = TagVal::FromWordDirect(idRef);
    
  if (AbstractCode::GetIdRef(tagVal) == AbstractCode::Global)
    tagVal = LookupSubst(Store::DirectWordToInt(tagVal->Sel(0)));

  switch (AbstractCode::GetIdRef(tagVal)) {
  case AbstractCode::LastUseLocal: // TODO: include liveness table
  case AbstractCode::Local:
    {    
      return IdToReg(tagVal->Sel(0));
    }
  case AbstractCode::Global:
    {
      u_int S;
      if(keepScratch) {
	S = scratch;
	delayedScratchInc = 1;
      } else {
	S = GetNewScratch();    
      }
      SET_INSTR_1R1I(PC,load_global,S,Store::DirectWordToInt(tagVal->Sel(0)));
      return S;
    }
  case AbstractCode::Immediate:
    {
      u_int S;
      if(keepScratch) {
	S = scratch;
	delayedScratchInc = 1;
      } else {
	S = GetNewScratch();    
      }
      word val = tagVal->Sel(0);
      if (PointerOp::IsInt(val)) {
	s_int x = Store::DirectWordToInt(val);
	if(x == 0) {
	  SET_INSTR_1R(PC,load_zero,S);
	} else {
	  SET_INSTR_1R1I(PC,load_int,S,x);
	}
      }
      else {
	u_int index = imEnv.Register(val);
	SET_INSTR_1R1I(PC,load_immediate,S,index);
      }
      return S;
    }
  default:
    Error("ByteCodeJitter::LoadIdRef: invalid idRef Tag");
  }

  return INVALID_INT;
}

ByteCodeJitter::ByteCodeJitter() {
}

ByteCodeJitter::~ByteCodeJitter() {
}

void *ByteCodeJitter::inlineTable[INLINE_TABLE_SIZE];

void ByteCodeJitter::Init() {
  LazyByteCompileInterpreter::Init();

  // prepare inlineTable
#define INIT_INLINE_TABLE(primitiveName,primitiveNumber) {		\
  Chunk *name = (Chunk *) (String::New(primitiveName));			\
  word value = PrimitiveTable::LookupValue(name);			\
  Closure *closure = Closure::FromWordDirect(value);			\
  ConcreteCode *concreteCode =						\
    ConcreteCode::FromWord(closure->GetConcreteCode());			\
  Interpreter *interpreter = concreteCode->GetInterpreter();		\
  inlineTable[primitiveNumber] = (void *) interpreter->GetCFunction();	\
}

  INIT_INLINE_TABLE("Int.+",INT_PLUS);
  INIT_INLINE_TABLE("Int.-",INT_MINUS);
  INIT_INLINE_TABLE("Ref.:=",REF_ASSIGN);
  INIT_INLINE_TABLE("Future.await",FUTURE_AWAIT);
  INIT_INLINE_TABLE("Future.byneed",FUTURE_BYNEED);
  INIT_INLINE_TABLE("Hole.hole",HOLE_HOLE);
  INIT_INLINE_TABLE("Hole.fill",HOLE_FILL);
}

// helpers to inline common primitives

inline TagVal *ByteCodeJitter::Inline_HoleHole(Vector *args, 
					       TagVal *idDefInstrOpt) {
  BCJIT_DEBUG("inline Hole.hole\n");
  if(args->GetLength() == 1) { // request unit      
    SET_INSTR_1R(PC,await,GetNewScratch());
  }
  if(idDefInstrOpt == INVALID_POINTER) { // tailcall
    u_int S = GetNewScratch();
    SET_INSTR_1R(PC,inlined_hole_hole,S);
    SET_INSTR_1R(PC,seam_return1,S);
    return INVALID_POINTER;
  }
  Tuple *idDefInstr = Tuple::FromWordDirect(idDefInstrOpt->Sel(0));
  TagVal *idDef = TagVal::FromWord(idDefInstr->Sel(0));
  if(idDef != INVALID_POINTER) {
    u_int dst = IdToReg(idDef->Sel(0));
    SET_INSTR_1R(PC,inlined_hole_hole,dst);
  }
  return TagVal::FromWordDirect(idDefInstr->Sel(1)); 
}

inline TagVal *ByteCodeJitter::Inline_HoleFill(Vector *args, 
					       TagVal *idDefInstrOpt) {
  BCJIT_DEBUG("inline Future.fill\n");
  u_int arg0, arg1;
  // check which primitives also needs a CCC (in principle)
  switch(args->GetLength()) {
  case 0:
    Error("Hole.fill got 0 args");
    break;
  case 1: // deconstruct the tuple
    {
      u_int tuple = LoadIdRefKill(args->Sub(0));
      arg0 = GetNewScratch();
      arg1 = GetNewScratch();
      SET_INSTR_2R(PC,select_tup0,arg0,tuple);
      SET_INSTR_2R(PC,select_tup1,arg1,tuple);
    }
    break;
  case 2:
    {
      arg0 = LoadIdRefKill(args->Sub(0));
      arg1 = LoadIdRefKill(args->Sub(1));
    }
    break;
  default:
    Error("Hole.fill got more than 2 args");
  }
  SET_INSTR_2R(PC,inlined_hole_fill,arg0,arg1);
  if(idDefInstrOpt == INVALID_POINTER) { // tailcall --> return unit
    u_int S = GetNewScratch();
    SET_INSTR_1R(PC,load_zero,S);
    SET_INSTR_1R(PC,seam_return1,S);
    return INVALID_POINTER;
  } 
  Tuple *idDefInstr = Tuple::FromWordDirect(idDefInstrOpt->Sel(0));
  TagVal *idDef = TagVal::FromWord(idDefInstr->Sel(0));
  if(idDef != INVALID_POINTER) {
    u_int ret = IdToReg(idDef->Sel(0));
    SET_INSTR_1R(PC,load_zero,ret);
  }
  return TagVal::FromWordDirect(idDefInstr->Sel(1));     
}

inline TagVal *ByteCodeJitter::Inline_FutureAwait(Vector *args, 
						  TagVal *idDefInstrOpt) {
  BCJIT_DEBUG("inline Future.await\n");
  u_int arg = LoadIdRefKill(args->Sub(0));
  SET_INSTR_1R(PC,await,arg);
  if(idDefInstrOpt == INVALID_POINTER) {
    SET_INSTR_1R(PC,seam_return1,arg);
      return INVALID_POINTER;
  }
  Tuple *idDefInstr = Tuple::FromWordDirect(idDefInstrOpt->Sel(0));
  TagVal *ret = TagVal::FromWord(idDefInstr->Sel(0));
  if(ret != INVALID_POINTER) {
    u_int dst = IdToReg(ret->Sel(0));
    if(dst != arg)
      SET_INSTR_2R(PC,load_reg,dst,arg);
  }
  return TagVal::FromWordDirect(idDefInstr->Sel(1)); 
}

inline TagVal *ByteCodeJitter::Inline_FutureByneed(Vector *args, 
						   TagVal *idDefInstrOpt) {
  BCJIT_DEBUG("inline Future.byneed\n");
  if(args->GetLength() != 1) { // create a tuple and pass this
    Error("Uups! BCJitter: Future.byneed didn't get 1 arg\n");
  }
  u_int src = LoadIdRefKill(args->Sub(0));
  if(idDefInstrOpt == INVALID_POINTER) { // tailcall
    u_int S = GetNewScratch();
    SET_INSTR_2R(PC,inlined_future_byneed,S,src);
    SET_INSTR_1R(PC,seam_return1,S);
    return INVALID_POINTER;
    } 
  Tuple *idDefInstr = Tuple::FromWordDirect(idDefInstrOpt->Sel(0));
  TagVal *idDef = TagVal::FromWord(idDefInstr->Sel(0));
  if(idDef != INVALID_POINTER) {
    u_int dst = IdToReg(idDef->Sel(0));
    SET_INSTR_2R(PC,inlined_future_byneed,dst,src);
  }
  return TagVal::FromWordDirect(idDefInstr->Sel(1)); 
}


inline TagVal *ByteCodeJitter::Inline_IntPlus(Vector *args, 
					      TagVal *idDefInstrOpt) {
  // TODO: we might need a CCC
  
  // try first to compile the special case
  word wY = ExtractImmediate(args->Sub(1));
  if(wY != INVALID_POINTER && Store::DirectWordToInt(wY) == 1) {
    u_int x = LoadIdRefKill(args->Sub(0));
    if(idDefInstrOpt == INVALID_POINTER) { // tailcall
      SET_INSTR_1R(PC,iinc,x);
      SET_INSTR_1R(PC,seam_return1,x);
      return INVALID_POINTER;
    } 
    Tuple *idDefInstr = Tuple::FromWord(idDefInstrOpt->Sel(0));
    TagVal *ret0 = TagVal::FromWord(idDefInstr->Sel(0));
    if(ret0 != INVALID_POINTER && IdToReg(ret0->Sel(0)) == x) {
      SET_INSTR_1R(PC,iinc,x);
      return TagVal::FromWordDirect(idDefInstr->Sel(1)); 
    }    
  }

  word wX = ExtractImmediate(args->Sub(0));
  if(wX != INVALID_POINTER && Store::DirectWordToInt(wX) == 1) {
    u_int y = LoadIdRefKill(args->Sub(1));
    if(idDefInstrOpt == INVALID_POINTER) { // tailcall
      SET_INSTR_1R(PC,iinc,y);
      SET_INSTR_1R(PC,seam_return1,y);
      return INVALID_POINTER;
    } 
    Tuple *idDefInstr = Tuple::FromWord(idDefInstrOpt->Sel(0));
    TagVal *ret0 = TagVal::FromWord(idDefInstr->Sel(0));
    if(ret0 != INVALID_POINTER && IdToReg(ret0->Sel(0)) == y) {
      SET_INSTR_1R(PC,iinc,y);
      return TagVal::FromWordDirect(idDefInstr->Sel(1)); 
    }
    // else: inline iadd
  }
  
  // normal case
  u_int x = LoadIdRefKill(args->Sub(0));
  u_int y = LoadIdRefKill(args->Sub(1));
  if(idDefInstrOpt == INVALID_POINTER) { // tailcall
    u_int S = GetNewScratch();
    SET_INSTR_3R(PC,iadd,S,x,y);
    SET_INSTR_1R(PC,seam_return1,S);
    return INVALID_POINTER;
  } 
  Tuple *idDefInstr = Tuple::FromWord(idDefInstrOpt->Sel(0));
  TagVal *ret0 = TagVal::FromWord(idDefInstr->Sel(0));
  u_int dst = (ret0 == INVALID_POINTER) ? 
    GetNewScratch() : IdToReg(ret0->Sel(0));
  SET_INSTR_3R(PC,iadd,dst,x,y);
  return TagVal::FromWordDirect(idDefInstr->Sel(1)); 
}

inline TagVal *ByteCodeJitter::Inline_IntMinus(Vector *args, 
					       TagVal *idDefInstrOpt) {
  // TODO: we might need a CCC

  u_int x = LoadIdRefKill(args->Sub(0));
  word wY = ExtractImmediate(args->Sub(1));

  // try first to compile the special case
  if(wY != INVALID_POINTER && Store::DirectWordToInt(wY) == 1) {
    if(idDefInstrOpt == INVALID_POINTER) { // tailcall
      SET_INSTR_1R(PC,idec,x);
      SET_INSTR_1R(PC,seam_return1,x);
      return INVALID_POINTER;
    } 
    Tuple *idDefInstr = Tuple::FromWord(idDefInstrOpt->Sel(0));
    TagVal *ret0 = TagVal::FromWord(idDefInstr->Sel(0));
    if(ret0 != INVALID_POINTER && IdToReg(ret0->Sel(0)) == x) {
      SET_INSTR_1R(PC,idec,x);
      return TagVal::FromWordDirect(idDefInstr->Sel(1)); 
    }
    // else: inline iadd
  }
  
  // normal case
  u_int y = LoadIdRefKill(args->Sub(1));
  if(idDefInstrOpt == INVALID_POINTER) { // tailcall
    u_int S = GetNewScratch();
    SET_INSTR_3R(PC,isub,S,x,y);
    SET_INSTR_1R(PC,seam_return1,S);
    return INVALID_POINTER;
  } 
  Tuple *idDefInstr = Tuple::FromWordDirect(idDefInstrOpt->Sel(0));
  TagVal *ret0 = TagVal::FromWord(idDefInstr->Sel(0));
  u_int dst = (ret0 == INVALID_POINTER) ? 
    GetNewScratch() : IdToReg(ret0->Sel(0));
  SET_INSTR_3R(PC,isub,dst,x,y);
  return TagVal::FromWordDirect(idDefInstr->Sel(1)); 
}

inline TagVal *ByteCodeJitter::Inline_RefAssign(Vector *args, 
						TagVal *idDefInstrOpt) {
  // TODO: we might need a CCC
  u_int ref = LoadIdRefKill(args->Sub(0));
  u_int val = LoadIdRefKill(args->Sub(1));
  SET_INSTR_2R(PC,set_cell,ref,val);
  if(idDefInstrOpt == INVALID_POINTER) { // tailcall
    u_int S = GetNewScratch();
    SET_INSTR_1R(PC,load_zero,S);
    SET_INSTR_1R(PC,seam_return1,S); // return unit
    return INVALID_POINTER;
  }
  Tuple *idDefInstr = Tuple::FromWordDirect(idDefInstrOpt->Sel(0));
  return TagVal::FromWordDirect(idDefInstr->Sel(1)); 
}

bool ByteCodeJitter::InlinePrimitive(void *cFunction, 
				     Vector *args, 
				     TagVal *idDefInstrOpt, 
				     TagVal **continuation) {
  if(cFunction == inlineTable[HOLE_HOLE]) { 
    *continuation = Inline_HoleHole(args,idDefInstrOpt);
    return true;
  } 
  if(cFunction == inlineTable[HOLE_FILL]) {
    *continuation = Inline_HoleFill(args,idDefInstrOpt);
    return true;
  }
  if(cFunction == inlineTable[FUTURE_AWAIT]) {
    *continuation = Inline_FutureAwait(args,idDefInstrOpt);
    return true;
  }
  if(cFunction == inlineTable[FUTURE_BYNEED]) {
    *continuation = Inline_FutureByneed(args,idDefInstrOpt);
    return true;
  }
  if(cFunction == inlineTable[INT_PLUS]) {
    *continuation = Inline_IntPlus(args,idDefInstrOpt);
    return true;
  }
  if(cFunction == inlineTable[INT_MINUS]) {
    *continuation = Inline_IntMinus(args,idDefInstrOpt);
    return true;
  }
  if(cFunction == inlineTable[REF_ASSIGN]) {
    *continuation = Inline_RefAssign(args,idDefInstrOpt);
    return true;
  }
  return false;
}

// AbstractCode Instructions

// Kill of id vector * instr
inline TagVal *ByteCodeJitter::InstrKill(TagVal *pc) {
  // use this information in later versions of the compiler
  return TagVal::FromWordDirect(pc->Sel(1));
}
 
// PutVar of id * idRef  * instr
inline TagVal *ByteCodeJitter::InstrPutVar(TagVal *pc) {
  u_int dst = IdToReg(pc->Sel(0));
  TagVal *tagVal = TagVal::FromWordDirect(pc->Sel(1));

  if (AbstractCode::GetIdRef(tagVal) == AbstractCode::Global)
    tagVal = LookupSubst(Store::DirectWordToInt(tagVal->Sel(0)));

  switch (AbstractCode::GetIdRef(tagVal)) {
  case AbstractCode::LastUseLocal:
  case AbstractCode::Local:
    {    
      u_int src = IdToReg(tagVal->Sel(0));
      if(src != dst) {
	SET_INSTR_2R(PC,load_reg,dst,src);
      }
    }
    break;
  case AbstractCode::Global:
    {
      SET_INSTR_1R1I(PC,load_global,dst,
		     Store::DirectWordToInt(tagVal->Sel(0)));
    }
    break;
  case AbstractCode::Immediate:
    {
      word val = tagVal->Sel(0);
      if (PointerOp::IsInt(val)) {
	s_int x = Store::DirectWordToInt(val);
	if(x == 0) {
	  SET_INSTR_1R(PC,load_zero,dst);
	} else {
	  SET_INSTR_1R1I(PC,load_int,dst,x);
	}
      }
      else {
	u_int index = imEnv.Register(val);
	SET_INSTR_1R1I(PC,load_immediate,dst,index);
      }
    }
    break;
  default:
    Error("ByteCodeJitter::LoadIdRef: invalid idRef Tag");
  }

  return TagVal::FromWordDirect(pc->Sel(2));
}

// PutNew of id * string * instr
inline TagVal *ByteCodeJitter::InstrPutNew(TagVal *pc) {
  u_int dst = IdToReg(pc->Sel(0));
  u_int strIndex = imEnv.Register(pc->Sel(1));
  SET_INSTR_1R1I(PC,new_con,dst,strIndex);  
  return TagVal::FromWordDirect(pc->Sel(2));
}

// PutCon of id * idRef * idRef vector * instr
inline TagVal *ByteCodeJitter::InstrPutCon(TagVal *pc) {
  u_int dst = IdToReg(pc->Sel(0));
  u_int constr = LoadIdRefKill(pc->Sel(1));

  Vector *idRefs = Vector::FromWordDirect(pc->Sel(2));
  u_int size = idRefs->GetLength();

  SET_INSTR_2R1I(PC,prepare_con,dst,constr,size);
  u_int reg;
  for(u_int i=0; i<size; i++) {
    reg = LoadIdRefKill(idRefs->Sub(i),true);
    SET_INSTR_2R1I(PC,init_con,dst,reg,i);
  }

  return TagVal::FromWordDirect(pc->Sel(3));
}

// PutTag of id * int * int * idRef vector * instr 
inline TagVal *ByteCodeJitter::InstrPutTag(TagVal *pc) {
  u_int maxTag = Store::DirectWordToInt(pc->Sel(1));
  u_int tag  = Store::DirectWordToInt(pc->Sel(2));  
  u_int newtagInstr;
  u_int inittagInstr;

  if (Alice::IsBigTagVal(maxTag)) {
    newtagInstr  = new_bigtagval;
    inittagInstr = init_bigtagval;
  } else {
    newtagInstr  = new_tagval;
    inittagInstr = init_tagval;
  }

  Vector *idRefs = Vector::FromWordDirect(pc->Sel(3));
  u_int nargs = idRefs->GetLength();
  u_int dst = IdToReg(pc->Sel(0));

  SET_INSTR_1R2I(PC,newtagInstr,dst,nargs,tag);    
  u_int reg;
  for (u_int i = 0; i<nargs; i++) {
    reg = LoadIdRefKill(idRefs->Sub(i),true);
    SET_INSTR_2R1I(PC,inittagInstr,dst,reg,i);
  }
  
  return TagVal::FromWordDirect(pc->Sel(4));
}

// PutRef of id * idRef * instr
inline TagVal *ByteCodeJitter::InstrPutRef(TagVal *pc) {
  u_int dst = IdToReg(pc->Sel(0));
  u_int src = LoadIdRefKill(pc->Sel(1));
  SET_INSTR_2R(PC,new_cell,dst,src);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// PutTup of id * idRef vector * instr
inline TagVal *ByteCodeJitter::InstrPutTup(TagVal *pc) {
  u_int dst = IdToReg(pc->Sel(0));
  Vector *idRefs = Vector::FromWordDirect(pc->Sel(1));
  u_int size = idRefs->GetLength();

  SET_INSTR_1R1I(PC,new_tup,dst,size);
  u_int reg;
  for(u_int i=0; i<size; i++) {
    reg = LoadIdRefKill(idRefs->Sub(i),true);
    SET_INSTR_2R1I(PC,init_tup,dst,reg,i);
  }

  return TagVal::FromWordDirect(pc->Sel(2));
}

// PutPolyRec of id * label vector * idRef vector * instr       
inline TagVal *ByteCodeJitter::InstrPutPolyRec(TagVal *pc) {
  u_int dst = IdToReg(pc->Sel(0));
  u_int labelsAddr = imEnv.Register(pc->Sel(1));
  SET_INSTR_1R1I(PC,new_polyrec,dst,labelsAddr);

  Vector *idRefs = Vector::FromWordDirect(pc->Sel(2));
  u_int size = idRefs->GetLength();
  u_int reg;
  for(u_int i=0; i<size; i++) {
    reg = LoadIdRefKill(idRefs->Sub(i),true);
    SET_INSTR_2R1I(PC,init_polyrec,dst,reg,i);
  }

  return TagVal::FromWordDirect(pc->Sel(3));
}

// PutVec of id * idRef vector * instr
inline TagVal *ByteCodeJitter::InstrPutVec(TagVal *pc) {
  u_int dst = IdToReg(pc->Sel(0));
  Vector *idRefs = Vector::FromWordDirect(pc->Sel(1));
  u_int size = idRefs->GetLength();
  SET_INSTR_1R1I(PC,new_vec,dst,size);

  u_int reg;
  for(u_int i=0; i<size; i++) {
    reg = LoadIdRefKill(idRefs->Sub(i),true);
    SET_INSTR_2R1I(PC,init_vec,dst,reg,i);
  }
  
  return TagVal::FromWordDirect(pc->Sel(2));
}

// Close of id * idRef vector * template * instr
inline TagVal *ByteCodeJitter::InstrClose(TagVal *pc) {
  u_int dst = IdToReg(pc->Sel(0));
  Vector *globalRefs = Vector::FromWordDirect(pc->Sel(1));
  u_int nGlobals = globalRefs->GetLength();

  // Instantiate the template into an abstract code:
  TagVal *abstractCode =
    TagVal::New(AbstractCode::Function, AbstractCode::functionWidth);
  TagVal *template_ = TagVal::FromWordDirect(pc->Sel(2));
  template_->AssertWidth(AbstractCode::functionWidth);
  abstractCode->Init(0, template_->Sel(0));

  // Inherit substitution
  Vector *subst = Vector::New(nGlobals);
  for (u_int i = nGlobals; i--; ) {
    TagVal *tagVal = TagVal::FromWord(globalRefs->Sub(i));
      if (AbstractCode::GetIdRef(tagVal) == AbstractCode::Global) {
	TagVal *substVal = LookupSubst(Store::DirectWordToInt(tagVal->Sel(0)));
	if (AbstractCode::GetIdRef(substVal) == AbstractCode::Immediate) {
	  TagVal *some = TagVal::New(Types::SOME, 1);
	  some->Init(0, substVal->Sel(0));
	  subst->Init(i, some->ToWord());
	}
	else
	  subst->Init(i, Store::IntToWord(Types::NONE));
      }
      else
	subst->Init(i, Store::IntToWord(Types::NONE));
  }
  abstractCode->Init(1, subst->ToWord());
  abstractCode->Init(2, template_->Sel(2));
  abstractCode->Init(3, template_->Sel(3));
  abstractCode->Init(4, template_->Sel(4));
  abstractCode->Init(5, template_->Sel(5));
  abstractCode->Init(6, template_->Sel(6));

  // Construct concrete code from abstract code:
  word wConcreteCode =
    AliceLanguageLayer::concreteCodeConstructor(abstractCode);
  u_int ccAddr = imEnv.Register(wConcreteCode);
  // compile the code
  SET_INSTR_1R2I(PC,mk_closure,dst,ccAddr,nGlobals);
  for (u_int i = nGlobals; i--; ) {
    u_int reg = LoadIdRefKill(globalRefs->Sub(i),true);
    SET_INSTR_2R1I(PC,init_closure,dst,reg,i);
  }

  return TagVal::FromWordDirect(pc->Sel(3));
}

// Specialize of id * idRef vector * template * instr  
/* On runtime every value of the global environment is directly 
 * substituted into the code. This means that every time such a closure
 * is really needed, new code must be compiled.
 */
inline TagVal *ByteCodeJitter::InstrSpecialize(TagVal *pc) {
  u_int dst = IdToReg(pc->Sel(0));
  Vector *globalRefs = Vector::FromWordDirect(pc->Sel(1));
  u_int nGlobals = globalRefs->GetLength();

  // create code for runtime substitution building
  // substitiution is stored in scratch register S0
  u_int S0 = GetNewScratch();
  u_int S1 = GetNewScratch();
  SET_INSTR_1R1I(PC,new_vec,S0,nGlobals);
  for(u_int i=nGlobals; i--; ) {
    u_int src = LoadIdRefKill(globalRefs->Sub(i),true);
    SET_INSTR_1R2I(PC,new_tagval,S1,1,Types::SOME);
    SET_INSTR_2R1I(PC,init_tagval,S1,src,0);
    SET_INSTR_2R1I(PC,init_vec,S0,S1,i);
  }

  u_int templateAddr = imEnv.Register(pc->Sel(2));
  SET_INSTR_2R2I(PC,spec_closure,dst,S0,templateAddr,nGlobals);
  for (u_int i = 0; i<nGlobals; i++) {
    u_int src = LoadIdRefKill(globalRefs->Sub(i),true);
    SET_INSTR_2R1I(PC,init_closure,dst,src,i);
  }
  return TagVal::FromWordDirect(pc->Sel(3));
}

// AppPrim of value * idRef vector * (idDef * instr) option   
/*inline*/ void ByteCodeJitter::CompileApplyPrimitive(Closure *closure, 
						      Vector *args, 
						      bool isTailcall) { 
  u_int nArgs = args->GetLength();
  ConcreteCode *concreteCode = 
    ConcreteCode::FromWord(closure->GetConcreteCode());		
  Interpreter *interpreter = concreteCode->GetInterpreter();

  // we can directly store the address in the code as chunks are not GC'd
  u_int interpreterAddr = (u_int) interpreter;

  // The CCC for primitives is normally done in  primitive interpreter->Run.
  // As we want to skip this and directly call the primitive, we must
  // compile the CCC manually.
  u_int inArity = interpreter->GetInArity(concreteCode);
  bool needCCC = false;
  if(inArity != nArgs) {
    BCJIT_DEBUG("primitive CCC: %d --> %d\n",nArgs,inArity);
    switch(inArity) {							
    case 0:
      {
	if(nArgs == 1) { // request unit
	  u_int arg0 = LoadIdRefKill(args->Sub(0));
	  SET_INSTR_1R(PC,await,arg0);
	  needCCC = true;
	}
      }
      break;
    case 1:
      {
	switch(nArgs) {
	case 0: // set unit as argument
	  {
	    u_int S = GetNewScratch();
	    SET_INSTR_1I(PC,seam_set_nargs,1);	    
	    SET_INSTR_1R(PC,load_zero,S);
	    SET_INSTR_1R1I(PC,seam_set_sreg,S,0);
	    needCCC = true;
	  }
	  break;
	case 1:
	  Assert(false); // must not be reached as inArity!=nArgs
	  break;
	default: // construct a tuple
	  {
	    u_int S = GetNewScratch();
	    SET_INSTR_1R1I(PC,new_tup,S,nArgs);
	    for(u_int i=nArgs; i--; ) {
	      u_int arg = LoadIdRefKill(args->Sub(i),true);
	      SET_INSTR_2R1I(PC,init_tup,S,arg,i);
	    }
	    SET_INSTR_1I(PC,seam_set_nargs,1);
	    SET_INSTR_1R1I(PC,seam_set_sreg,S,0); 
	    needCCC = true;
	  }
	}
      }
      break;							
    default:							
      {
	if(nArgs > Scheduler::maxArgs) {
	  /* In this case we are lost :-( We cannot do a direct call. 
	   * Since we must pass a tuple, the primitive must do the CCC 
	   * internally. We solve this by translating the primitive call 
	   * as a normal call. This is not optimal, but this case should 
	   * almost never happen. 
	   * I have never seen a primitive with more than 16 arguments ;-)
	   */
	  u_int S0 = GetNewScratch();
	  u_int S1 = GetNewScratch();
	  SET_INSTR_1R1I(PC,new_tup,S0,nArgs); // tuple in S0
	  for(u_int i=nArgs; i--; ) {
	    u_int src = LoadIdRefKill(args->Sub(i),true);
	    SET_INSTR_2R1I(PC,init_tup,S0,src,i);
	  }
	  u_int primAddr = imEnv.Register(closure->ToWord());
	  SET_INSTR_1R1I(PC,load_immediate,S1,primAddr); // primitive in S1
	  SET_INSTR_1R1I(PC,seam_call1,S1,S0);
	  return;
	} else if(nArgs == 1) { // deconstruct tuple
	  u_int tuple = LoadIdRefKill(args->Sub(0));
	  // trust the static compiler that 
	  // inArity == length of deconstructed tuple
	  SET_INSTR_1R(PC,seam_set_nargs,inArity);
	  u_int S = GetNewScratch();
	  for(u_int i=inArity; i--; ) {
	    SET_INSTR_2R1I(PC,select_tup,S,tuple,i);
	    SET_INSTR_1R1I(PC,seam_set_sreg,S,i);
	  }
	  needCCC = true;
	}
      }
    }
  }

  // If a CCC was compiled, we know that all arguments are already in 
  // Scheduler registers. So we only need to set the call instruction.
  if(needCCC) {
    u_int callInstr = isTailcall ? seam_tailcall_prim : seam_call_prim;
    SET_INSTR_2I(PC,callInstr,nArgs,interpreterAddr);     
    return;
  }

  // compile argument setting and call instructions
  switch(nArgs) {
  case 0:
    {
      u_int callInstr = isTailcall ? seam_tailcall_prim0 : seam_call_prim0;
      SET_INSTR_1I(PC,callInstr,interpreterAddr);
    }
    break;
  case 1: 
    {
      u_int arg0 = LoadIdRefKill(args->Sub(0));
      u_int callInstr = isTailcall ? seam_tailcall_prim1 : seam_call_prim1;
      SET_INSTR_1R1I(PC,callInstr,arg0,interpreterAddr);
    }
    break;
  case 2:
    {
      u_int arg0 = LoadIdRefKill(args->Sub(0));
      u_int arg1 = LoadIdRefKill(args->Sub(1));
      u_int callInstr = isTailcall ? seam_tailcall_prim2 : seam_call_prim2;
      SET_INSTR_2R1I(PC,callInstr,arg0,arg1,interpreterAddr);
    }
    break;
  default:
    {
      if(nArgs <= Scheduler::maxArgs) {
	SET_INSTR_1I(PC,seam_set_nargs,nArgs);
	u_int reg;
	for(u_int i=0; i<nArgs; i++) {
	  reg = LoadIdRefKill(args->Sub(i),true);
	  SET_INSTR_1R1I(PC,seam_set_sreg,reg,i);
	}
	u_int callInstr = isTailcall ? seam_tailcall_prim : seam_call_prim;
	SET_INSTR_2I(PC,callInstr,nArgs,interpreterAddr);     
      } else {
	u_int S = GetNewScratch();
	SET_INSTR_1R1I(PC,new_tup,S,nArgs);
	for(u_int i=nArgs; i--; ) {
	  u_int src = LoadIdRefKill(args->Sub(i),true);
	  SET_INSTR_2R1I(PC,init_tup,S,src,i);
	}
	u_int callInstr = isTailcall ? seam_tailcall_prim1 : seam_call_prim1;
	SET_INSTR_1R1I(PC,callInstr,S,interpreterAddr);
      }
    }
  }
}


// AppPrim of value * idRef vector * (idDef * instr) option   
inline TagVal *ByteCodeJitter::InstrAppPrim(TagVal *pc) {
  Closure *closure = Closure::FromWordDirect(pc->Sel(0));
  Vector *args = Vector::FromWordDirect(pc->Sel(1));
  TagVal *idDefInstrOpt = TagVal::FromWord(pc->Sel(2));
  bool isTailcall = (idDefInstrOpt == INVALID_POINTER);

  // try first to inline some frequent primitives
  ConcreteCode *concreteCode = 
    ConcreteCode::FromWord(closure->GetConcreteCode());		
  Interpreter *interpreter = concreteCode->GetInterpreter();
  void *cFunction = (void*) interpreter->GetCFunction();


  TagVal *continuation;
  if(InlinePrimitive(cFunction,args,idDefInstrOpt,&continuation))
    return continuation;
  
  // compile normal primitive call
  CompileApplyPrimitive(closure,args,isTailcall);

  if(isTailcall)
    return INVALID_POINTER;

  // non-tailcall
  Tuple *idDefInstr = Tuple::FromWordDirect(idDefInstrOpt->Sel(0));
  Vector *rets = Vector::New(1);
  rets->Init(0,idDefInstr->Sel(0));
  CompileCCC(1,rets); // TODO: optimize if we can skip CCC
  // compile continuation
  return TagVal::FromWordDirect(idDefInstr->Sel(1)); 
}


// AppVar of idRef * idRef vector * bool * (idDef vector * instr) option
/*inline*/ TagVal *ByteCodeJitter::InstrAppVar(TagVal *pc) {
  Vector *args = Vector::FromWordDirect(pc->Sel(1));
  u_int nArgs = args->GetLength();
  TagVal *idDefsInstrOpt = TagVal::FromWord(pc->Sel(3));
  bool isTailcall = (idDefsInstrOpt == INVALID_POINTER);

  TagVal *tagVal = TagVal::FromWordDirect(pc->Sel(0));
  if (AbstractCode::GetIdRef(tagVal) == AbstractCode::Global)
    tagVal = LookupSubst(Store::DirectWordToInt(tagVal->Sel(0)));  

  switch (AbstractCode::GetIdRef(tagVal)) {
  case AbstractCode::Immediate:
    {
      Closure *closure = Closure::FromWord(tagVal->Sel(0));       
      if(closure != INVALID_POINTER) {
	ConcreteCode *concreteCode = 
	  ConcreteCode::FromWord(closure->GetConcreteCode());		
	if(concreteCode != INVALID_POINTER) {
	  Interpreter *interpreter = concreteCode->GetInterpreter();
	  void *cFunction = (void*) interpreter->GetCFunction();
	  if(cFunction != NULL) { // this is a primitive
	    // try to inline some other common primitives
	    // prepare args for InlinePrimitive
	    TagVal *continuation;
	    TagVal *idDefInstrOpt;
	    if(isTailcall)
	      idDefInstrOpt = INVALID_POINTER;
	    else {		
	      Tuple *idDefsInstr = 
		Tuple::FromWordDirect(idDefsInstrOpt->Sel(0));
	      Vector *rets = Vector::FromWordDirect(idDefsInstr->Sel(0));
	      if(rets->GetLength() != 1) {
		Error("Uups. BCJ::AppVar inline prim, #rets != 1");
	      }
	      idDefInstrOpt = TagVal::New(idDefsInstrOpt->GetTag(),2);
	      Tuple *tuple = Tuple::New(2);
	      tuple->Init(0,rets->Sub(0));
	      tuple->Init(1,idDefsInstr->Sel(1));
	      idDefInstrOpt->Init(0,tuple->ToWord());
	    }
	    if(InlinePrimitive(cFunction,args,idDefInstrOpt,&continuation))
	      return continuation;
	    
	    // we directly call the primitive function
	    CompileApplyPrimitive(closure,args,isTailcall);
	    if(isTailcall) {
	      continuation = INVALID_POINTER;
	    } else {
	      Tuple *idDefsInstr = 
		Tuple::FromWordDirect(idDefsInstrOpt->Sel(0));
	      Vector *rets = Vector::FromWordDirect(idDefsInstr->Sel(0));
	      u_int inArity = rets->GetLength();
	      CompileCCC(inArity,rets); 
	      continuation = TagVal::FromWordDirect(idDefsInstr->Sel(1));
	    } 
	    return continuation;
	  }
	}
      }
    }
    break;
  default:
    ;
  }

  // TODO: try to find out more about closure to optimize
  //       - skip call CCC
  //       - call primitives

  // compile argument setting and call instructions
  switch(nArgs) {
  case 0: 
    {
      u_int callInstr = isTailcall ? seam_tailcall0 : seam_call0;
      u_int closure = LoadIdRefKill(pc->Sel(0));
      SET_INSTR_1R(PC,callInstr,closure);
    }
    break;
  case 1: 
    {
      u_int arg0 = LoadIdRefKill(args->Sub(0));
      u_int callInstr = isTailcall ? seam_tailcall1 : seam_call1;
      u_int closure = LoadIdRefKill(pc->Sel(0));
      SET_INSTR_2R(PC,callInstr,closure,arg0);
    }
    break;
  case 2:
    {
      u_int arg0 = LoadIdRefKill(args->Sub(0));
      u_int arg1 = LoadIdRefKill(args->Sub(1));
      u_int callInstr = isTailcall ? seam_tailcall2 : seam_call2;
      u_int closure = LoadIdRefKill(pc->Sel(0));
      SET_INSTR_3R(PC,callInstr,closure,arg0,arg1);
    }
    break;
  case 3:
    {
      u_int arg0 = LoadIdRefKill(args->Sub(0));
      u_int arg1 = LoadIdRefKill(args->Sub(1));
      u_int arg2 = LoadIdRefKill(args->Sub(2));
      u_int callInstr = isTailcall ? seam_tailcall3 : seam_call3;
      u_int closure = LoadIdRefKill(pc->Sel(0));
      SET_INSTR_4R(PC,callInstr,closure,arg0,arg1,arg2);
    }
    break;
  default:
    {
      if(nArgs <= Scheduler::maxArgs) {
	SET_INSTR_1I(PC,seam_set_nargs,nArgs);
	u_int reg;
	for(u_int i=0; i<nArgs; i++) {
	  reg = LoadIdRefKill(args->Sub(i),true);
	  SET_INSTR_1R1I(PC,seam_set_sreg,reg,i);
	}
	u_int callInstr = isTailcall ? seam_tailcall : seam_call;
	u_int closure = LoadIdRefKill(pc->Sel(0));
	SET_INSTR_1R1I(PC,callInstr,closure,nArgs);     
      } else {
	u_int S = GetNewScratch();
	SET_INSTR_1R1I(PC,new_tup,S,nArgs);
	for(u_int i=nArgs; i--; ) {
	  u_int src = LoadIdRefKill(args->Sub(i),true);
	  SET_INSTR_2R1I(PC,init_tup,S,src,i);
	}
	u_int callInstr = isTailcall ? seam_tailcall1 : seam_call1;
	u_int closure = LoadIdRefKill(pc->Sel(0));
	SET_INSTR_2R(PC,callInstr,closure,S);
      }
    }
  }

 compile_continuation: // nice hack ;-)

  if(isTailcall) 
    return INVALID_POINTER;

  // non-tailcall
  Tuple *idDefsInstr = Tuple::FromWordDirect(idDefsInstrOpt->Sel(0));
  Vector *rets = Vector::FromWordDirect(idDefsInstr->Sel(0));
  u_int inArity = rets->GetLength();
  CompileCCC(inArity,rets); 

  return TagVal::FromWordDirect(idDefsInstr->Sel(1)); // compile continuation
}

// GetRef of id * idRef * instr
inline TagVal *ByteCodeJitter::InstrGetRef(TagVal *pc) {
  BCJIT_DEBUG("compile GetRef ");
  u_int dst = IdToReg(pc->Sel(0));
  u_int src = LoadIdRefKill(pc->Sel(1));
  BCJIT_DEBUG(" src->%d and dst->%d\n",src,dst);
  SET_INSTR_2R(PC,load_cell,dst,src);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// GetTup of idDef vector * idRef * instr
inline TagVal *ByteCodeJitter::InstrGetTup(TagVal *pc) {
  Vector *idDefs = Vector::FromWordDirect(pc->Sel(0));
  u_int size = idDefs->GetLength();
  u_int src = LoadIdRefKill(pc->Sel(1));

  if(size == 0) { // this is a request for unit
    SET_INSTR_1R(PC,await,src);
    return TagVal::FromWordDirect(pc->Sel(2));
  }

  // use specialized instructions first
  u_int dst;
  TagVal *idDef;
  u_int index = size - 1;
  for(; index>2; index--) {
    idDef = TagVal::FromWord(idDefs->Sub(index));
    if(idDef != INVALID_POINTER) {
      dst = IdToReg(idDef->Sel(0));
#ifdef DO_REG_ALLOC
      if(dst == src) {
	u_int S = GetNewScratch();
	SET_INSTR_2R(PC,load_reg,S,src);
	src = S;
      }
#endif
      SET_INSTR_2R1I(PC,select_tup,dst,src,index);
    }
  }
  switch(index) {
  case 2: { 
    idDef = TagVal::FromWord(idDefs->Sub(2));
    if(idDef != INVALID_POINTER) {
      dst = IdToReg(idDef->Sel(0));
#ifdef DO_REG_ALLOC
      if(dst == src) {
	u_int S = GetNewScratch();
	SET_INSTR_2R(PC,load_reg,S,src);
	src = S;
      }
#endif
      SET_INSTR_2R(PC,select_tup2,dst,src); 
    }
  }
  case 1: { 
    idDef = TagVal::FromWord(idDefs->Sub(1));
    if(idDef != INVALID_POINTER) {
      dst = IdToReg(idDef->Sel(0));
#ifdef DO_REG_ALLOC
      if(dst == src) {
	u_int S = GetNewScratch();
	SET_INSTR_2R(PC,load_reg,S,src);
	src = S;
      }
#endif
      SET_INSTR_2R(PC,select_tup1,dst,src); 
    }
  }
  case 0: { 
    idDef = TagVal::FromWord(idDefs->Sub(0));
    if(idDef != INVALID_POINTER) {
      dst = IdToReg(idDef->Sel(0));
      SET_INSTR_2R(PC,select_tup0,dst,src); 
    }
  }
  default: ;
  }
  
  return TagVal::FromWordDirect(pc->Sel(2));
}

// Sel of id * idRef * int * instr
inline TagVal *ByteCodeJitter::InstrSel(TagVal *pc) {
  u_int dst = IdToReg(pc->Sel(0));
  u_int index = Store::DirectWordToInt(pc->Sel(2));
  u_int src = LoadIdRefKill(pc->Sel(1));   
  switch(index) {
  case 0: { SET_INSTR_2R(PC,select_tup0,dst,src); } break;
  case 1: { SET_INSTR_2R(PC,select_tup1,dst,src); } break;
  case 2: { SET_INSTR_2R(PC,select_tup2,dst,src); } break;
  default: { SET_INSTR_2R1I(PC,select_tup,dst,src,index); }
  }
  return TagVal::FromWordDirect(pc->Sel(3));
}

// LazyPolySel of id vector * idRef * label vector * instr 
inline TagVal *ByteCodeJitter::InstrLazyPolySel(TagVal *pc) {
  Vector *ids = Vector::FromWordDirect(pc->Sel(0));
  u_int size = ids->GetLength();
  u_int src = LoadIdRefKill(pc->Sel(1));
  Vector *labels = Vector::FromWordDirect(pc->Sel(2));

  /*
  for(u_int i=size; i--; ) {
    u_int dst = IdToReg(ids->Sub(i));
#ifdef DO_REG_ALLOC
    if(i > 0 && dst == size) {
    u_int S = GetNewScratch();
      SET_INSTR_2R(PC,load_reg,S,src);
      src = S;
    }
#endif
    u_int labelAddr = imEnv.Register(labels->Sub(i));
    SET_INSTR_2R1I(PC,lazyselect_polyrec,dst,src,labelAddr);
    } */

  if(size > 1) {
    Vector *regs = Vector::New(size);
    u_int regsAddr = imEnv.Register(regs->ToWord());
    Vector *selLabels = Vector::New(size);
    u_int selLabelsAddr = imEnv.Register(selLabels->ToWord());
    for(u_int i=size; i--; ) {
      word dst = Store::IntToWord(IdToReg(ids->Sub(i)));
      regs->Init(i,dst);
      selLabels->Init(i,labels->Sub(i));
    }
    SET_INSTR_1R2I(PC,lazyselect_polyrec_n,src,regsAddr,selLabelsAddr);
  } else { // unfold 
    u_int dst = IdToReg(ids->Sub(0));
    u_int labelAddr = imEnv.Register(labels->Sub(0));
    SET_INSTR_2R1I(PC,lazyselect_polyrec,dst,src,labelAddr);
  }
  
  return TagVal::FromWordDirect(pc->Sel(3));
}

// Raise of idRef
inline TagVal *ByteCodeJitter::InstrRaise(TagVal *pc) {
  u_int exn = LoadIdRefKill(pc->Sel(0));
  SET_INSTR_1R(PC,raise_normal,exn);
  return INVALID_POINTER;
}

// Reraise of idRef
inline TagVal *ByteCodeJitter::InstrReraise(TagVal *pc) {
  u_int pkg = LoadIdRefKill(pc->Sel(0));
  SET_INSTR_1R(PC,raise_direct,pkg); 
  return INVALID_POINTER;
}

// Try of instr * idDef * idDef * instr
inline TagVal *ByteCodeJitter::InstrTry(TagVal *pc) {
  TagVal *idDef1 = TagVal::FromWord(pc->Sel(1));
  TagVal *idDef2 = TagVal::FromWord(pc->Sel(2));
  u_int pkg = (idDef1 != INVALID_POINTER) ? 
    IdToReg(idDef1->Sel(0)) : GetNewScratch();
  u_int exn = (idDef2 != INVALID_POINTER) ? 
    IdToReg(idDef2->Sel(0)) : GetNewScratch();
  Tuple *handler = Tuple::New(3);
  handler->Init(0,Store::IntToWord(pkg));
  handler->Init(1,Store::IntToWord(exn));
  u_int handlerAddr = imEnv.Register(handler->ToWord());
  SET_INSTR_1I(PC,install_handler,handlerAddr);
  CompileInstr(TagVal::FromWordDirect(pc->Sel(0))); // compile normal code
  handler->Init(2,Store::IntToWord(PC));     // now set handler pc
  return TagVal::FromWordDirect(pc->Sel(3)); // continue with handler code
}

// EndTry of instr
inline TagVal *ByteCodeJitter::InstrEndTry(TagVal *pc) {
  SET_INSTR(PC,remove_handler);
  return TagVal::FromWordDirect(pc->Sel(0));
}

// EndHandle of instr
inline TagVal *ByteCodeJitter::InstrEndHandle(TagVal *pc) {
  return TagVal::FromWordDirect(pc->Sel(0));
}

// IntTest of idRef * (int * instr) vector * instr 
inline TagVal *ByteCodeJitter::InstrIntTest(TagVal *pc) {
  u_int testVal = LoadIdRefKill(pc->Sel(0));
  Vector *tests = Vector::FromWordDirect(pc->Sel(1));
  u_int nTests = tests->GetLength();
  // profiling suggested that "ijump" cannot be used in most cases as nTests>1
  IntMap *map = IntMap::New(2 * nTests);
  u_int mapAddr = imEnv.Register(map->ToWord());
  SET_INSTR_1R1I(PC,itest,testVal,mapAddr);
#ifdef RELATIVE_JUMP
  u_int instrPC = PC;
#endif
  CompileInstr(TagVal::FromWordDirect(pc->Sel(2))); // compile else branch
  for (u_int i = 0; i<nTests; i++) {
    Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
#ifdef RELATIVE_JUMP
    map->Put(pair->Sel(0),Store::IntToWord(PC - instrPC));
#else
    map->Put(pair->Sel(0),Store::IntToWord(PC));
#endif
    CompileInstr(TagVal::FromWordDirect(pair->Sel(1))); // compile branch
  }
  return INVALID_POINTER;
}

// CompactIntTest of idRef * int * instrs * instr 
inline TagVal *ByteCodeJitter::InstrCompactIntTest(TagVal *pc) {
  u_int testVal = LoadIdRefKill(pc->Sel(0));
  u_int offset  = Store::DirectWordToInt(pc->Sel(1));
  Vector *jumpTable = Vector::FromWordDirect(pc->Sel(2));
  u_int size = jumpTable->GetLength();
  
  // try to optimize the test with specialized instructions
  if(size == 1) {
    u_int ijumpInstrPC = PC;
    SET_INSTR_1R2I(PC,ijump_eq,0,0,0);                  // create dummy code
#ifdef RELATIVE_JUMP
    u_int jmpPC = PC;
#endif
    CompileInstr(TagVal::FromWordDirect(pc->Sel(3)));   // compile else branch
#ifdef RELATIVE_JUMP
    SET_INSTR_1R2I(ijumpInstrPC,ijump_eq,testVal,offset,PC-jmpPC);
#else
    SET_INSTR_1R2I(ijumpInstrPC,ijump_eq,testVal,offset,PC); // patch instr
#endif
    return TagVal::FromWordDirect(jumpTable->Sub(0));   // compile then part
  }
  
  // size > 1
  SET_INSTR_1R2I(PC,citest,testVal,size,offset);
  u_int jumpTablePC = PC;
#ifdef RELATIVE_JUMP
  u_int jmpPC = PC;
#endif
  PC += size;
  CompileInstr(TagVal::FromWordDirect(pc->Sel(3))); // compile else branch
  for(u_int i=0; i<size; i++) {
#ifdef RELATIVE_JUMP
    SET_1I(jumpTablePC,PC - jmpPC);
#else
    SET_1I(jumpTablePC,PC); // jumpTablePC is implicitly incremented
#endif
    CompileInstr(TagVal::FromWordDirect(jumpTable->Sub(i))); // compile branch
  }

  return INVALID_POINTER;
}

// RealTest of idRef * (real * instr) vector * instr
inline TagVal *ByteCodeJitter::InstrRealTest(TagVal *pc) {
  u_int testVal = LoadIdRefKill(pc->Sel(0));
  Vector *tests = Vector::FromWordDirect(pc->Sel(1));
  u_int nTests = tests->GetLength();

  // try to optimize the test with specialized instructions
  if(nTests == 1) { 
    u_int rjumpInstrPC = PC;
    SET_INSTR_1R2I(PC,rjump_eq,0,0,0); // create dummy code
#ifdef RELATIVE_JUMP
    u_int jmpPC = PC;
#endif
    CompileInstr(TagVal::FromWordDirect(pc->Sel(2))); // compile else branch
    Tuple *pair = Tuple::FromWordDirect(tests->Sub(0));
    u_int valAddr = imEnv.Register(pair->Sel(0));
#ifdef RELATIVE_JUMP
    SET_INSTR_1R2I(rjumpInstrPC,rjump_eq,testVal,valAddr,PC-jmpPC);
#else
    SET_INSTR_1R2I(rjumpInstrPC,rjump_eq,testVal,valAddr,PC); // patch instr
#endif
    return TagVal::FromWordDirect(pair->Sel(1)); // compile then part
  }

  // nTests > 1
  ChunkMap *map = ChunkMap::New(2 * nTests);
  u_int mapAddr = imEnv.Register(map->ToWord());
  SET_INSTR_1R1I(PC,rtest,testVal,mapAddr);
#ifdef RELATIVE_JUMP
  u_int instrPC = PC;
#endif
  CompileInstr(TagVal::FromWordDirect(pc->Sel(2))); // compile else branch
  for (u_int i = 0; i<nTests; i++) {
    Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
#ifdef RELATIVE_JUMP
    map->Put(pair->Sel(0),Store::IntToWord(PC - instrPC));
#else
    map->Put(pair->Sel(0),Store::IntToWord(PC));
#endif
    CompileInstr(TagVal::FromWordDirect(pair->Sel(1))); // compile branch
  }
  return INVALID_POINTER;
}

// StringTest of idRef * (string * instr) vector * instr 
inline TagVal *ByteCodeJitter::InstrStringTest(TagVal *pc) {
  u_int testVal = LoadIdRefKill(pc->Sel(0));
  Vector *tests = Vector::FromWordDirect(pc->Sel(1));
  u_int nTests = tests->GetLength();

  // try to optimize the test with specialized instructions
  if(nTests == 1) { 
    u_int sjumpInstrPC = PC;
    SET_INSTR_1R2I(PC,sjump_eq,0,0,0); // create dummy code
#ifdef RELATIVE_JUMP
    u_int jmpPC = PC;
#endif
    CompileInstr(TagVal::FromWordDirect(pc->Sel(2))); // compile else branch
    Tuple *pair = Tuple::FromWordDirect(tests->Sub(0));
    u_int valAddr = imEnv.Register(pair->Sel(0));
#ifdef RELATIVE_JUMP
    SET_INSTR_1R2I(sjumpInstrPC,sjump_eq,testVal,valAddr,PC-jmpPC);
#else
    SET_INSTR_1R2I(sjumpInstrPC,sjump_eq,testVal,valAddr,PC); // patch instr
#endif
    return TagVal::FromWordDirect(pair->Sel(1)); // compile then part
  }

  // nTests > 1
  ChunkMap *map = ChunkMap::New(2 * nTests);
  u_int mapAddr = imEnv.Register(map->ToWord());
  SET_INSTR_1R1I(PC,stest,testVal,mapAddr);
#ifdef RELATIVE_JUMP
  u_int instrPC = PC;
#endif
  CompileInstr(TagVal::FromWordDirect(pc->Sel(2))); // compile else branch
  for (u_int i = 0; i<nTests; i++) {
    Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
#ifdef RELATIVE_JUMP
    map->Put(pair->Sel(0),Store::IntToWord(PC - instrPC));
#else
    map->Put(pair->Sel(0),Store::IntToWord(PC));
#endif
    CompileInstr(TagVal::FromWordDirect(pair->Sel(1))); // compile branch
  }
  return INVALID_POINTER;
}

// TagTest of idRef * int * (int * instr) vector
//          * (int * idDef vector * instr) vector * instr   
/*inline*/ TagVal *ByteCodeJitter::InstrTagTest(TagVal *pc) {
  u_int testVal = LoadIdRefKill(pc->Sel(0));
  u_int maxTag = Store::DirectWordToInt(pc->Sel(1));
  Vector *nullaryTests = Vector::FromWordDirect(pc->Sel(2));
  Vector *naryTests = Vector::FromWordDirect(pc->Sel(3));
  u_int nullarySize = nullaryTests->GetLength();
  u_int narySize = naryTests->GetLength();
  u_int size = nullarySize + narySize;

  // specify instructions
  bool isBigTag = Alice::IsBigTagVal(maxTag);
   
  // check if we can use an if
  if(size == 1) {
    u_int testInstr = isBigTag ? bigtagtest1 : tagtest1;
    u_int loadInstr = isBigTag ? load_bigtagval : load_tagval;
    u_int patchInstrPC = PC;
    SET_INSTR_1R2I(PC,testInstr,0,0,0); // dummy instr
#ifdef RELATIVE_JUMP
    u_int jmpPC = PC;
#endif
    CompileInstr(TagVal::FromWordDirect(pc->Sel(4))); // compile else branch  
    if(nullarySize == 1) {
      Tuple *pair = Tuple::FromWordDirect(nullaryTests->Sub(0));
      u_int tag = Store::DirectWordToInt(pair->Sel(0));
#ifdef RELATIVE_JUMP
      SET_INSTR_1R2I(patchInstrPC,testInstr,testVal,tag,PC-jmpPC);
#else
      SET_INSTR_1R2I(patchInstrPC,testInstr,testVal,tag,PC); // patch instr
#endif
      return TagVal::FromWordDirect(pair->Sel(1)); // compile branch   
    } else if (narySize == 1) {
      Tuple *triple = Tuple::FromWordDirect(naryTests->Sub(0));
      u_int tag = Store::DirectWordToInt(triple->Sel(0));
#ifdef RELATIVE_JUMP
      SET_INSTR_1R2I(patchInstrPC,testInstr,testVal,tag,PC-jmpPC);
#else
      SET_INSTR_1R2I(patchInstrPC,testInstr,testVal,tag,PC); // patch instr
#endif
      // compile binding
      Vector *idDefs = Vector::FromWordDirect(triple->Sel(1));
      u_int idDefsLength = idDefs->GetLength();
      for (u_int j=0; j<idDefsLength; j++) {
	TagVal *idDef = TagVal::FromWord(idDefs->Sub(j));
	if( idDef != INVALID_POINTER ) { // not wildcard
	  u_int dst = IdToReg(idDef->Sel(0));
#ifdef DO_REG_ALLOC
	  if(dst == testVal) {
	    u_int S = GetNewScratch();
	    SET_INSTR_2R(PC,load_reg,S,testVal);
	    testVal = S;
	  }
#endif
	  SET_INSTR_2R1I(PC,loadInstr,dst,testVal,j);
	}
      }
      return TagVal::FromWordDirect(triple->Sel(2)); // compile branch   
    }
  }

  bool isFastTest = (size*OPTIMIZE_TAGTEST_LEVEL > maxTag);

  // create needed datastructures
  if(!isFastTest) {
    u_int testInstr = isBigTag ? bigtagtest : tagtest;
    u_int loadInstr = isBigTag ? load_bigtagval : load_tagval;
    IntMap *map = IntMap::New(2 * (nullarySize + narySize));
    u_int mapAddr = imEnv.Register(map->ToWord());
    SET_INSTR_1R1I(PC,testInstr,testVal,mapAddr);
#ifdef RELATIVE_JUMP
    u_int instrPC = PC;
#endif
    CompileInstr(TagVal::FromWordDirect(pc->Sel(4))); // compile else branch  
    
    // compile nullary tests
    for(u_int i=0; i<nullarySize; i++) {
      Tuple *pair = Tuple::FromWordDirect(nullaryTests->Sub(i));
#ifdef RELATIVE_JUMP
      map->Put(pair->Sel(0),Store::IntToWord(PC - instrPC));
#else
      map->Put(pair->Sel(0),Store::IntToWord(PC));
#endif
      CompileInstr(TagVal::FromWordDirect(pair->Sel(1))); // compile branch
    }
    
    // compile n-ary tests
    for(u_int i=0; i<narySize; i++) {
      Tuple *triple = Tuple::FromWordDirect(naryTests->Sub(i));
#ifdef RELATIVE_JUMP
      map->Put(triple->Sel(0),Store::IntToWord(PC - instrPC));
#else
      map->Put(triple->Sel(0),Store::IntToWord(PC));
#endif
      // compile binding
      Vector *idDefs = Vector::FromWordDirect(triple->Sel(1));
      u_int idDefsLength = idDefs->GetLength();
      u_int src = testVal;
      for (u_int j=0; j<idDefsLength; j++) {
	TagVal *idDef = TagVal::FromWord(idDefs->Sub(j));
	if( idDef != INVALID_POINTER ) { // not wildcard
	  u_int dst = IdToReg(idDef->Sel(0));
#ifdef DO_REG_ALLOC
	  if(dst == src) {
	    u_int S = GetNewScratch();
	    SET_INSTR_2R(PC,load_reg,S,src);
	    src = S;
	  }
#endif
	  SET_INSTR_2R1I(PC,loadInstr,dst,src,j);
	}
      }
      CompileInstr(TagVal::FromWordDirect(triple->Sel(2))); // compile branch
    }
  } else { // fast test
    u_int testInstr = isBigTag ? cbigtagtest_direct : ctagtest_direct;
    u_int loadInstr = isBigTag ? load_bigtagval : load_tagval;

    SET_INSTR_1R1I(PC,testInstr,testVal,maxTag);
    u_int jumpTablePC = PC;
#ifdef RELATIVE_JUMP
    u_int jmpPC = PC;
#endif
    PC += maxTag;
    u_int elsePC = PC;
    u_int defaultTablePC = jumpTablePC;
    for(u_int i=0; i<maxTag; i++) {
#ifdef RELATIVE_JUMP
      SET_1I(defaultTablePC,elsePC - jmpPC);
#else
      SET_1I(defaultTablePC,elsePC);
#endif
    }
    CompileInstr(TagVal::FromWordDirect(pc->Sel(4))); // compile else branch  
    
    // compile nullary tests
    for(u_int i=0; i<nullarySize; i++) {
      Tuple *pair = Tuple::FromWordDirect(nullaryTests->Sub(i));
      u_int index = jumpTablePC + Store::DirectWordToInt(pair->Sel(0));
#ifdef RELATIVE_JUMP
      SET_1I(index,PC-jmpPC);
#else
      SET_1I(index,PC);
#endif
      CompileInstr(TagVal::FromWordDirect(pair->Sel(1))); // compile branch
    }
    
    // compile n-ary tests
    for(u_int i=0; i<narySize; i++) {
      Tuple *triple = Tuple::FromWordDirect(naryTests->Sub(i));
      u_int index = jumpTablePC + Store::DirectWordToInt(triple->Sel(0));
#ifdef RELATIVE_JUMP
      SET_1I(index,PC-jmpPC);
#else
      SET_1I(index,PC);
#endif
      // compile binding
      Vector *idDefs = Vector::FromWordDirect(triple->Sel(1));
      u_int idDefsLength = idDefs->GetLength();
      u_int src = testVal;
      for (u_int j=0; j<idDefsLength; j++) {
	TagVal *idDef = TagVal::FromWord(idDefs->Sub(j));
	if( idDef != INVALID_POINTER ) { // not wildcard
	  u_int dst = IdToReg(idDef->Sel(0));
#ifdef DO_REG_ALLOC
	  if(dst == src) {
	    u_int S = GetNewScratch();
	    SET_INSTR_2R(PC,load_reg,S,src);
	    src = S;
	  }
#endif
	  SET_INSTR_2R1I(PC,loadInstr,dst,src,j);
	}
      }
      CompileInstr(TagVal::FromWordDirect(triple->Sel(2))); // compile branch
    }
  }
 
  return INVALID_POINTER;
} 

// TODO: check for optimizations
// CompactTagTest of idRef * int * tagTests * instr option  
inline TagVal *ByteCodeJitter::InstrCompactTagTest(TagVal *pc) {
  u_int testVal = LoadIdRefKill(pc->Sel(0));
  u_int maxTag = Store::DirectWordToInt(pc->Sel(1));
  Vector *tests = Vector::FromWordDirect(pc->Sel(2));
  u_int size = tests->GetLength();
  TagVal *elseInstrOpt = TagVal::FromWord(pc->Sel(3));
  bool isSomeElseInstr = (elseInstrOpt != INVALID_POINTER);
  
  bool isFastTest = (size*OPTIMIZE_TAGTEST_LEVEL > maxTag);
  /* --> doesn't seem to be fast
  // check if we can use an if
  if(size == 2 && !isSomeElseInstr) {
    bool isBigTag = Alice::IsBigTagVal(maxTag);
    u_int testInstr = isBigTag ? bigtagtest1 : tagtest1;
    u_int loadInstr = isBigTag ? load_bigtagval : load_tagval;
    u_int patchInstrPC = PC;
    SET_INSTR_1R2I(PC,testInstr,0,0,0); // dummy instr
    { // compile else case case
      Tuple *pair = Tuple::FromWordDirect(tests->Sub(1));
      // compile binding
      TagVal *idDefsOpt = TagVal::FromWord(pair->Sel(0));
      if(idDefsOpt != INVALID_POINTER) {
	Vector *idDefs = Vector::FromWordDirect(idDefsOpt->Sel(0));
	u_int idDefsLength = idDefs->GetLength();
	u_int src = testVal;
        for (u_int j=0; j<idDefsLength; j++) {
	  TagVal *idDef = TagVal::FromWord(idDefs->Sub(j));
	  if( idDef != INVALID_POINTER ) { // not wildcard
	    u_int dst = IdToReg(idDef->Sel(0));
#ifdef DO_REG_ALLOC
	  if(dst == src) {
	    SET_INSTR_2R(PC,load_reg,S1,src);
	    src = S1;
	  }
#endif
	    SET_INSTR_2R1I(PC,loadInstr,dst,src,j);
	  }
	}
      }
      CompileInstr(TagVal::FromWordDirect(pair->Sel(1)));
    }
    SET_INSTR_1R2I(patchInstrPC,testInstr,testVal,0,PC); // patch instr
    // compile rest
    Tuple *pair = Tuple::FromWordDirect(tests->Sub(0));
    // compile binding
    TagVal *idDefsOpt = TagVal::FromWord(pair->Sel(0));
    if(idDefsOpt != INVALID_POINTER) {
      Vector *idDefs = Vector::FromWordDirect(idDefsOpt->Sel(0));
      u_int idDefsLength = idDefs->GetLength();
      u_int src = testVal;
      for (u_int j=0; j<idDefsLength; j++) {
	TagVal *idDef = TagVal::FromWord(idDefs->Sub(j));
	if( idDef != INVALID_POINTER ) { // not wildcard
	  u_int dst = IdToReg(idDef->Sel(0));
#ifdef DO_REG_ALLOC
	  if(dst == src) {
	    SET_INSTR_2R(PC,load_reg,S1,src);
	    src = S;
	  }
#endif
	  SET_INSTR_2R1I(PC,loadInstr,dst,src,j);
	}
      }
    }
    return TagVal::FromWordDirect(pair->Sel(1)); // compile branch
  }
  */
  // specify instructions
  u_int testInstr;
  u_int loadInstr;
  if(Alice::IsBigTagVal(maxTag)) {
    loadInstr = load_bigtagval;
    testInstr = isFastTest ? cbigtagtest_direct : cbigtagtest;
  }
  else {
    loadInstr = load_tagval;
    testInstr = isFastTest ? ctagtest_direct : ctagtest;
  }

  // create needed datastructures
  u_int newTestsSize = isFastTest ? maxTag : size;
  SET_INSTR_1R1I(PC,testInstr,testVal,newTestsSize);
  u_int jumpTablePC = PC;
#ifdef RELATIVE_JUMP
  u_int jmpPC = PC;
#endif
  PC += newTestsSize;
  u_int elsePC = PC;

  if(isSomeElseInstr) { // compile else branch  
    CompileInstr(TagVal::FromWordDirect(elseInstrOpt->Sel(0))); 
  }

  // compile tests
  for(u_int i=0; i<size; i++) {
    Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
#ifdef RELATIVE_JUMP
    SET_1I(jumpTablePC,PC - jmpPC);    
#else
    SET_1I(jumpTablePC,PC);
#endif
    // compile binding
    TagVal *idDefsOpt = TagVal::FromWord(pair->Sel(0));
    if(idDefsOpt != INVALID_POINTER) {
      Vector *idDefs = Vector::FromWordDirect(idDefsOpt->Sel(0));
      u_int idDefsLength = idDefs->GetLength();
      u_int src = testVal;
      for (u_int j=0; j<idDefsLength; j++) {
	TagVal *idDef = TagVal::FromWord(idDefs->Sub(j));
	if( idDef != INVALID_POINTER ) { // not wildcard
	  u_int dst = IdToReg(idDef->Sel(0));
#ifdef DO_REG_ALLOC
	  if(dst == src) {
	    u_int S = GetNewScratch();
	    SET_INSTR_2R(PC,load_reg,S,src);
	    src = S;
	  }
#endif
	  SET_INSTR_2R1I(PC,loadInstr,dst,src,j);
	}
      }
    }
    CompileInstr(TagVal::FromWordDirect(pair->Sel(1))); // compile branch
  }
  // fill the rest with the else PC
  for(u_int i=size; i<newTestsSize; i++) {
#ifdef RELATIVE_JUMP
    SET_1I(jumpTablePC,elsePC-jmpPC);
#else
    SET_1I(jumpTablePC,elsePC);
#endif
  }
  return INVALID_POINTER;  
}


// Contest of idRef * (idRef * instr) vector 
//          * (idRef * idDef vector * instr) vector * instr    
inline TagVal *ByteCodeJitter::InstrConTest(TagVal *pc) {
  u_int testVal = LoadIdRefKill(pc->Sel(0));
  Vector *nullaryTests = Vector::FromWordDirect(pc->Sel(1));
  Vector *naryTests = Vector::FromWordDirect(pc->Sel(2));
  u_int nullarySize = nullaryTests->GetLength();
  u_int narySize = naryTests->GetLength();

  // compile nullary tests
  for(u_int i=0 ; i<nullarySize; i++) {
    Tuple *pair = Tuple::FromWordDirect(nullaryTests->Sub(i));
    u_int src = LoadIdRefKill(pair->Sel(0),true);
    u_int conTestInstrPC = PC;
    SET_INSTR_2R1I(PC,contest,0,0,0);                      // dummy instr
#ifdef RELATIVE_JUMP
    u_int jmpPC = PC;
#endif
    CompileInstr(TagVal::FromWordDirect(pair->Sel(1)));    // compile branch
#ifdef RELATIVE_JUMP
    SET_INSTR_2R1I(conTestInstrPC,contest,testVal,src,PC-jmpPC);
#else
    SET_INSTR_2R1I(conTestInstrPC,contest,testVal,src,PC); // patch instr
#endif
  }

  // compile n-ary tests
  for(u_int i=0; i<narySize; i++) {
    Tuple *triple = Tuple::FromWordDirect(naryTests->Sub(i));
    u_int src = LoadIdRefKill(triple->Sel(0),true);
    u_int conTestInstrPC = PC;
    SET_INSTR_2R1I(PC,contest,0,0,0);                      // dummy instr
#ifdef RELATIVE_JUMP
    u_int jmpPC = PC;
#endif
    // compile binding
    Vector *idDefs = Vector::FromWordDirect(triple->Sel(1));
    u_int idDefsLength = idDefs->GetLength();
    u_int testValReg = testVal;
    for (u_int j=0; j<idDefsLength; j++) {
      TagVal *idDef = TagVal::FromWord(idDefs->Sub(j));
      if( idDef != INVALID_POINTER ) { // not wildcard
	u_int dst = IdToReg(idDef->Sel(0));
#ifdef DO_REG_ALLOC
	  if(dst == testValReg) {
	    u_int S = GetNewScratch();
	    SET_INSTR_2R(PC,load_reg,S,testValReg);
	    testValReg = S;
	  }
#endif
	SET_INSTR_2R1I(PC,load_con,dst,testValReg,j);
      }
    }
    CompileInstr(TagVal::FromWordDirect(triple->Sel(2)));  // compile branch
#ifdef RELATIVE_JUMP
    SET_INSTR_2R1I(conTestInstrPC,contest,testVal,src,PC-jmpPC);
#else
    SET_INSTR_2R1I(conTestInstrPC,contest,testVal,src,PC); // patch instr
#endif
  }
  
  return TagVal::FromWordDirect(pc->Sel(3));
}

// VecTest of idRef * (idDef vector * instr) vector * instr  
inline TagVal *ByteCodeJitter::InstrVecTest(TagVal *pc) {
  u_int testVal = LoadIdRefKill(pc->Sel(0));
  Vector *tests = Vector::FromWordDirect(pc->Sel(1));
  u_int nTests = tests->GetLength();
  IntMap *map = IntMap::New(2 * nTests);
  u_int mapAddr = imEnv.Register(map->ToWord());
  SET_INSTR_1R1I(PC,vectest,testVal,mapAddr);
#ifdef RELATIVE_JUMP
  u_int instrPC = PC;
#endif
  CompileInstr(TagVal::FromWordDirect(pc->Sel(2))); // compile else branch
  for (u_int i = 0; i<nTests; i++) {
    Tuple *pair    = Tuple::FromWordDirect(tests->Sub(i));
    Vector *idDefs = Vector::FromWordDirect(pair->Sel(0));
    word key       = Store::IntToWord(idDefs->GetLength());
#ifdef RELATIVE_JUMP
    map->Put(key, Store::IntToWord(PC - instrPC));
#else
    map->Put(key, Store::IntToWord(PC));
#endif
    // compile binding
    u_int idDefsLength = idDefs->GetLength();
    u_int src = testVal;
    for (u_int j=0; j<idDefsLength; j++) {
      TagVal *idDef = TagVal::FromWord(idDefs->Sub(j));
      if( idDef != INVALID_POINTER ) { // not wildcard
	u_int dst = IdToReg(idDef->Sel(0));
#ifdef DO_REG_ALLOC
	if(dst == testVal) {
	  u_int S = GetNewScratch();
	  SET_INSTR_2R(PC,load_reg,S,testVal);
	  src = S;
	} 
#endif
	SET_INSTR_2R1I(PC,load_vec,dst,src,j);	
      }
    }
    CompileInstr(TagVal::FromWordDirect(pair->Sel(1)));
  }
  return INVALID_POINTER;
}

// Shared of stamp * instr
inline TagVal *ByteCodeJitter::InstrShared(TagVal *pc) {
  word stamp = pc->Sel(0);
  if(sharedTable->IsMember(stamp)) {
    u_int target = Store::DirectWordToInt(sharedTable->Get(stamp));
#ifdef RELATIVE_JUMP
    u_int instrPC = PC;
    SET_INSTR_1I(PC,jump,0);
    s_int offset = target - PC;
    SET_INSTR_1I(instrPC,jump,offset); // patch
#else
    SET_INSTR_1I(PC,jump,target);
#endif
    return INVALID_POINTER;
  }
  else {
    // PC is the address of the instr after Shared
    sharedTable->Put(stamp,Store::IntToWord(PC)); 
    return TagVal::FromWordDirect(pc->Sel(1));
  }
}

// Return of idRef vector
inline TagVal *ByteCodeJitter::InstrReturn(TagVal *pc) {
  Vector *returnIdRefs = Vector::FromWordDirect(pc->Sel(0));
  u_int nArgs          = returnIdRefs->GetLength();
  switch(nArgs) {
  case 1:
    {
      u_int r0 = LoadIdRefKill(returnIdRefs->Sub(0));
      SET_INSTR_1R(PC,seam_return1,r0);
    }
    break;
  case 2:
    {
      u_int r0 = LoadIdRefKill(returnIdRefs->Sub(0));
      u_int r1 = LoadIdRefKill(returnIdRefs->Sub(1));
      SET_INSTR_2R(PC,seam_return2,r0,r1);
    }
    break;
  case 3:
    {
      u_int r0 = LoadIdRefKill(returnIdRefs->Sub(0));
      u_int r1 = LoadIdRefKill(returnIdRefs->Sub(1));
      u_int r2 = LoadIdRefKill(returnIdRefs->Sub(2));
      SET_INSTR_3R(PC,seam_return3,r0,r1,r2);
    }
    break;
  case 4:
    {
      u_int r0 = LoadIdRefKill(returnIdRefs->Sub(0));
      u_int r1 = LoadIdRefKill(returnIdRefs->Sub(1));
      u_int r2 = LoadIdRefKill(returnIdRefs->Sub(2));
      u_int r3 = LoadIdRefKill(returnIdRefs->Sub(3));
      SET_INSTR_4R(PC,seam_return4,r0,r1,r2,r3);
    }
    break;
  default:
    {
      if(nArgs <= Scheduler::maxArgs) {
	SET_INSTR_1I(PC,seam_set_nargs,nArgs);
	u_int reg;
	for(u_int i=0; i<nArgs; i++) {
	  reg = LoadIdRefKill(returnIdRefs->Sub(i),true);
	  SET_INSTR_1R1I(PC,seam_set_sreg,reg,i);
	}
	SET_INSTR_1I(PC,seam_return,nArgs);
      } else {
	u_int S = GetNewScratch();
	SET_INSTR_1R1I(PC,new_tup,S,nArgs);
	for(u_int i=nArgs; i--; ) {
	  u_int src = LoadIdRefKill(returnIdRefs->Sub(i),true);
	  SET_INSTR_2R1I(PC,init_tup,S,src,i);
	}
	SET_INSTR_1I(PC,seam_return1,S);
      }
    }
  }
  return INVALID_POINTER;
}

void ByteCodeJitter::CompileInstr(TagVal *pc) {
  u_int oldScratch = scratch; // save scratch registers
  while( pc != INVALID_POINTER ) {
    scratch = currentNLocals; // reset scratch registers
    switch (AbstractCode::GetInstr(pc)) {
    case AbstractCode::Kill:
      pc = InstrKill(pc); break;
    case AbstractCode::PutVar:
      pc = InstrPutVar(pc); break;
    case AbstractCode::PutNew:
      pc = InstrPutNew(pc); break;
    case AbstractCode::PutTag:
      pc = InstrPutTag(pc); break;
    case AbstractCode::PutCon:
      pc = InstrPutCon(pc); break;
    case AbstractCode::PutRef:
      pc = InstrPutRef(pc); break;
    case AbstractCode::PutTup:
      pc = InstrPutTup(pc); break;
    case AbstractCode::PutPolyRec:
      pc = InstrPutPolyRec(pc); break;
    case AbstractCode::PutVec:
      pc = InstrPutVec(pc); break;
    case AbstractCode::Close:
      pc = InstrClose(pc); break;
    case AbstractCode::Specialize:
      pc = InstrSpecialize(pc); break;
      // pc = InstrClose(pc); break;
    case AbstractCode::AppPrim:
      pc = InstrAppPrim(pc); break;
    case AbstractCode::AppVar:
      pc = InstrAppVar(pc); break;
    case AbstractCode::GetRef:
      pc = InstrGetRef(pc); break;
    case AbstractCode::GetTup:
      pc = InstrGetTup(pc); break;
    case AbstractCode::Sel: 
      pc = InstrSel(pc); break;
    case AbstractCode::LazyPolySel:
      pc = InstrLazyPolySel(pc); break;
    case AbstractCode::Raise:
      pc = InstrRaise(pc); break;
    case AbstractCode::Reraise:
      pc = InstrReraise(pc); break;
    case AbstractCode::Try:
      pc = InstrTry(pc); break;
    case AbstractCode::EndTry:
      pc = InstrEndTry(pc); break;
    case AbstractCode::EndHandle:
      pc = InstrEndHandle(pc); break;
    case AbstractCode::IntTest:
      pc = InstrIntTest(pc); break;
    case AbstractCode::CompactIntTest:
      pc = InstrCompactIntTest(pc); break;
    case AbstractCode::RealTest:
      pc = InstrRealTest(pc); break;
    case AbstractCode::StringTest:
      pc = InstrStringTest(pc); break;
    case AbstractCode::TagTest:
      pc = InstrTagTest(pc); break;
    case AbstractCode::CompactTagTest:
      pc = InstrCompactTagTest(pc); break;
    case AbstractCode::ConTest:
      pc = InstrConTest(pc); break;
    case AbstractCode::VecTest:
      pc = InstrVecTest(pc); break;
    case AbstractCode::Shared:
      pc = InstrShared(pc); break;
    case AbstractCode::Return:
      pc = InstrReturn(pc); break;
    default:
      Error("ByteCodeJitter::CompileInstr: invalid abstractCode tag");
    }
    scratch += delayedScratchInc;
    delayedScratchInc = 0;
    if(nRegisters < scratch)
      nRegisters = scratch;
  }
  scratch = oldScratch;
}

void ByteCodeJitter::CompileCCC(u_int inArity, Vector *rets) {
  switch(inArity) {
  case 0: 
    break;
  case 1:
    {
      TagVal *idDef = TagVal::FromWord(rets->Sub(0));      
      if(idDef != INVALID_POINTER) {
	u_int dst = IdToReg(idDef->Sel(0));
	if (dst == 0) {
	  SET_INSTR(PC,ccc1);
	} else { 
	  SET_INSTR_1R(PC,seam_ccc1,dst);
	}
      } else {
	SET_INSTR(PC,seam_ccc1_wildcard);
      }
    }
    break;
  default:
    {
      bool isFastCCCN = true;
      for(u_int i=0; i<inArity; i++) {	
	TagVal *idDef = TagVal::FromWord(rets->Sub(i));
	if(idDef == INVALID_POINTER || IdToReg(idDef->Sel(0)) != i) {
	  isFastCCCN = false;
	  break;
	}
      }
      if(isFastCCCN) {
	SET_INSTR_1I(PC,cccn,inArity);
      } else {	  
	SET_INSTR_1I(PC,seam_cccn,inArity);
	for(u_int i=0; i<inArity; i++) {
	  TagVal *idDef = TagVal::FromWord(rets->Sub(i));
	  if(idDef != INVALID_POINTER) {
	    u_int reg = IdToReg(idDef->Sel(0));
	    SET_INSTR_1R1I(PC,seam_load_sreg,reg,i);
	  }
	}
      }
    }
  }
}

u_int invocations = 0;

// Function of coord * value option vector * string vector *
//             idDef args * outArity option * instr * liveness
word ByteCodeJitter::Compile(LazyByteCompileClosure *lazyCompileClosure) {
//   timeval startTime;
//   gettimeofday(&startTime,0);

  BCJIT_DEBUG("start compilation (%d times) ", invocations);
  BCJIT_DEBUG("and compile the following abstract code:\n");
  TagVal *abstractCode = lazyCompileClosure->GetAbstractCode();
  
//   Tuple *coord = Tuple::FromWordDirect(abstractCode->Sel(0));
//   std::fprintf(stderr, "%d. compile function (%p) at %s:%d.%d nArgs=%d\n\n",
// 	       ++invocations, abstractCode->ToWord(),
// 	       String::FromWordDirect(coord->Sel(0))->ExportC(),
// 	       Store::DirectWordToInt(coord->Sel(1)),
// 	       Store::DirectWordToInt(coord->Sel(2)),
// 	       Vector::FromWordDirect(abstractCode->Sel(3))->GetLength()
// 	       ); 
//   AbstractCode::Disassemble(stderr,
// 			    TagVal::FromWordDirect(abstractCode->Sel(5)));
    

  // perform register allocation
  currentNLocals = GetNumberOfLocals(abstractCode);

#ifdef DO_REG_ALLOC
  Vector *liveness = Vector::FromWordDirect(abstractCode->Sel(6));
  u_int local_mapping[currentNLocals];
  mapping = local_mapping;
  RegisterAllocator::Run(liveness,mapping,&currentNLocals);
#endif

  // now prepare scratch registers
  scratch = currentNLocals;
  nRegisters = scratch;
  delayedScratchInc = 0;
  
  // prepare immediate environment
  imEnv.Init();

  // prepare control data structures
  sharedTable = IntMap::New(INITIAL_SHAREDTABLE_SIZE);
 
  // prepare substituation
  Vector *substInfo = Vector::FromWordDirect(abstractCode->Sel(1));
  u_int nSubst      = substInfo->GetLength();
  globalSubst       = Vector::New(nSubst);
  for (u_int i = nSubst; i--;) {
    TagVal *valueOpt = TagVal::FromWord(substInfo->Sub(i));
    TagVal *subst;
    if (valueOpt != INVALID_POINTER) {
      subst = TagVal::New(AbstractCode::Immediate, 1);
      subst->Init(0, valueOpt->Sel(0));
    }
    else {
      subst = TagVal::New(AbstractCode::Global, 1);
      subst->Init(0, Store::IntToWord(i));
    }
    globalSubst->Init(i, subst->ToWord());
  }

  // prepare ByteCode WriteBuffer
  WriteBuffer::Init(); 
  PC = 0;

  // compile calling convention conversion
  Vector *args = Vector::FromWordDirect(abstractCode->Sel(3));
  CompileCCC(args->GetLength(),args);

  // compile function body
  CompileInstr(TagVal::FromWordDirect(abstractCode->Sel(5)));

  // create compiled concrete code
  Chunk *code = WriteBuffer::FlushCode();

  ByteConcreteCode *bcc = 
    ByteConcreteCode::NewInternal(abstractCode,
				  code,
				  imEnv.ExportEnv(),
				  Store::IntToWord(nRegisters));

//   static u_int sumNRegisters = 0;
//   sumNRegisters += nRegisters;
//   fprintf(stderr,"nLocals %d, nRegisters %d, sumNRegisters %d\n",
// 	  currentNLocals,nRegisters,sumNRegisters);

//   static u_int codeSize = 0;
//   codeSize += code->GetSize();
//   fprintf(stderr,"codeSize %d\n",codeSize);
//   fprintf(stderr,"-----------------\ncompiled code:\n");
// #ifdef THREADED
//   ByteCode::Disassemble(stderr,(u_int *)code->GetBase(),code,
// 			Tuple::FromWordDirect(imEnv.ExportEnv()));
// #else
//   ByteCode::Disassemble(stderr,0,code,
// 			Tuple::FromWordDirect(imEnv.ExportEnv()));
// #endif
//   fprintf(stderr,"-------------\n");

//   static double totalTime = 0;
//   timeval stopTime;
//   gettimeofday(&stopTime,0);
//   totalTime += 0.000001 * ((double) (stopTime.tv_usec - startTime.tv_usec))
//     + ((double) (stopTime.tv_sec - startTime.tv_sec));
//   fprintf(stderr,"compile time %f seconds\n",totalTime);

  return bcc->ToWord();
}

