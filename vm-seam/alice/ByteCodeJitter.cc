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
#include "alice/HotSpotConcreteCode.hh"
#include "alice/AbstractCode.hh"
#include "alice/ByteCodeAlign.hh"
#include "alice/ByteCodeBuffer.hh"
#include "alice/ByteCode.hh"
#include "alice/ByteCodeConstProp.hh"
#include "alice/LazySelInterpreter.hh"

#include <sys/time.h>

// switch debug messages on/off

#define BCJIT_DEBUG(s,...) /*fprintf(stderr,s, ##__VA_ARGS__)*/
//#define DEBUG_DISASSEMBLE

#define INSERT_DEBUG_MSG(msg)

// #define INSERT_DEBUG_MSG(msg)  {		\
//     String *s = String::New(msg);		\
//     u_int addr = imEnv.Register(s->ToWord());	\
//     SET_INSTR_1I(PC,debug_msg,addr);		\
//   }

using namespace ByteCodeInstr;

//
// some helper functions
//

void Jitter_PrintLiveness(Vector *liveness) {
  u_int size = liveness->GetLength();
  fprintf(stderr, "size = %"U_INTF"\n", size/3);
  for(u_int i = 0, j = 1; i<size; i+=3, j++) {
    u_int index = Store::DirectWordToInt(liveness->Sub(i));
    u_int start = Store::DirectWordToInt(liveness->Sub(i+1));
    u_int end   = Store::DirectWordToInt(liveness->Sub(i+2));
    fprintf(stderr, "%"U_INTF". %"U_INTF" -> [%"U_INTF", %"U_INTF"]\n", j, index, start, end);
  }
}

static inline u_int GetNumberOfLocals(TagVal *abstractCode) {
  TagVal *annotation = TagVal::FromWordDirect(abstractCode->Sel(2));
  switch (AbstractCode::GetAnnotation(annotation)) {
  case AbstractCode::Simple:
      return Store::DirectWordToInt(annotation->Sel(0));
  case AbstractCode::Debug:
      return Vector::FromWordDirect(annotation->Sel(0))->GetLength();
  }
}

static inline Vector *ShiftIdDefs(Vector *srcs, s_int offset) {
  u_int size = srcs->GetLength();
  Vector *dsts = Vector::New(size);
  for(u_int i = size; i--; ) {
    TagVal *argOpt = TagVal::FromWord(srcs->Sub(i));
    if(argOpt != INVALID_POINTER) {
      u_int id = Store::DirectWordToInt(argOpt->Sel(0)) + offset;
      TagVal *newOpt = TagVal::New(Types::SOME,1);
      newOpt->Init(0,Store::IntToWord(id));
      dsts->Init(i,newOpt->ToWord());
    } else {
      dsts->Init(i,srcs->Sub(i));
    }
  }
  return dsts;
}

inline word ByteCodeJitter::ExtractImmediate(word idRef) {
  TagVal *tagVal = TagVal::FromWordDirect(idRef);

  if (AbstractCode::GetIdRef(tagVal) == AbstractCode::Global)
    tagVal = LookupSubst(Store::DirectWordToInt(tagVal->Sel(0)));

  switch (AbstractCode::GetIdRef(tagVal)) {
  case AbstractCode::Immediate:
    return tagVal->Sel(0);
  default:
    return INVALID_POINTER;		       
  }
}

ByteCodeJitter::PatchTable::PatchTable() : size(20), top(0) { 
  table = new u_int[size]; 
}

u_int ByteCodeJitter::PatchTable::jumpInstrSize;

void ByteCodeJitter::PatchTable::Init() {
  WriteBuffer::Init(10);
  u_int pc = 0;
  SET_INSTR_1I(pc,jump,0);
  jumpInstrSize = pc;
}

//
// Register Allocation
//

#ifdef DO_REG_ALLOC
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
  static void Run(Vector *liveness, u_int mapping[], 
		  u_int *nLocals, u_int offset = 0) {
    MinHeap heap(*nLocals);
    u_int regs = offset;
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

//
// peephole optimizer
//

class PeepHoleOptimizer {
public:
  // This function optimizes a common case that occurs in procedure
  // inlining:
  // - The callee expects unit as argument, but only uses
  //   it to request the unit value.
  // - The caller does not pass a value, so the CCC has to insert a 
  //   load_zero instruction.
  // In this case it is obvious that we can skip both the load_zero
  // instruction and the following request on it.
  static TagVal *optimizeInlineInCCC(Vector *formalArgs, Vector *args,
				     TagVal *instr) {
    if(formalArgs->GetLength() == 1 && args->GetLength() == 0) {
      if(AbstractCode::GetInstr(instr) == AbstractCode::GetTup) {
	Vector *regs = Vector::FromWordDirect(instr->Sel(0));
	if(regs->GetLength() == 0) {
	  TagVal *idRef = TagVal::FromWordDirect(instr->Sel(1));
	  switch(AbstractCode::GetIdRef(idRef)) {
	  case AbstractCode::LastUseLocal:
	    {
	      // this case does not seem to occur
	      return TagVal::FromWordDirect(instr->Sel(2));
	    }
	    break;
	  case AbstractCode::Local:
	    {
	      word local = idRef->Sel(0);
	      instr = TagVal::FromWordDirect(instr->Sel(2));
	      if(AbstractCode::GetInstr(instr) == AbstractCode::Kill) {
		Vector *regs = Vector::FromWordDirect(instr->Sel(0));
		for(u_int i = regs->GetLength(); i--; ) {
		  if(regs->Sub(i) == local) {
		    return TagVal::FromWordDirect(instr->Sel(1));
		  }
		}
	      }
	    }
	    break;
	  default:
	    ;
	  }
	}
      }
    }
    return INVALID_POINTER;
  }

  static bool optimizeReturnUnit(Vector *args) {
    if(args->GetLength() == 1) {
      TagVal *tagVal = TagVal::FromWordDirect(args->Sub(0));    
//       if (AbstractCode::GetIdRef(tagVal) == AbstractCode::Global)
// 	tagVal = LookupSubst(Store::DirectWordToInt(tagVal->Sel(0)));
      switch (AbstractCode::GetIdRef(tagVal)) {
      case AbstractCode::Immediate:
	{
	  word val = tagVal->Sel(0);
	  return (PointerOp::IsInt(val) && Store::DirectWordToInt(val) == 0);
	}
      default:
	;
      }
    }
    return false;
  }
};

//
// helpers to load arguments into registers
//

void ByteCodeJitter::LoadIdRefInto(u_int dst, word idRef) {
  TagVal *tagVal = TagVal::FromWordDirect(idRef);

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
	u_int  index = imEnv.Register(val);
	SET_INSTR_1R1I(PC,load_immediate,dst,index);
      }
    }
    break;
  default:
    Error("ByteCodeJitter::LoadIdRef: invalid idRef Tag");
  }
}

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
}

ByteCodeJitter::ByteCodeJitter() {
}

ByteCodeJitter::~ByteCodeJitter() {
}

//
// inlining of primitives
//

void *ByteCodeJitter::inlineTable[INLINE_TABLE_SIZE];

void ByteCodeJitter::Init() {
  PatchTable::Init();

  // prepare inlineTable
#define INIT_INLINE_TABLE(primitiveName,primitiveNumber) {		\
  Chunk *name = reinterpret_cast<Chunk *>(String::New(primitiveName));			\
  word value = PrimitiveTable::LookupValue(name);			\
  Closure *closure = Closure::FromWordDirect(value);			\
  ConcreteCode *concreteCode =						\
    ConcreteCode::FromWord(closure->GetConcreteCode());			\
  Interpreter *interpreter = concreteCode->GetInterpreter();		\
  inlineTable[primitiveNumber] = reinterpret_cast<void *>(interpreter->GetCFunction());	\
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

#define DEFINE_PRIMITIVE_RETURN_REG(X)					\
  u_int X;								\
  if(inlineDepth > 0 && currentFormalArgs != INVALID_POINTER) {		\
    TagVal *argOpt = TagVal::FromWord(currentFormalArgs->Sub(0));	\
    X = (argOpt != INVALID_POINTER) ?					\
      IdToReg(argOpt->Sel(0)) : GetNewScratch();			\
  } else								\
    X = GetNewScratch();


inline void ByteCodeJitter::InlinePrimitiveReturn(u_int reg) {
#ifdef DO_INLINING
  if(inlineDepth > 0 && currentFormalArgs != INVALID_POINTER) {
    u_int nFormals = currentFormalArgs->GetLength();
    switch(nFormals) {
    case 0:
      break;
    case 1:
      {
	TagVal *argOpt = TagVal::FromWord(currentFormalArgs->Sub(0));
	if(argOpt != INVALID_POINTER) {
	  u_int dst = IdToReg(argOpt->Sel(0));
 	  if(dst != reg) {
	    SET_INSTR_2R(PC,load_reg,dst,reg);
 	  }	
	}  
      }
      break;
    default:
      for(u_int i = nFormals; i--; ) {
	TagVal *argOpt = TagVal::FromWord(currentFormalArgs->Sub(i));
	if(argOpt != INVALID_POINTER) {
	  u_int dst = IdToReg(argOpt->Sel(0));
	  SET_INSTR_2R1I(PC,select_tup,dst,reg,i);
	}	  
      }
    };
    patchTable->Add(PC);    
    SET_INSTR_1I(PC,jump,0);
  } else {    
    SET_INSTR_1R(PC,seam_return1,reg);
  }
#else
  SET_INSTR_1R(PC,seam_return1,reg);
#endif
}

inline void ByteCodeJitter::Inline_HoleHole(Vector *args, 
					    TagVal *idDefInstrOpt) {
  BCJIT_DEBUG("inline Hole.hole\n");
  if(args->GetLength() == 1) { // request unit      
    SET_INSTR_1R(PC,await,GetNewScratch());
  }
  if(idDefInstrOpt == INVALID_POINTER) { // tailcall
    DEFINE_PRIMITIVE_RETURN_REG(S);
    SET_INSTR_1R(PC,inlined_hole_hole,S);
    InlinePrimitiveReturn(S);
    return;
  }
  Tuple *idDefInstr = Tuple::FromWordDirect(idDefInstrOpt->Sel(0));
  TagVal *idDef = TagVal::FromWord(idDefInstr->Sel(0));
  if(idDef != INVALID_POINTER) {
    u_int dst = IdToReg(idDef->Sel(0));
    SET_INSTR_1R(PC,inlined_hole_hole,dst);
  }
}

inline void ByteCodeJitter::Inline_HoleFill(Vector *args, 
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
    DEFINE_PRIMITIVE_RETURN_REG(S);
    SET_INSTR_1R(PC,load_zero,S);
    InlinePrimitiveReturn(S);
    return;
  } 
  Tuple *idDefInstr = Tuple::FromWordDirect(idDefInstrOpt->Sel(0));
  TagVal *idDef = TagVal::FromWord(idDefInstr->Sel(0));
  if(idDef != INVALID_POINTER) {
    u_int ret = IdToReg(idDef->Sel(0));
    SET_INSTR_1R(PC,load_zero,ret);
  }
}

inline void ByteCodeJitter::Inline_FutureAwait(Vector *args, 
					       TagVal *idDefInstrOpt) {
  BCJIT_DEBUG("inline Future.await\n");
  u_int arg = LoadIdRefKill(args->Sub(0));
  SET_INSTR_1R(PC,await,arg);
  if(idDefInstrOpt == INVALID_POINTER) {
    InlinePrimitiveReturn(arg);
    return;
  }
  Tuple *idDefInstr = Tuple::FromWordDirect(idDefInstrOpt->Sel(0));
  TagVal *ret = TagVal::FromWord(idDefInstr->Sel(0));
  if(ret != INVALID_POINTER) {
    u_int dst = IdToReg(ret->Sel(0));
    if(dst != arg)
      SET_INSTR_2R(PC,load_reg,dst,arg);
  }
}

inline void ByteCodeJitter::Inline_FutureByneed(Vector *args, 
						TagVal *idDefInstrOpt) {
  BCJIT_DEBUG("inline Future.byneed\n");
  if(args->GetLength() != 1) { // create a tuple and pass this
    Error("Uups! BCJitter: Future.byneed didn't get 1 arg\n");
  }
  u_int src = LoadIdRefKill(args->Sub(0));
  if(idDefInstrOpt == INVALID_POINTER) { // tailcall
    DEFINE_PRIMITIVE_RETURN_REG(S);
    SET_INSTR_2R(PC,inlined_future_byneed,S,src);
    InlinePrimitiveReturn(S);
    return;
    } 
  Tuple *idDefInstr = Tuple::FromWordDirect(idDefInstrOpt->Sel(0));
  TagVal *idDef = TagVal::FromWord(idDefInstr->Sel(0));
  if(idDef != INVALID_POINTER) {
    u_int dst = IdToReg(idDef->Sel(0));
    SET_INSTR_2R(PC,inlined_future_byneed,dst,src);
  }
}


/*inline*/ void ByteCodeJitter::Inline_IntPlus(Vector *args, 
					       TagVal *idDefInstrOpt) {
  // TODO: we might need a CCC
  
  // try first to compile the special case
  word wY = ExtractImmediate(args->Sub(1));
  if(wY != INVALID_POINTER && Store::DirectWordToInt(wY) == 1) {
    u_int x = LoadIdRefKill(args->Sub(0));
    if(idDefInstrOpt == INVALID_POINTER) { // tailcall
      SET_INSTR_2R(PC,iinc,x,x);
      InlinePrimitiveReturn(x);
      return;
    } 
    Tuple *idDefInstr = Tuple::FromWord(idDefInstrOpt->Sel(0));
    TagVal *ret0 = TagVal::FromWord(idDefInstr->Sel(0));
    u_int S = (ret0 == INVALID_POINTER) ? 
      GetNewScratch() : IdToReg(ret0->Sel(0));
    SET_INSTR_2R(PC,iinc,S,x);
    return; 
  }

  word wX = ExtractImmediate(args->Sub(0));
  if(wX != INVALID_POINTER && Store::DirectWordToInt(wX) == 1) {
    u_int y = LoadIdRefKill(args->Sub(1));
    if(idDefInstrOpt == INVALID_POINTER) { // tailcall
      SET_INSTR_2R(PC,iinc,y,y);
      InlinePrimitiveReturn(y);
      return;
    } 
    Tuple *idDefInstr = Tuple::FromWord(idDefInstrOpt->Sel(0));
    TagVal *ret0 = TagVal::FromWord(idDefInstr->Sel(0));
    u_int S = (ret0 == INVALID_POINTER) ? 
      GetNewScratch() : IdToReg(ret0->Sel(0));
    SET_INSTR_2R(PC,iinc,S,y);
    return; 
  }
  
  // normal case
  u_int x = LoadIdRefKill(args->Sub(0));
  u_int y = LoadIdRefKill(args->Sub(1));
  if(idDefInstrOpt == INVALID_POINTER) { // tailcall
    DEFINE_PRIMITIVE_RETURN_REG(S);
    SET_INSTR_3R(PC,iadd,S,x,y);
    InlinePrimitiveReturn(S);
    return;
  } 
  Tuple *idDefInstr = Tuple::FromWord(idDefInstrOpt->Sel(0));
  TagVal *ret0 = TagVal::FromWord(idDefInstr->Sel(0));
  u_int dst = (ret0 == INVALID_POINTER) ? 
    GetNewScratch() : IdToReg(ret0->Sel(0));
  SET_INSTR_3R(PC,iadd,dst,x,y);
}

/*inline*/ void ByteCodeJitter::Inline_IntMinus(Vector *args, 
						TagVal *idDefInstrOpt) {
  // TODO: we might need a CCC

  // try first to compile the special case

  word wY = ExtractImmediate(args->Sub(1));
  if(wY != INVALID_POINTER && Store::DirectWordToInt(wY) == 1) {
    u_int x = LoadIdRefKill(args->Sub(0));
    if(idDefInstrOpt == INVALID_POINTER) { // tailcall
      SET_INSTR_2R(PC,idec,x,x);
      InlinePrimitiveReturn(x);
      return;
    } 
    Tuple *idDefInstr = Tuple::FromWord(idDefInstrOpt->Sel(0));
    TagVal *ret0 = TagVal::FromWord(idDefInstr->Sel(0));
    u_int S = (ret0 == INVALID_POINTER) ? 
      GetNewScratch() : IdToReg(ret0->Sel(0));
    SET_INSTR_2R(PC,idec,S,x);
    return; 
  }

  word wX = ExtractImmediate(args->Sub(0));
  if(wX != INVALID_POINTER && Store::DirectWordToInt(wX) == 1) {
    u_int y = LoadIdRefKill(args->Sub(1));
    if(idDefInstrOpt == INVALID_POINTER) { // tailcall
      SET_INSTR_2R(PC,idec,y,y);
      InlinePrimitiveReturn(y);
      return;
    } 
    Tuple *idDefInstr = Tuple::FromWord(idDefInstrOpt->Sel(0));
    TagVal *ret0 = TagVal::FromWord(idDefInstr->Sel(0));
    u_int S = (ret0 == INVALID_POINTER) ? 
      GetNewScratch() : IdToReg(ret0->Sel(0));
    SET_INSTR_2R(PC,idec,S,y);
    return; 
  }
  
  // normal case
  u_int x = LoadIdRefKill(args->Sub(0));
  u_int y = LoadIdRefKill(args->Sub(1));
  if(idDefInstrOpt == INVALID_POINTER) { // tailcall
    DEFINE_PRIMITIVE_RETURN_REG(S);
    SET_INSTR_3R(PC,isub,S,x,y);
    InlinePrimitiveReturn(S);
    return;
  } 
  Tuple *idDefInstr = Tuple::FromWordDirect(idDefInstrOpt->Sel(0));
  TagVal *ret0 = TagVal::FromWord(idDefInstr->Sel(0));
  u_int dst = (ret0 == INVALID_POINTER) ? 
    GetNewScratch() : IdToReg(ret0->Sel(0));
  SET_INSTR_3R(PC,isub,dst,x,y);
}

inline void ByteCodeJitter::Inline_RefAssign(Vector *args, 
					     TagVal *idDefInstrOpt) {
  // TODO: we might need a CCC
  u_int ref = LoadIdRefKill(args->Sub(0));
  u_int val = LoadIdRefKill(args->Sub(1));
  SET_INSTR_2R(PC,set_cell,ref,val);
  if(idDefInstrOpt == INVALID_POINTER) { // tailcall
    // try to avoid load zero instruction
    if(inlineDepth > 0 && currentFormalArgs != INVALID_POINTER) {
      TagVal *argOpt = TagVal::FromWord(currentFormalArgs->Sub(0));
      if(argOpt != INVALID_POINTER) {
	u_int dst = IdToReg(argOpt->Sel(0));
	SET_INSTR_1R(PC,load_zero,dst);
      }              
      patchTable->Add(PC);    
      SET_INSTR_1I(PC,jump,0);
      return;
    }
    u_int S = GetNewScratch();
    SET_INSTR_1R(PC,load_zero,S);
    InlinePrimitiveReturn(S); // return unit
    return;
  }
  Tuple *idDefInstr = Tuple::FromWordDirect(idDefInstrOpt->Sel(0));
  TagVal *ret0 = TagVal::FromWord(idDefInstr->Sel(0));
  if(ret0 != INVALID_POINTER) {
    u_int dst = IdToReg(ret0->Sel(0));
    SET_INSTR_1R(PC,load_zero,dst);
  }
}

bool ByteCodeJitter::InlinePrimitive(void *cFunction, 
				     Vector *args, 
				     TagVal *idDefInstrOpt) {
  if(cFunction == inlineTable[HOLE_HOLE]) { 
    Inline_HoleHole(args,idDefInstrOpt);
    return true;
  } 
  if(cFunction == inlineTable[HOLE_FILL]) {
    Inline_HoleFill(args,idDefInstrOpt);
    return true;
  }
  if(cFunction == inlineTable[FUTURE_AWAIT]) {
    Inline_FutureAwait(args,idDefInstrOpt);
    return true;
  }
  if(cFunction == inlineTable[FUTURE_BYNEED]) {
    Inline_FutureByneed(args,idDefInstrOpt);
    return true;
  }
  if(cFunction == inlineTable[INT_PLUS]) {
    Inline_IntPlus(args,idDefInstrOpt);
    return true;
  }
  if(cFunction == inlineTable[INT_MINUS]) {
    Inline_IntMinus(args,idDefInstrOpt);
    return true;
  }
  if(cFunction == inlineTable[REF_ASSIGN]) {
    Inline_RefAssign(args,idDefInstrOpt);
    return true;
  }
  return false;
}

//
// compilation of AbstractCode instructions
//

// Kill of id vector * instr
inline TagVal *ByteCodeJitter::InstrKill(TagVal *pc) {
  // use this information in later versions of the compiler
  return TagVal::FromWordDirect(pc->Sel(1));
}
 
// PutVar of id * idRef  * instr
inline TagVal *ByteCodeJitter::InstrPutVar(TagVal *pc) {
  u_int dst = IdToReg(pc->Sel(0));
  LoadIdRefInto(dst,pc->Sel(1));
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
  for(u_int i=size; i--; ) {
    u_int reg = LoadIdRefKill(idRefs->Sub(i),true);
    SET_INSTR_2R1I(PC,init_con,dst,reg,i);
  }

  return TagVal::FromWordDirect(pc->Sel(3));
}

// PutTag of id * int * int * idRef vector * instr 
TagVal *ByteCodeJitter::InstrPutTag(TagVal *pc) {
  u_int maxTag = Store::DirectWordToInt(pc->Sel(1));
  u_int tag  = Store::DirectWordToInt(pc->Sel(2));  

  u_int tagInstr = (Alice::IsBigTagVal(maxTag)) ? 
    new_bigtagval_init : new_tagval_init;

  Vector *idRefs = Vector::FromWordDirect(pc->Sel(3));
  u_int nargs = idRefs->GetLength();
  u_int dst = IdToReg(pc->Sel(0));

  // load arguments
  u_int regs[nargs];
  for (u_int i = 0; i<nargs; i++) {
    u_int reg = LoadIdRefKill(idRefs->Sub(i));
    regs[i] = reg;
  }
  // set instruction
  if(Alice::IsBigTagVal(maxTag)) {
    switch(nargs) {
    case 1:  SET_INSTR_1R1I(PC,new_bigtagval_init1,dst,tag); break;
    case 2:  SET_INSTR_1R1I(PC,new_bigtagval_init2,dst,tag); break;
    case 3:  SET_INSTR_1R1I(PC,new_bigtagval_init3,dst,tag); break;
    case 4:  SET_INSTR_1R1I(PC,new_bigtagval_init4,dst,tag); break;
    default: SET_INSTR_1R2I(PC,new_bigtagval_init,dst,tag,nargs);    
    }
  } else {
    switch(nargs) {
    case 1:  SET_INSTR_1R1I(PC,new_tagval_init1,dst,tag); break;
    case 2:  SET_INSTR_1R1I(PC,new_tagval_init2,dst,tag); break;
    case 3:  SET_INSTR_1R1I(PC,new_tagval_init3,dst,tag); break;
    case 4:  SET_INSTR_1R1I(PC,new_tagval_init4,dst,tag); break;
    default: SET_INSTR_1R2I(PC,new_tagval_init,dst,tag,nargs);    
    }
  }
  // set arguments
  for(u_int i = nargs; i--; ) {
    u_int reg = regs[i];
    SET_1R(PC,reg);
  }
  
  return TagVal::FromWordDirect(pc->Sel(4));

//   u_int maxTag = Store::DirectWordToInt(pc->Sel(1));
//   u_int tag  = Store::DirectWordToInt(pc->Sel(2));  
//   u_int newtagInstr;
//   u_int inittagInstr;

//   if (Alice::IsBigTagVal(maxTag)) {
//     newtagInstr  = new_bigtagval;
//     inittagInstr = init_bigtagval;
//   } else {
//     newtagInstr  = new_tagval;
//     inittagInstr = init_tagval;
//   }

//   Vector *idRefs = Vector::FromWordDirect(pc->Sel(3));
//   u_int nargs = idRefs->GetLength();
//   u_int dst = IdToReg(pc->Sel(0));

//   SET_INSTR_1R2I(PC,newtagInstr,dst,nargs,tag);    
//   for (u_int i = nargs; i--; ) {
//     u_int reg = LoadIdRefKill(idRefs->Sub(i),true);
//     SET_INSTR_2R1I(PC,inittagInstr,dst,reg,i);
//   }
  
//   return TagVal::FromWordDirect(pc->Sel(4));
}

// PutRef of id * idRef * instr
inline TagVal *ByteCodeJitter::InstrPutRef(TagVal *pc) {
  u_int dst = IdToReg(pc->Sel(0));
  u_int src = LoadIdRefKill(pc->Sel(1));
  SET_INSTR_2R(PC,new_cell,dst,src);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// PutTup of id * idRef vector * instr
void ByteCodeJitter::NewTup(u_int dst, Vector *idRefs) {
  u_int size = idRefs->GetLength();
  switch(size) {
  case 2:
    {
      u_int r0 = LoadIdRefKill(idRefs->Sub(0));
      u_int r1 = LoadIdRefKill(idRefs->Sub(1));
      SET_INSTR_3R(PC,new_pair,dst,r0,r1);
    }
    break;
  case 3:
    {
      u_int r0 = LoadIdRefKill(idRefs->Sub(0));
      u_int r1 = LoadIdRefKill(idRefs->Sub(1));
      u_int r2 = LoadIdRefKill(idRefs->Sub(2));
      SET_INSTR_4R(PC,new_triple,dst,r0,r1,r2);
    }
    break;
 default:
   {
     SET_INSTR_1R1I(PC,new_tup,dst,size);
     for(u_int i=size; i--; ) {
       u_int reg = LoadIdRefKill(idRefs->Sub(i),true);
       SET_INSTR_2R1I(PC,init_tup,dst,reg,i);
     }
   }
  }
}
inline TagVal *ByteCodeJitter::InstrPutTup(TagVal *pc) {
  u_int dst = IdToReg(pc->Sel(0));
  Vector *idRefs = Vector::FromWordDirect(pc->Sel(1));
  NewTup(dst,idRefs);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// PutPolyRec of id * label vector * idRef vector * instr       
inline TagVal *ByteCodeJitter::InstrPutPolyRec(TagVal *pc) {
  u_int dst = IdToReg(pc->Sel(0));
  u_int labelsAddr = imEnv.Register(pc->Sel(1));
  SET_INSTR_1R1I(PC,new_polyrec,dst,labelsAddr);

  Vector *idRefs = Vector::FromWordDirect(pc->Sel(2));
  u_int size = idRefs->GetLength();
  for(u_int i = size; i--; ) {
    u_int reg = LoadIdRefKill(idRefs->Sub(i),true);
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
  for(u_int i = size; i--; ) {
    u_int reg = LoadIdRefKill(idRefs->Sub(i),true);
    SET_INSTR_2R1I(PC,init_vec,dst,reg,i);
  }
  
  return TagVal::FromWordDirect(pc->Sel(2));
}

// Close of id * idRef vector * template * instr
TagVal *ByteCodeJitter::InstrClose(TagVal *pc) {
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
// On runtime every value of the global environment is directly 
// substituted into the code. This means that every time such a closure
// is really needed, new code must be compiled.
TagVal *ByteCodeJitter::InstrSpecialize(TagVal *pc) {
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
  for (u_int i = nGlobals; i--; ) {
    u_int src = LoadIdRefKill(globalRefs->Sub(i),true);
    SET_INSTR_2R1I(PC,init_closure,dst,src,i);
  }
  return TagVal::FromWordDirect(pc->Sel(3));
}

#define CHOOSE_CALL_INSTR(instr,instr_prefix)				\
  switch(nActualArgs) {							\
  case 0:								\
    instr = isTailcall ? instr_prefix##_tailcall0 : instr_prefix##_call0; \
    break;								\
  case 1:								\
    instr = isTailcall ? instr_prefix##_tailcall1 : instr_prefix##_call1; \
    break;								\
  case 2:								\
    instr = isTailcall ? instr_prefix##_tailcall2 : instr_prefix##_call2; \
    break;								\
  case 3:								\
    instr =								\
      isTailcall ? instr_prefix##_tailcall3 : instr_prefix##_call3;	\
	break;								\
  default:								\
    instr =								\
      isTailcall ? instr_prefix##_tailcall : instr_prefix##_call;	\
  }

// AppPrim of value * idRef vector * (idDef * instr) option   
void ByteCodeJitter::CompileApplyPrimitive(Closure *closure, 
					   Vector *args, 
					   bool isTailcall) { 
  u_int nArgs = args->GetLength();
  ConcreteCode *concreteCode = 
    ConcreteCode::FromWord(closure->GetConcreteCode());		
  Interpreter *interpreter = concreteCode->GetInterpreter();

  // we can directly store the address in the code as chunks are not GC'd
  u_int interpreterAddr = reinterpret_cast<u_int>(interpreter);

  u_int inArity = interpreter->GetInArity(concreteCode);
  u_int argRegs[inArity];
  bool overflow = nArgs > Scheduler::maxArgs;

  u_int callInstr;
  u_int nActualArgs;

  if(!overflow) {
    // The CCC for primitives is normally done in  primitive interpreter->Run.
    // As we want to skip this and directly call the primitive, we must
    // compile the CCC manually.
    switch(inArity) {
    case 0:
      {
	if(nArgs == 1) { // request unit argument
	  u_int arg0 = LoadIdRefKill(args->Sub(0));
	  SET_INSTR_1R(PC,await,arg0);
	}
      }
      break;
    case 1:
      {
	u_int dst;
	switch(nArgs) {
	case 0:
	  {
	    dst = GetNewScratch();
	    SET_INSTR_1R(PC,load_zero,dst);
	  } 
	  break;
	case 1:
	  {
	    dst = LoadIdRefKill(args->Sub(0));
	  }
	  break;
	default: // nArgs > 1 --> construct tuple
	  {
	    dst = GetNewScratch();
	    NewTup(dst,args);
	  }
	}
	argRegs[0] = dst;
      }
      break;
    default:
      {
	if(nArgs == 1) { // deconstruct tuple
	  u_int tup = LoadIdRefKill(args->Sub(0));
	  for(u_int i=inArity; i--; ) {
	    u_int dst = GetNewScratch();
	    SET_INSTR_2R1I(PC,select_tup,dst,tup,i);
	    argRegs[i] = dst;
	  }
	} else {
	  Assert(nArgs == inArity);
	  for(u_int i = nArgs; i--; )
	    argRegs[i] = LoadIdRefKill(args->Sub(i));
	}
      }
    }
    // generate call instruction
    nActualArgs = inArity;
    CHOOSE_CALL_INSTR(callInstr,seam_prim);
  } else { // overflow
    // If there are more arguments than registers in the global register bank,
    // we cannot hardwire the CCC into the code. Therefore, we have to use a 
    // normal immediate call instead of a primitive call, and pack all 
    // arguments into one tuple.
    u_int S = GetNewScratch();
    NewTup(S, args);
    argRegs[0] = S;
    nActualArgs = 1;
    // use closure instead of interpreter address
    interpreterAddr = imEnv.Register(closure->ToWord());
    CHOOSE_CALL_INSTR(callInstr,immediate);
  }
   
  if(nActualArgs < 4) {
    SET_INSTR_1I(PC,callInstr,interpreterAddr);
  } else {
    SET_INSTR_2I(PC,callInstr,interpreterAddr,nActualArgs);
  }
  for(u_int i = nActualArgs; i--; ) {
    u_int r = argRegs[i];
    SET_1R(PC,r);
  }
}


// AppPrim of value * idRef vector * (idDef * instr) option   
inline TagVal *ByteCodeJitter::InstrAppPrim(TagVal *pc) {
  Closure *closure = Closure::FromWordDirect(pc->Sel(0));
  Vector *args = Vector::FromWordDirect(pc->Sel(1));
  TagVal *idDefInstrOpt = TagVal::FromWord(pc->Sel(2));
#ifdef DO_INLINING
  bool isTailcall = (idDefInstrOpt == INVALID_POINTER
		     && !(inlineDepth > 0 && 
			  currentFormalArgs != INVALID_POINTER));
#else
  bool isTailcall = (idDefInstrOpt == INVALID_POINTER);
#endif
  // try first to inline some frequent primitives
  ConcreteCode *concreteCode = 
    ConcreteCode::FromWord(closure->GetConcreteCode());		
  Interpreter *interpreter = concreteCode->GetInterpreter();
  u_int outArity = interpreter->GetOutArity(concreteCode);
  void *cFunction = reinterpret_cast<void*>(interpreter->GetCFunction());

  // check if we can inline the primitive
  if(InlinePrimitive(cFunction,args,idDefInstrOpt)) {
    if(isTailcall) {
      return INVALID_POINTER;
    } else {
      Tuple *idDefInstr = Tuple::FromWordDirect(idDefInstrOpt->Sel(0));
      return TagVal::FromWordDirect(idDefInstr->Sel(1));
    }
  }
  
  // compile normal primitive call
  CompileApplyPrimitive(closure,args,isTailcall);

#ifdef DO_INLINING
  if(idDefInstrOpt == INVALID_POINTER
     && inlineDepth > 0 && currentFormalArgs != INVALID_POINTER) {
    u_int inArity = currentFormalArgs->GetLength();
    CompileCCC(currentFormalArgs,outArity);
    patchTable->Add(PC);    
    SET_INSTR_1I(PC,jump,0);
    return INVALID_POINTER;
  }
#endif
  if(isTailcall) 
    return INVALID_POINTER;

  // non-tailcall
  Tuple *idDefInstr = Tuple::FromWordDirect(idDefInstrOpt->Sel(0));
  Vector *rets = Vector::New(1);
  rets->Init(0,idDefInstr->Sel(0));
  CompileCCC(rets,outArity); // TODO: optimize if we can skip CCC
  // compile continuation
  return TagVal::FromWordDirect(idDefInstr->Sel(1)); 
}

void ByteCodeJitter::CompileSelfCall(TagVal *instr, bool isTailcall) {
  Assert(AbstractCode::GetInstr(instr) == AbstractCode::AppVar);
  Vector *args = Vector::FromWordDirect(instr->Sel(1));
  u_int nArgs = args->GetLength();

  if(isTailcall) {
    CompileInlineCCC(currentFormalInArgs,args,false);
    u_int oldPC = PC;
    SET_INSTR_1I(PC,check_preempt_jump,0);
    s_int offset = skipCCCPC - PC;
    SET_INSTR_1I(oldPC,check_preempt_jump,offset);
    return;
  }

  bool overflow = nArgs > Scheduler::maxArgs;
  u_int nActualArgs = overflow ? 1 : nArgs;
  u_int argRegs[nActualArgs];
    
  // load arguments into registers
    
  if(overflow) { // construct tuple
    u_int S = GetNewScratch();
    NewTup(S, args);
    argRegs[0] = S;
  } else {
    for(u_int i = nArgs; i--; )
      argRegs[i] = LoadIdRefKill(args->Sub(i));
  }
    
  // generate call instruction
  u_int callInstr;
  switch(nActualArgs) {
  case 0:  callInstr = self_call0; break;
  case 1:  callInstr = self_call1; break;
  case 2:  callInstr = self_call2; break;
  case 3:  callInstr = self_call3; break;
  default: callInstr = self_call;
  }
  if(nActualArgs < 4) {
    SET_INSTR(PC,callInstr);
  } else {
    SET_INSTR_1I(PC,callInstr,nActualArgs);
  }
  for(u_int i = nActualArgs; i--; ) {
    u_int r = argRegs[i];
    SET_1R(PC,r);
  }
}

// AppVar of idRef * idRef vector * bool * (idDef vector * instr) option
/*inline*/ TagVal *ByteCodeJitter::InstrAppVar(TagVal *pc) {
  Vector *args = Vector::FromWordDirect(pc->Sel(1));
  u_int nArgs = args->GetLength();
  TagVal *idDefsInstrOpt = TagVal::FromWord(pc->Sel(3));
  s_int outArity = INVALID_INT;

#ifdef DO_INLINING
  bool isTailcall = (idDefsInstrOpt == INVALID_POINTER
		     && !(inlineDepth > 0 && 
			  currentFormalArgs != INVALID_POINTER));
#else
  bool isTailcall = (idDefsInstrOpt == INVALID_POINTER);
#endif

#ifdef DO_INLINING
      // check if the call can be inlined
      Map *inlineMap = inlineInfo->GetInlineMap();
      if(inlineMap->IsMember(pc->ToWord())) {
	Tuple *tup = Tuple::FromWordDirect(inlineMap->Get(pc->ToWord()));
	TagVal *abstractCode = TagVal::FromWordDirect(tup->Sel(0));
	// break compile cycle (check moved to ByteCodeInliner)
	Vector *subst = Vector::FromWordDirect(tup->Sel(1));
	u_int offset = Store::DirectWordToInt(tup->Sel(2));
	InlineInfo *info = InlineInfo::FromWordDirect(tup->Sel(3));
	TagVal *continuation = 
	  CompileInlineFunction(abstractCode,info,subst,
				offset,args,idDefsInstrOpt);
	if(inlineDepth > 0 && idDefsInstrOpt == INVALID_POINTER) {
	  patchTable->Add(PC);    
	  SET_INSTR_1I(PC,jump,0);
	}
	return continuation;
      }
#endif

  TagVal *tagVal = TagVal::FromWordDirect(pc->Sel(0));
  if (AbstractCode::GetIdRef(tagVal) == AbstractCode::Global)
    tagVal = LookupSubst(Store::DirectWordToInt(tagVal->Sel(0)));  

  switch (AbstractCode::GetIdRef(tagVal)) {
  case AbstractCode::Immediate:
    {
      BCJIT_DEBUG("AppVar: entered immediate\n");
      word wClosure = tagVal->Sel(0);
      // extract the closure
      Closure *closure;
      while ((closure = Closure::FromWord(wClosure)) == INVALID_POINTER) {
	Transient *transient = Store::WordToTransient(wClosure);
	if ((transient != INVALID_POINTER) &&
	(transient->GetLabel() == BYNEED_LABEL)) {
	  Closure *byneedClosure = 
	    static_cast<Byneed *>(transient)->GetClosure();
	  ConcreteCode *concreteCode =
	    ConcreteCode::FromWord(byneedClosure->GetConcreteCode());
	  // select from lazy-sel closure
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
	break;
      }
      // check which kind of immediate closure we got
      if(closure != INVALID_POINTER) {
	word wConcreteCode = closure->GetConcreteCode();	
	if(wConcreteCode == currentConcreteCode) {
	  // this is a self recursive call
	  CompileSelfCall(pc,isTailcall);
	  outArity = currentOutArity;
	  if(isTailcall)
	    return INVALID_POINTER;
	  else
	    goto compile_continuation;
	}
	ConcreteCode *concreteCode = ConcreteCode::FromWord(wConcreteCode);
	if(concreteCode != INVALID_POINTER) {
	  Interpreter *interpreter = concreteCode->GetInterpreter();
	  outArity = interpreter->GetOutArity(concreteCode);
	  void *cFunction = reinterpret_cast<void*>(interpreter->GetCFunction());
	  // check if this is a primitive
	  if(cFunction != NULL) { 
	    // try to inline some other common primitives
	    // prepare args for InlinePrimitive
	    TagVal *continuation;
	    TagVal *idDefInstrOpt;
	    if(idDefsInstrOpt == INVALID_POINTER) {
	      idDefInstrOpt = INVALID_POINTER;
	    } else {		
	      Tuple *idDefsInstr = 
		Tuple::FromWordDirect(idDefsInstrOpt->Sel(0));
	      Vector *rets = Vector::FromWordDirect(idDefsInstr->Sel(0));
	      if(rets->GetLength() != 1) {
		// We cannot inline this anyway, as we only know how to inline
		// primitives that return one value
		idDefInstrOpt = INVALID_POINTER;
	      } else {
		idDefInstrOpt = TagVal::New(idDefsInstrOpt->GetTag(),2);
		Tuple *tuple = Tuple::New(2);
		tuple->Init(0,rets->Sub(0));
		tuple->Init(1,idDefsInstr->Sel(1));
		idDefInstrOpt->Init(0,tuple->ToWord());
	      }
	    }
	    if(InlinePrimitive(cFunction,args,idDefInstrOpt)) {
	      if(idDefInstrOpt == INVALID_POINTER) {
		return INVALID_POINTER;
	      } else {		
		Tuple *idDefInstr = 
		  Tuple::FromWordDirect(idDefInstrOpt->Sel(0));
		return TagVal::FromWordDirect(idDefInstr->Sel(1));
	      }
	    }
	    // else: couldn't inline primitve; try somthing else
	    // we directly call the primitive function
	    CompileApplyPrimitive(closure,args,isTailcall);
#ifdef DO_INLINING
	    //	    if(isTailcall) {
	    if(idDefInstrOpt == INVALID_POINTER
	       && inlineDepth > 0 && currentFormalArgs != INVALID_POINTER) {
	      CompileCCC(currentFormalArgs,outArity);
	      patchTable->Add(PC);    
	      SET_INSTR_1I(PC,jump,0);
	      continuation = INVALID_POINTER;
	    }
#endif
	    if(idDefsInstrOpt == INVALID_POINTER) {
// 	    if(isTailcall) {
	      continuation = INVALID_POINTER;
	    } else {
	      Tuple *idDefsInstr = 
		Tuple::FromWordDirect(idDefsInstrOpt->Sel(0));
	      Vector *rets = Vector::FromWordDirect(idDefsInstr->Sel(0));
	      CompileCCC(rets,outArity); 
	      continuation = TagVal::FromWordDirect(idDefsInstr->Sel(1));
	    } 
	    return continuation;
	  } else {
	    // ok, it wasn't a primitive
	    // but we are sure that is an immediate calls
	    // so we can use specialized call instructions

	    bool overflow = nArgs > Scheduler::maxArgs;
	    u_int nActualArgs = overflow ? 1 : nArgs;
	    u_int argRegs[nActualArgs];
	    
	    // load arguments into registers
	    
	    if(overflow) { // construct tuple
	      u_int S = GetNewScratch();
	      NewTup(S, args);
	      argRegs[0] = S;
	    } else {
	      for(u_int i = nArgs; i--; )
		argRegs[i] = LoadIdRefKill(args->Sub(i));
	    }

	    // generate call instruction
	    u_int callInstr;	    
	    if (interpreter == ByteCodeInterpreter::self) {
	      // byte code call	    	      
	      CHOOSE_CALL_INSTR(callInstr,bci);
	    } 
	    else if (interpreter == HotSpotInterpreter::self) {
	      // immediate call that might change into a byte code call
	      CHOOSE_CALL_INSTR(callInstr,rewrite);
	    } else {
	      // none byte code call that is immediate
	      CHOOSE_CALL_INSTR(callInstr,immediate);
	    }
	    // ATTENTION: It is essential to transform the dereferenced closure
	    // back to word. The idea is that the jitter dereferences once and
	    // interpreter can thus access the closure argument with
	    // Closure::FromWordDirect
	    u_int closureAddr = imEnv.Register(closure->ToWord());	      
	    if(nActualArgs < 4) {
	      SET_INSTR_1I(PC,callInstr,closureAddr);
	    } else {
	      SET_INSTR_2I(PC,callInstr,closureAddr,nActualArgs);
	    }
	    for(u_int i = nActualArgs; i--; ) {
	      u_int r = argRegs[i];
	      SET_1R(PC,r);
	    }

	    goto compile_continuation;
	  }
	}
      }
    }
    break;
  default:
    ;
  }

  // standard call instruction with dynamic tests
  {
    bool overflow = nArgs > Scheduler::maxArgs;
    u_int nActualArgs = overflow ? 1 : nArgs;
    u_int argRegs[nActualArgs];
    
    // load arguments into registers
    
    if(overflow) { // construct tuple
      u_int S = GetNewScratch();
      NewTup(S, args);
      argRegs[0] = S;
    } else {
      for(u_int i = nArgs; i--; )
	argRegs[i] = LoadIdRefKill(args->Sub(i));
    }
    u_int closure = LoadIdRefKill(pc->Sel(0));
    
    // generate call instruction
    u_int callInstr;
    CHOOSE_CALL_INSTR(callInstr,seam);
    if(nActualArgs < 4) {
      SET_INSTR_1R(PC,callInstr,closure);
    } else {
      SET_INSTR_1R1I(PC,callInstr,closure,nActualArgs);
    }
    for(u_int i = nActualArgs; i--; ) {
      u_int r = argRegs[i];
      SET_1R(PC,r);
    }
  } 

 compile_continuation: // nice hack ;-)
#ifdef DO_INLINING
  if(idDefsInstrOpt == INVALID_POINTER
     && inlineDepth > 0 && currentFormalArgs != INVALID_POINTER) {
    u_int inArity = currentFormalArgs->GetLength();
    CompileCCC(currentFormalArgs,outArity);
    patchTable->Add(PC);    
    SET_INSTR_1I(PC,jump,0);
    INSERT_DEBUG_MSG("return from tailcall");
    return INVALID_POINTER;
  }
#endif
  if(isTailcall) 
    return INVALID_POINTER;
  // non-tailcall
  Tuple *idDefsInstr = Tuple::FromWordDirect(idDefsInstrOpt->Sel(0));
  Vector *rets = Vector::FromWordDirect(idDefsInstr->Sel(0));
  CompileCCC(rets,outArity); 

  return TagVal::FromWordDirect(idDefsInstr->Sel(1)); // compile continuation
}

// GetRef of id * idRef * instr
inline TagVal *ByteCodeJitter::InstrGetRef(TagVal *pc) {
  u_int dst = IdToReg(pc->Sel(0));
  u_int src = LoadIdRefKill(pc->Sel(1));
  SET_INSTR_2R(PC,load_cell,dst,src);
  return TagVal::FromWordDirect(pc->Sel(2));
}

// GetTup of idDef vector * idRef * instr
/*inline*/ TagVal *ByteCodeJitter::InstrGetTup(TagVal *pc) {
  Vector *idDefs = Vector::FromWordDirect(pc->Sel(0));
  u_int size = idDefs->GetLength();
  u_int src = LoadIdRefKill(pc->Sel(1));
  // try to use special case instructions
  switch(size) {
  case 0: // this is a request to unit
    {
      SET_INSTR_1R(PC,await,src);
      return TagVal::FromWordDirect(pc->Sel(2));
    }
//   case 1:
//     {
//       TagVal *idDef = TagVal::FromWord(idDefs->Sub(0));
//       if(idDef != INVALID_POINTER) {
// 	u_int dst = IdToReg(idDef->Sel(0));
// 	SET_INSTR_2R(PC,select_tup1,dst,src); 
//       }
//       return TagVal::FromWordDirect(pc->Sel(2));
//     }
  case 2:
    {
      TagVal *idDef1 = TagVal::FromWord(idDefs->Sub(0));
      TagVal *idDef2 = TagVal::FromWord(idDefs->Sub(1));
      if(idDef1 != INVALID_POINTER && idDef2 != INVALID_POINTER) {
	u_int dst1 = IdToReg(idDef1->Sel(0));
	u_int dst2 = IdToReg(idDef2->Sel(0));
	SET_INSTR_3R(PC,get_tup2,dst1,dst2,src);
	return TagVal::FromWordDirect(pc->Sel(2));
      }
    }
    break;
  case 3:
    {
      TagVal *idDef1 = TagVal::FromWord(idDefs->Sub(0));
      TagVal *idDef2 = TagVal::FromWord(idDefs->Sub(1));
      TagVal *idDef3 = TagVal::FromWord(idDefs->Sub(2));
      if(idDef1 != INVALID_POINTER && idDef2 != INVALID_POINTER 
	 && idDef3 != INVALID_POINTER) {
	u_int dst1 = IdToReg(idDef1->Sel(0));
	u_int dst2 = IdToReg(idDef2->Sel(0));
	u_int dst3 = IdToReg(idDef3->Sel(0));
	SET_INSTR_4R(PC,get_tup3,dst1,dst2,dst3,src);
	return TagVal::FromWordDirect(pc->Sel(2));
      }
    }
    break;
  default:
    ;
  }
  // pointwise selection
  // use specialized instructions first
  u_int index = size - 1;
  for(; index>2; index--) {
    TagVal *idDef = TagVal::FromWord(idDefs->Sub(index));
    if(idDef != INVALID_POINTER) {
      u_int dst = IdToReg(idDef->Sel(0));
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
    TagVal *idDef = TagVal::FromWord(idDefs->Sub(2));
    if(idDef != INVALID_POINTER) {
      u_int dst = IdToReg(idDef->Sel(0));
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
    TagVal *idDef = TagVal::FromWord(idDefs->Sub(1));
    if(idDef != INVALID_POINTER) {
      u_int dst = IdToReg(idDef->Sel(0));
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
    TagVal *idDef = TagVal::FromWord(idDefs->Sub(0));
    if(idDef != INVALID_POINTER) {
      u_int dst = IdToReg(idDef->Sel(0));
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
TagVal *ByteCodeJitter::InstrIntTest(TagVal *pc) {
  u_int testVal = LoadIdRefKill(pc->Sel(0));
  Vector *tests = Vector::FromWordDirect(pc->Sel(1));
  u_int nTests = tests->GetLength();
  // profiling suggested that "ijump" cannot be used in most cases as nTests>1
  IntMap *map = IntMap::New(2 * nTests);
  u_int mapAddr = imEnv.Register(map->ToWord());
  SET_INSTR_1R1I(PC,itest,testVal,mapAddr);
  u_int instrPC = PC;
  CompileInstr(TagVal::FromWordDirect(pc->Sel(2))); // compile else branch
  for (u_int i = 0; i<nTests; i++) {
    Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
    map->Put(pair->Sel(0),Store::IntToWord(PC - instrPC));
    CompileInstr(TagVal::FromWordDirect(pair->Sel(1))); // compile branch
  }
  return INVALID_POINTER;
}

// CompactIntTest of idRef * int * instrs * instr 
TagVal *ByteCodeJitter::InstrCompactIntTest(TagVal *pc) {
  u_int testVal = LoadIdRefKill(pc->Sel(0));
  u_int offset  = Store::DirectWordToInt(pc->Sel(1));
  Vector *jumpTable = Vector::FromWordDirect(pc->Sel(2));
  u_int size = jumpTable->GetLength();
  
  // try to optimize the test with specialized instructions
  if(size == 1) {
    u_int ijumpInstrPC = PC;
    SET_INSTR_1R2I(PC,ijump_eq,0,0,0);                  // create dummy code
    u_int jmpPC = PC;
    CompileInstr(TagVal::FromWordDirect(pc->Sel(3)));   // compile else branch
    SET_INSTR_1R2I(ijumpInstrPC,ijump_eq,testVal,offset,PC-jmpPC);
    return TagVal::FromWordDirect(jumpTable->Sub(0));   // compile then part
  }
  
  // size > 1
  SET_INSTR_1R2I(PC,citest,testVal,offset,size);
  u_int jumpTablePC = PC;
  u_int jmpPC = PC;
  PC += size;
  CompileInstr(TagVal::FromWordDirect(pc->Sel(3))); // compile else branch
  for(u_int i=0; i<size; i++) {
    SET_1I(jumpTablePC,PC - jmpPC);
    CompileInstr(TagVal::FromWordDirect(jumpTable->Sub(i))); // compile branch
  }

  return INVALID_POINTER;
}

// RealTest of idRef * (real * instr) vector * instr
TagVal *ByteCodeJitter::InstrRealTest(TagVal *pc) {
  u_int testVal = LoadIdRefKill(pc->Sel(0));
  Vector *tests = Vector::FromWordDirect(pc->Sel(1));
  u_int nTests = tests->GetLength();

  // try to optimize the test with specialized instructions
  if(nTests == 1) { 
    u_int rjumpInstrPC = PC;
    SET_INSTR_1R2I(PC,rjump_eq,0,0,0); // create dummy code
    u_int jmpPC = PC;
    CompileInstr(TagVal::FromWordDirect(pc->Sel(2))); // compile else branch
    Tuple *pair = Tuple::FromWordDirect(tests->Sub(0));
    u_int valAddr = imEnv.Register(pair->Sel(0));
    SET_INSTR_1R2I(rjumpInstrPC,rjump_eq,testVal,valAddr,PC-jmpPC);
    return TagVal::FromWordDirect(pair->Sel(1)); // compile then part
  }

  // nTests > 1
  ChunkMap *map = ChunkMap::New(2 * nTests);
  u_int mapAddr = imEnv.Register(map->ToWord());
  SET_INSTR_1R1I(PC,rtest,testVal,mapAddr);
  u_int instrPC = PC;
  CompileInstr(TagVal::FromWordDirect(pc->Sel(2))); // compile else branch
  for (u_int i = 0; i<nTests; i++) {
    Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
    map->Put(pair->Sel(0),Store::IntToWord(PC - instrPC));
    CompileInstr(TagVal::FromWordDirect(pair->Sel(1))); // compile branch
  }
  return INVALID_POINTER;
}

// StringTest of idRef * (string * instr) vector * instr 
TagVal *ByteCodeJitter::InstrStringTest(TagVal *pc) {
  u_int testVal = LoadIdRefKill(pc->Sel(0));
  Vector *tests = Vector::FromWordDirect(pc->Sel(1));
  u_int nTests = tests->GetLength();

  // try to optimize the test with specialized instructions
  if(nTests == 1) { 
    u_int sjumpInstrPC = PC;
    SET_INSTR_1R2I(PC,sjump_eq,0,0,0); // create dummy code
    u_int jmpPC = PC;
    CompileInstr(TagVal::FromWordDirect(pc->Sel(2))); // compile else branch
    Tuple *pair = Tuple::FromWordDirect(tests->Sub(0));
    u_int valAddr = imEnv.Register(pair->Sel(0));
    SET_INSTR_1R2I(sjumpInstrPC,sjump_eq,testVal,valAddr,PC-jmpPC);
    return TagVal::FromWordDirect(pair->Sel(1)); // compile then part
  }

  // nTests > 1
  ChunkMap *map = ChunkMap::New(2 * nTests);
  u_int mapAddr = imEnv.Register(map->ToWord());
  SET_INSTR_1R1I(PC,stest,testVal,mapAddr);
  u_int instrPC = PC;
  CompileInstr(TagVal::FromWordDirect(pc->Sel(2))); // compile else branch
  for (u_int i = 0; i<nTests; i++) {
    Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
    map->Put(pair->Sel(0),Store::IntToWord(PC - instrPC));
    CompileInstr(TagVal::FromWordDirect(pair->Sel(1))); // compile branch
  }
  return INVALID_POINTER;
}

// helper to compile tagval selection
void ByteCodeJitter::LoadTagVal(u_int testVal, Vector *idDefs, bool isBig) {
  u_int idDefsLength = idDefs->GetLength();
  switch(idDefsLength) {
  case 1: 
    {
      u_int loadInstr = isBig ? load_bigtagval1 : load_tagval1;
      TagVal *idDef = TagVal::FromWord(idDefs->Sub(0));
      if(idDef != INVALID_POINTER) { // no wildcard
	u_int dst = IdToReg(idDef->Sel(0));
	SET_INSTR_2R(PC,loadInstr,dst,testVal);	  
      }
      return;
    }
  case 2:
    {
      u_int loadInstr = isBig ? load_bigtagval2 : load_tagval2;
      TagVal *idDef1 = TagVal::FromWord(idDefs->Sub(0));
      TagVal *idDef2 = TagVal::FromWord(idDefs->Sub(1));
      if(idDef1 != INVALID_POINTER && idDef2 != INVALID_POINTER) {
	u_int dst1 = IdToReg(idDef1->Sel(0));
	u_int dst2 = IdToReg(idDef2->Sel(0));
	SET_INSTR_3R(PC,loadInstr,dst1,dst2,testVal);
	return;
      }
    }
    break; // goto default translation
  case 3:
    {
      u_int loadInstr = isBig ? load_bigtagval3 : load_tagval3;
      TagVal *idDef1 = TagVal::FromWord(idDefs->Sub(0));
      TagVal *idDef2 = TagVal::FromWord(idDefs->Sub(1));
      TagVal *idDef3 = TagVal::FromWord(idDefs->Sub(2));
      if(idDef1 != INVALID_POINTER && 
	 idDef2 != INVALID_POINTER &&
	 idDef3 != INVALID_POINTER) {
	u_int dst1 = IdToReg(idDef1->Sel(0));
	u_int dst2 = IdToReg(idDef2->Sel(0));
	u_int dst3 = IdToReg(idDef3->Sel(0));
	SET_INSTR_4R(PC,loadInstr,dst1,dst2,dst3,testVal);
	return;
      }
    }
    break; // goto default translation
  default:
    ;
  }
  // default translation
  u_int loadInstr = isBig ? load_bigtagval : load_tagval;
  for(u_int i = idDefsLength; i--; ) {
    TagVal *idDef = TagVal::FromWord(idDefs->Sub(i));
    if(idDef != INVALID_POINTER) { // no wildcard
      u_int dst = IdToReg(idDef->Sel(0));
#ifdef DO_REG_ALLOC
      // check if the test value is overwritten although it is still needed
      if(dst == testVal && i > 0) {
	u_int S = GetNewScratch();
	SET_INSTR_2R(PC,load_reg,S,testVal);
	testVal = S;
      }
#endif
      SET_INSTR_2R1I(PC,loadInstr,dst,testVal,i);
    }
  }
}

// TagTest of idRef * int * (int * instr) vector
//          * (int * idDef vector * instr) vector * instr   
TagVal *ByteCodeJitter::InstrTagTest(TagVal *pc) {
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
    u_int jmpPC = PC;
    CompileInstr(TagVal::FromWordDirect(pc->Sel(4))); // compile else branch  
    if(nullarySize == 1) {
      Tuple *pair = Tuple::FromWordDirect(nullaryTests->Sub(0));
      u_int tag = Store::DirectWordToInt(pair->Sel(0));
      SET_INSTR_1R2I(patchInstrPC,testInstr,testVal,tag,PC-jmpPC);
      return TagVal::FromWordDirect(pair->Sel(1)); // compile branch   
    } else if (narySize == 1) {
      Tuple *triple = Tuple::FromWordDirect(naryTests->Sub(0));
      u_int tag = Store::DirectWordToInt(triple->Sel(0));
      SET_INSTR_1R2I(patchInstrPC,testInstr,testVal,tag,PC-jmpPC);
      // compile binding
      Vector *idDefs = Vector::FromWordDirect(triple->Sel(1));
      LoadTagVal(testVal,idDefs,isBigTag);
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
    u_int instrPC = PC;
    CompileInstr(TagVal::FromWordDirect(pc->Sel(4))); // compile else branch  
    
    // compile nullary tests
    for(u_int i=0; i<nullarySize; i++) {
      Tuple *pair = Tuple::FromWordDirect(nullaryTests->Sub(i));
      map->Put(pair->Sel(0),Store::IntToWord(PC - instrPC));
      CompileInstr(TagVal::FromWordDirect(pair->Sel(1))); // compile branch
    }
    
    // compile n-ary tests
    for(u_int i = narySize; i--; ) {
      Tuple *triple = Tuple::FromWordDirect(naryTests->Sub(i));
      map->Put(triple->Sel(0),Store::IntToWord(PC - instrPC));
      // compile binding
      Vector *idDefs = Vector::FromWordDirect(triple->Sel(1));
      LoadTagVal(testVal,idDefs,isBigTag);
      CompileInstr(TagVal::FromWordDirect(triple->Sel(2))); // compile branch
    }
  } else { // fast test
    u_int testInstr = isBigTag ? cbigtagtest_direct : ctagtest_direct;
    u_int loadInstr = isBigTag ? load_bigtagval : load_tagval;

    SET_INSTR_1R1I(PC,testInstr,testVal,maxTag);
    u_int jumpTablePC = PC;
    u_int jmpPC = PC;
    PC += maxTag;
    u_int elsePC = PC;
    u_int defaultTablePC = jumpTablePC;
    for(u_int i=0; i<maxTag; i++) {
      SET_1I(defaultTablePC,elsePC - jmpPC);
    }
    CompileInstr(TagVal::FromWordDirect(pc->Sel(4))); // compile else branch  
    
    // compile nullary tests
    for(u_int i = nullarySize; i--; ) {
      Tuple *pair = Tuple::FromWordDirect(nullaryTests->Sub(i));
      u_int index = jumpTablePC + Store::DirectWordToInt(pair->Sel(0));
      SET_1I(index,PC-jmpPC);
      CompileInstr(TagVal::FromWordDirect(pair->Sel(1))); // compile branch
    }
    
    // compile n-ary tests
    for(u_int i=narySize; i--; ) {
      Tuple *triple = Tuple::FromWordDirect(naryTests->Sub(i));
      u_int index = jumpTablePC + Store::DirectWordToInt(triple->Sel(0));
      SET_1I(index,PC-jmpPC);
      // compile binding
      Vector *idDefs = Vector::FromWordDirect(triple->Sel(1));
      LoadTagVal(testVal,idDefs,isBigTag);
      CompileInstr(TagVal::FromWordDirect(triple->Sel(2))); // compile branch
    }
  }
 
  return INVALID_POINTER;
} 

// TODO: check for optimizations
// CompactTagTest of idRef * int * tagTests * instr option  
/*inline*/ TagVal *ByteCodeJitter::InstrCompactTagTest(TagVal *pc) {
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
  bool isBigTag = Alice::IsBigTagVal(maxTag);
  if(isBigTag) {
    loadInstr = load_bigtagval;
    testInstr = isFastTest ? cbigtagtest_direct : cbigtagtest;
  }
  else {
    loadInstr = load_tagval;
    testInstr = isFastTest ? ctagtest_direct : ctagtest;
  }
#ifdef DO_CONSTANT_PROPAGATION
  // check first if constant propagation has found out something interesting
  if(tagtestInfo->IsMember(pc->ToWord())) {
    Tuple *pair = Tuple::FromWordDirect(tagtestInfo->Get(pc->ToWord()));
    fprintf(stderr,"-> compiler could eliminate tagtest %p, next %p\n",
	    pc,pair->Sel(1));
    // compile binding
    TagVal *idDefsOpt = TagVal::FromWord(pair->Sel(0));
    if(idDefsOpt != INVALID_POINTER) {
      Vector *idDefs = Vector::FromWordDirect(idDefsOpt->Sel(0));
      LoadTagVal(testVal, idDefs,isBigTag);
    }
    return TagVal::FromWordDirect(pair->Sel(1));
  }
#endif
  // this is the normal case
  // create needed datastructures
  u_int newTestsSize = isFastTest ? maxTag : size;
  SET_INSTR_1R1I(PC,testInstr,testVal,newTestsSize);
  u_int jumpTablePC = PC;
  u_int jmpPC = PC;
  PC += newTestsSize;
  u_int elsePC = PC;

  if(isSomeElseInstr) { // compile else branch  
    CompileInstr(TagVal::FromWordDirect(elseInstrOpt->Sel(0))); 
  }

  // compile tests
  for(u_int i=0; i<size; i++) {
    Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
    SET_1I(jumpTablePC,PC - jmpPC);    
    // compile binding
    TagVal *idDefsOpt = TagVal::FromWord(pair->Sel(0));
    if(idDefsOpt != INVALID_POINTER) {
      Vector *idDefs = Vector::FromWordDirect(idDefsOpt->Sel(0));
      LoadTagVal(testVal, idDefs, isBigTag);
    }
    CompileInstr(TagVal::FromWordDirect(pair->Sel(1))); // compile branch
  }
  // fill the rest with the else PC
  for(u_int i=size; i<newTestsSize; i++) {
    SET_1I(jumpTablePC,elsePC-jmpPC);
  }
  return INVALID_POINTER;  
}


// Contest of idRef * (idRef * instr) vector 
//          * (idRef * idDef vector * instr) vector * instr    
TagVal *ByteCodeJitter::InstrConTest(TagVal *pc) {
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
    u_int jmpPC = PC;
    CompileInstr(TagVal::FromWordDirect(pair->Sel(1)));    // compile branch
    SET_INSTR_2R1I(conTestInstrPC,contest,testVal,src,PC-jmpPC);
  }

  // compile n-ary tests
  for(u_int i=0; i<narySize; i++) {
    Tuple *triple = Tuple::FromWordDirect(naryTests->Sub(i));
    u_int src = LoadIdRefKill(triple->Sel(0),true);
    u_int conTestInstrPC = PC;
    SET_INSTR_2R1I(PC,contest,0,0,0);                      // dummy instr
    u_int jmpPC = PC;
    // compile binding
    Vector *idDefs = Vector::FromWordDirect(triple->Sel(1));
    u_int idDefsLength = idDefs->GetLength();
    u_int testValReg = testVal;
    for (u_int j = idDefsLength; j--; ) {
      TagVal *idDef = TagVal::FromWord(idDefs->Sub(j));
      if( idDef != INVALID_POINTER ) { // not wildcard
	u_int dst = IdToReg(idDef->Sel(0));
#ifdef DO_REG_ALLOC
	  if(dst == testValReg && j > 0) {
	    u_int S = GetNewScratch();
	    SET_INSTR_2R(PC,load_reg,S,testValReg);
	    testValReg = S;
	  }
#endif
	SET_INSTR_2R1I(PC,load_con,dst,testValReg,j);
      }
    }
    CompileInstr(TagVal::FromWordDirect(triple->Sel(2)));  // compile branch
    SET_INSTR_2R1I(conTestInstrPC,contest,testVal,src,PC-jmpPC);
  }
  
  return TagVal::FromWordDirect(pc->Sel(3));
}

// VecTest of idRef * (idDef vector * instr) vector * instr  
TagVal *ByteCodeJitter::InstrVecTest(TagVal *pc) {
  u_int testVal = LoadIdRefKill(pc->Sel(0));
  Vector *tests = Vector::FromWordDirect(pc->Sel(1));
  u_int nTests = tests->GetLength();
  IntMap *map = IntMap::New(2 * nTests);
  u_int mapAddr = imEnv.Register(map->ToWord());
  SET_INSTR_1R1I(PC,vectest,testVal,mapAddr);
  u_int instrPC = PC;
  CompileInstr(TagVal::FromWordDirect(pc->Sel(2))); // compile else branch
  for (u_int i = 0; i<nTests; i++) {
    Tuple *pair    = Tuple::FromWordDirect(tests->Sub(i));
    Vector *idDefs = Vector::FromWordDirect(pair->Sel(0));
    word key       = Store::IntToWord(idDefs->GetLength());
    map->Put(key, Store::IntToWord(PC - instrPC));
    // compile binding
    u_int idDefsLength = idDefs->GetLength();
    u_int src = testVal;
    for (u_int j = idDefsLength; j--; ) {
      TagVal *idDef = TagVal::FromWord(idDefs->Sub(j));
      if( idDef != INVALID_POINTER ) { // not wildcard
	u_int dst = IdToReg(idDef->Sel(0));
#ifdef DO_REG_ALLOC
	if(dst == testVal && j > 0) {
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
    u_int instrPC = PC;
    SET_INSTR_1I(PC,jump,0);
    s_int offset = target - PC;
    SET_INSTR_1I(instrPC,jump,offset); // patch
    return INVALID_POINTER;
  }
  else {
    // PC is the address of the instr after Shared
    sharedTable->Put(stamp,Store::IntToWord(PC)); 
    return TagVal::FromWordDirect(pc->Sel(1));
  }
}

// Return of idRef vector
TagVal *ByteCodeJitter::InstrReturn(TagVal *pc) {
  Vector *returnIdRefs = Vector::FromWordDirect(pc->Sel(0));
  u_int nArgs = returnIdRefs->GetLength();
#ifdef DO_INLINING
  // compile exit of an inlined function
  if(inlineDepth > 0 && currentFormalArgs != INVALID_POINTER) {
    CompileInlineCCC(currentFormalArgs,returnIdRefs,true);
    patchTable->Add(PC);    
    SET_INSTR_1I(PC,jump,0);
    INSERT_DEBUG_MSG("inlined return");
    return INVALID_POINTER;
  }
#endif
  // normal return
  bool overflow = nArgs > Scheduler::maxArgs;
  u_int nActualArgs = overflow ? 1 : nArgs;
  u_int argRegs[nActualArgs];
    
  // load arguments into registers
    
  if(overflow) { // construct tuple
    u_int S = GetNewScratch();
    NewTup(S, returnIdRefs);
    argRegs[0] = S;
  } else {
    // check for specialized return instruction
    if(PeepHoleOptimizer::optimizeReturnUnit(returnIdRefs)) {
      SET_INSTR(PC,seam_return_zero);
      return INVALID_POINTER;
    }
    // load arguments into registers
    for(u_int i = nArgs; i--; )
      argRegs[i] = LoadIdRefKill(returnIdRefs->Sub(i));
  }
    
  // generate instruction
  u_int instr;
  switch(nActualArgs) {
  case 0:  instr = seam_return0; break;
  case 1:  instr = seam_return1; break;
  case 2:  instr = seam_return2; break;
  case 3:  instr = seam_return3; break;
  default: instr = seam_return;
  }
  if(nActualArgs < 4) {
    SET_INSTR(PC,instr);
  } else {
    SET_INSTR_1I(PC,instr,nActualArgs);
  }
  for(u_int i = nActualArgs; i--; ) {
    u_int r = argRegs[i];
    SET_1R(PC,r);
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

void ByteCodeJitter::CompileCCC(Vector *rets, s_int outArity) {
  u_int inArity = rets->GetLength();
  bool match = 
    outArity != INVALID_INT && 
    inArity == outArity &&
    outArity <= Scheduler::maxArgs;
  switch(inArity) {
  case 0: 
    break;
  case 1:
    {
      TagVal *idDef = TagVal::FromWord(rets->Sub(0));      
      if(idDef != INVALID_POINTER) {
	u_int dst = IdToReg(idDef->Sel(0));
	if (dst == 0) {
	  u_int instr = match ? get_arg0_direct : ccc1;
	  SET_INSTR(PC,instr);
	} else { 
	  u_int instr = match ? get_arg0 : seam_ccc1;
	  SET_INSTR_1R(PC,instr,dst);
	}
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
	u_int instr = match ? get_args_direct : cccn;
	SET_INSTR_1I(PC,instr,inArity);
      } else {	  
	Vector *args = Vector::New(inArity);
	u_int argsAddr = imEnv.Register(args->ToWord());
	u_int instr = match ? get_args : seam_cccn;
	SET_INSTR_1I(PC,instr,argsAddr);
	for(u_int i=0; i<inArity; i++) {
 	  TagVal *idDef = TagVal::FromWord(rets->Sub(i));
 	  if(idDef != INVALID_POINTER) {
 	    u_int reg = IdToReg(idDef->Sel(0));
	    TagVal *argOpt = TagVal::New(Types::SOME,1);
	    argOpt->Init(0,Store::IntToWord(reg));
	    args->Init(i,argOpt->ToWord());
 	  } else
	    args->Init(i,Store::IntToWord(Types::NONE));
	}
      }
    }
  }
}


class AssignmentMarker {
public:
  enum {CYCLE, LEAF};
};

class UIntSet {
protected:
  bool *set;
  u_int size;
public:
  UIntSet(u_int x) : size(x) {
    set = new bool[size];
    for(u_int i=size; i--; )
      set[i] = false;    
  }
  ~UIntSet() { delete [] set; }
  void Put(u_int key) { set[key] = true; }
  bool IsMember(u_int key) { return set[key]; }
};
class UIntMap : private UIntSet {
private:
  word *map;
public:
  using UIntSet::IsMember;
  UIntMap(u_int x) : UIntSet(x) { map = new word[size]; }
  ~UIntMap() { delete [] map; }
  void Put(u_int key, word item) { UIntSet::Put(key); map[key] = item; }
  word Get(u_int key) { return map[key]; }
  void Print() {
    for(u_int i=0; i<size; i++)
      if(IsMember(i))
	fprintf(stderr, " %"U_INTF" -> %p\n", i, map[i]);
  }
};

void ByteCodeJitter::CompileInlineCCC(Vector *formalArgs, 
				      Vector *args, bool isReturn=false) {
  u_int nFormalArgs = formalArgs->GetLength();
  u_int nArgs = args->GetLength();
  // argument match
  // this is the complicated case, as we have to consider for example
  // cycles in assignment
  if(nFormalArgs == nArgs) {
    // treat special cases efficiently
    switch(nArgs) {
    case 0:
      return;
    case 1:
      {
	TagVal *argOpt = TagVal::FromWord(formalArgs->Sub(0));
	if(argOpt != INVALID_POINTER) {
	  u_int dst = IdToReg(argOpt->Sel(0));
	  LoadIdRefInto(dst,args->Sub(0));
	}
	return;
      }
    default:
      ; // continue with complicated case
    }
    u_int size = 0;
    Vector *assignmentChains = Vector::New(3*nArgs);
    // build the chains
    UIntMap definedBy(currentNLocals);
    UIntMap usedBy(currentNLocals);
    UIntSet visited(currentNLocals);
    for(u_int i=0; i<nArgs; i++) {
      TagVal *argOpt = TagVal::FromWord(formalArgs->Sub(i));
      if(argOpt != INVALID_POINTER) {
	u_int dst = IdToReg(argOpt->Sel(0));
	TagVal *tagVal = TagVal::FromWordDirect(args->Sub(i));
	u_int idRefTag = AbstractCode::GetIdRef(tagVal);
	if(idRefTag == AbstractCode::Local ||
	   idRefTag == AbstractCode::LastUseLocal) {
	  u_int src = IdToReg(tagVal->Sel(0));
	  if(src != dst) {
	    definedBy.Put(dst, tagVal->ToWord());	
	    usedBy.Put(src,Store::IntToWord(dst));
	  } else {
	    visited.Put(dst);
	  }
	} else {
	  definedBy.Put(dst, tagVal->ToWord());	
	}
      }
    }
    for(u_int i=0; i<nArgs; i++) {
      TagVal *argOpt = TagVal::FromWord(formalArgs->Sub(i));
      if(argOpt != INVALID_POINTER) {
	u_int dst = IdToReg(argOpt->Sel(0));
	if(visited.IsMember(dst))
	  continue;
	else
	  visited.Put(dst);
	TagVal *tagVal = TagVal::FromWordDirect(args->Sub(i));
	// create an empty chain
	u_int bottom = nArgs;
	u_int top = nArgs;
	Vector *chain = Vector::New(2*nArgs + 1);
	// walk through the use chain
	u_int key = dst;
	bool skipDefChains = false;
	while(usedBy.IsMember(key)) {
	  key = Store::DirectWordToInt(usedBy.Get(key));
	  chain->Init(--bottom,Store::IntToWord(key));
	  visited.Put(key);
	  if(key == dst) { // cycle found, don't walk through def chains
	    TagVal *marker = TagVal::New(AssignmentMarker::CYCLE,0);
	    chain->Init(top,marker->ToWord());
	    skipDefChains = true;
	    break;
	  }
	}	    
	// walk through the definition chain
	if(!skipDefChains) {
	  key = dst;
	  if(definedBy.IsMember(key)) {
	    for(;;) {
	      TagVal *tagVal = TagVal::FromWordDirect(definedBy.Get(key));
	      u_int idRefTag = AbstractCode::GetIdRef(tagVal);
	      if(idRefTag == AbstractCode::Local ||
		 idRefTag == AbstractCode::LastUseLocal) {
		u_int id = IdToReg(tagVal->Sel(0));
		if(id == key)
		  break;
		if(definedBy.IsMember(id)) {
		  key = id;
		  chain->Init(++top,Store::IntToWord(key));
		  visited.Put(key);	    
		  continue;
		}
	      }
	      // stop chain here
	      TagVal *marker = TagVal::New(AssignmentMarker::LEAF,1);
	      marker->Init(0,tagVal->ToWord());
	      chain->Init(++top,marker->ToWord());
	      break;
	    }
	  }
	  if(top > nArgs) {
	    chain->Init(nArgs,Store::IntToWord(dst));
	  } else {
	    TagVal *idRef = TagVal::New(AbstractCode::Local,1);
	    idRef->Init(0,Store::IntToWord(key));
	    TagVal *marker = TagVal::New(AssignmentMarker::LEAF,1);
	    marker->Init(0,idRef->ToWord());
	    chain->Init(top,marker->ToWord());
	  }
	}
	// register the new chain
	if(top-bottom > 0) { // only append non empty chains
	  assignmentChains->Init(size++,Store::IntToWord(bottom));
	  assignmentChains->Init(size++,Store::IntToWord(top));
	  assignmentChains->Init(size++,chain->ToWord());	  
	}
      }
    }
    for(u_int i = 0; i<size; i+=3) {
      u_int bottom = Store::DirectWordToInt(assignmentChains->Sub(i));
      u_int top = Store::DirectWordToInt(assignmentChains->Sub(i+1));
      Vector *chain = Vector::FromWordDirect(assignmentChains->Sub(i+2));
      TagVal *marker = TagVal::FromWordDirect(chain->Sub(top));
      // cycle, e.g. x<-y, y<-z, z<-x 
      switch(marker->GetTag()) {
      case AssignmentMarker::CYCLE:
	{
	  u_int startId = Store::DirectWordToInt(chain->Sub(bottom));
	  // unroll cycle from right to left!
	  for(u_int j = top-1; j>bottom; j--) {
	    u_int id = Store::DirectWordToInt(chain->Sub(j));
	    SET_INSTR_2R(PC,swap_regs,startId,id);	    
	  }
	}
	break;
      case AssignmentMarker::LEAF:
	{
	  // cycle free assignment chain of form x<-1, y<-x, z<-y, ...
	  for(u_int j = bottom; j<top-1; j++) {
	    u_int dst = Store::DirectWordToInt(chain->Sub(j));
	    u_int src = Store::DirectWordToInt(chain->Sub(j+1));
	    SET_INSTR_2R(PC,load_reg,dst,src);
	  }
	  u_int dst = Store::DirectWordToInt(chain->Sub(top-1));
	  LoadIdRefInto(dst,marker->Sel(0));
	}
	break;
      default:
	fprintf(stderr,"unkown tag %"U_INTF"\n", marker->GetTag());
	Error("internal consistancy error");
      }
    }
    return;
  }
  // argument mismatch
  switch(nFormalArgs) {
  case 0:
    if(nArgs == 1 && !isReturn) {
      u_int reg = LoadIdRefKill(args->Sub(0));
      SET_INSTR_1R(PC,await,reg);
    }
    break;
  case 1:
    switch(nArgs) {
    case 0:
      {
	TagVal *argOpt = TagVal::FromWord(formalArgs->Sub(0));
	if(argOpt != INVALID_POINTER) {
	  u_int dst = IdToReg(argOpt->Sel(0));	
	  SET_INSTR_1R(PC,load_zero,dst); // load unit
	}
      }
      break;
    default:
      {
	Assert(nArgs > 1); // construct tuple into formal arg 0
	TagVal *argOpt = TagVal::FromWord(formalArgs->Sub(0));
	if(argOpt != INVALID_POINTER) {
	  u_int dst = IdToReg(argOpt->Sel(0));
	  switch(nArgs) {
	  case 2:
	  case 3:
	    NewTup(dst, args); // special instr for pairs and triples
	    break;
	  default:
	    {
	      // analyse for conflicts
	      u_int conflict = false;
	      for(u_int i=nArgs; i--; ) {
		TagVal *tagVal = TagVal::FromWordDirect(args->Sub(i));
		u_int idRefTag = AbstractCode::GetIdRef(tagVal);
		if(idRefTag == AbstractCode::Local ||
		   idRefTag == AbstractCode::LastUseLocal) {
		  u_int src = IdToReg(tagVal->Sel(0));
		  if(src == dst) {
		    conflict = true;
		    break;
		  }
		}
	      }
	      u_int tupReg = conflict ? GetNewScratch() : dst;
	      NewTup(tupReg, args);
	      if(conflict) {
		SET_INSTR_2R(PC,load_reg,dst,tupReg);
	      }
	    }
	  }
	}
      }
    }
    break;
  default:
    BCJIT_DEBUG("inlined ccc nArgs %d, nFormals %d\n",nArgs,nFormalArgs);
    if(nArgs == 1) { // deconstruct tuple into formal args
      u_int src = LoadIdRefKill(args->Sub(0));
      for(u_int i=nFormalArgs; i--; ) {
	TagVal *argOpt = TagVal::FromWord(formalArgs->Sub(i));
	if(argOpt != INVALID_POINTER) {
	  u_int dst = IdToReg(argOpt->Sel(0));
	  if(dst == src) {
	    u_int S = GetNewScratch();
	    SET_INSTR_2R(PC,load_reg,S,src);
	    src = S;
	  }
	  SET_INSTR_2R1I(PC,select_tup,dst,src,i);
	}	  
      }
    }
  }
}

// Function of coord * value option vector * string vector *
//             idDef args * outArity option * instr * liveness
TagVal *ByteCodeJitter::CompileInlineFunction(TagVal *abstractCode, 
					      InlineInfo *info,
					      Vector *subst,		   
					      u_int offset,
					      Vector *args,
					      TagVal *idDefsInstrOpt) {
  INSERT_DEBUG_MSG("inline entry")
#ifdef DEBUG_DISASSEMBLE
  Tuple *coord = Tuple::FromWordDirect(abstractCode->Sel(0));
  std::fprintf(stderr, "  inline function at %s:%d.%d depth %d\n",
	       String::FromWordDirect(coord->Sel(0))->ExportC(),
	       Store::DirectWordToInt(coord->Sel(1)),
	       Store::DirectWordToInt(coord->Sel(2)),
	       inlineDepth); 
  AbstractCode::Disassemble(stderr,
			    TagVal::FromWordDirect(abstractCode->Sel(5)));
#endif

  inlineDepth++;
  Vector *oldFormalArgs = currentFormalArgs;
  Vector *oldFormalInArgs = currentFormalInArgs;
  InlineInfo *oldInlineInfo = inlineInfo;
  inlineInfo = info;
  
  // prepare the exit of the inline function
  PatchTable *oldPatchTable = patchTable;
  patchTable = new PatchTable();

  currentFormalInArgs = ShiftIdDefs(currentFormalInArgs,-offset);

  TagVal *continuation;
  if(idDefsInstrOpt != INVALID_POINTER) {
    Tuple *idDefsInstr = Tuple::FromWordDirect(idDefsInstrOpt->Sel(0));
    currentFormalArgs = 
      ShiftIdDefs(Vector::FromWordDirect(idDefsInstr->Sel(0)),-offset);
    continuation = TagVal::FromWordDirect(idDefsInstr->Sel(1));
  } else if (inlineDepth == 1) {
    // if the first caller is a tailcall, than introduce a return instruction
    // otherwise use the continuation of the caller
    continuation = INVALID_POINTER;
    currentFormalArgs = INVALID_POINTER;
  } else {
    continuation = INVALID_POINTER;
    if(currentFormalArgs != INVALID_POINTER)
      currentFormalArgs = ShiftIdDefs(currentFormalArgs,-offset);
  }

  // compile in ccc
  Vector *formalArgs = Vector::FromWordDirect(abstractCode->Sel(3));
  BCJIT_DEBUG("compile in CCC\n");
  // shift formal args -> look for better solution
  Vector *formalArgsShifted = ShiftIdDefs(formalArgs,offset);

  TagVal *root = TagVal::FromWordDirect(abstractCode->Sel(5));
  TagVal *newRoot =
    PeepHoleOptimizer::optimizeInlineInCCC(formalArgsShifted,args,root);
  if(newRoot != INVALID_POINTER)
    root = newRoot;
  else
    CompileInlineCCC(formalArgsShifted,args);  

  // save jitter state
  Vector *oldGlobalSubst = globalSubst;
  IntMap *oldSharedTable = sharedTable;

  globalSubst = subst;
  localOffset += offset;
  sharedTable = IntMap::New(inlineInfo->GetNNodes());

  BCJIT_DEBUG("compile instr\n");
  CompileInstr(root);
  u_int jumpInstrSize = patchTable->GetJumpInstrSize();
  u_int nPatches = patchTable->GetLength();
  BCJIT_DEBUG("start patch with nPatches %d\n",nPatches);
  if(nPatches > 0) {
    BCJIT_DEBUG("start patch %d\n",nPatches);
    // avoid last "jump 0" instruction
    u_int jumpInstrSize = patchTable->GetJumpInstrSize();
    if(PC - jumpInstrSize == patchTable->Sub(nPatches - 1)) {
      PC -= jumpInstrSize;
      WriteBuffer::Shrink(jumpInstrSize);
      nPatches--;
    }
    for(u_int i = nPatches; i--; ) {
      u_int patchPC = patchTable->Sub(i);
      u_int offset = PC - (patchPC + jumpInstrSize);    
      SET_INSTR_1I(patchPC,jump,offset);
    }
  }

  // restore state
  localOffset -= offset;//oldLocalOffset;
  globalSubst = oldGlobalSubst;;
  currentFormalArgs = oldFormalArgs;
  currentFormalInArgs = oldFormalInArgs;
  inlineInfo = oldInlineInfo;
  sharedTable = oldSharedTable;
  delete patchTable;
  patchTable = oldPatchTable;
  inlineDepth--;

  return continuation;
}


u_int invocations = 0;

// Function of coord * value option vector * string vector *
//             idDef args * outArity option * instr * liveness
void ByteCodeJitter::Compile(HotSpotCode *hsc) {
//    timeval startTime;
//    gettimeofday(&startTime,0);
  BCJIT_DEBUG("start compilation (%d times) ", invocations);
  BCJIT_DEBUG("and compile the following abstract code:\n");
  Transform *transform =
    static_cast<Transform *>(hsc->GetAbstractRepresentation());
  TagVal *abstractCode = TagVal::FromWordDirect(transform->GetArgument());
#ifdef DEBUG_DISASSEMBLE 
  Tuple *coord = Tuple::FromWordDirect(abstractCode->Sel(0));
  std::fprintf(stderr, "\n%d. compile function (%p) at %s:%d.%d nArgs=%d\n",
	       ++invocations, abstractCode->ToWord(),
	       String::FromWordDirect(coord->Sel(0))->ExportC(),
	       Store::DirectWordToInt(coord->Sel(1)),
	       Store::DirectWordToInt(coord->Sel(2)),
	       Vector::FromWordDirect(abstractCode->Sel(3))->GetLength()
	       ); 
  AbstractCode::Disassemble(stderr,
			    TagVal::FromWordDirect(abstractCode->Sel(5)));
#endif    

  currentConcreteCode = hsc->ToWord();
  // do inline anaysis
  currentFormalArgs = INVALID_POINTER;
  localOffset = 0;
  inlineDepth = 0;
#ifdef DO_INLINING
  TagVal *inlineInfoOpt = hsc->GetInlineInfoOpt();
  if(inlineInfoOpt == INVALID_POINTER) {
    ByteCodeInliner::ResetRoot();
    inlineInfo = ByteCodeInliner::AnalyseInlining(abstractCode);
  } else
    inlineInfo = InlineInfo::FromWordDirect(inlineInfoOpt->Sel(0));
  // run constant propagation
#ifdef DO_CONSTANT_PROPAGATION
  tagtestInfo = ByteCodeConstProp::RunAnalysis(abstractCode, inlineInfo);
#endif
#endif

//    timeval startTime;
//    gettimeofday(&startTime,0);

  // perform register allocation
#ifdef DO_INLINING
  currentNLocals = inlineInfo->GetNLocals();
#else
  currentNLocals = GetNumberOfLocals(abstractCode);
#endif

#ifdef DO_REG_ALLOC
#ifdef DO_INLINING
  Vector *liveness = inlineInfo->GetLiveness();
#else
  Vector *liveness = Vector::FromWordDirect(abstractCode->Sel(6));
#endif
  u_int local_mapping[currentNLocals];
  mapping = local_mapping;
  RegisterAllocator::Run(liveness,mapping,&currentNLocals);
#ifdef DO_INLINING
  // add the alias analysis
  Array *aliases = inlineInfo->GetAliases();
  u_int nAliases = aliases->GetLength();
  // the loop direction is important !
  for(u_int i = 0; i<nAliases; i++)
    mapping[i] = mapping[Store::DirectWordToInt(aliases->Sub(i))];
#endif
#endif

  // now prepare scratch registers
  scratch = currentNLocals;
  nRegisters = scratch;
  delayedScratchInc = 0;
  
  // prepare immediate environment
  imEnv.Init();

  // prepare control data structures
#ifdef DO_INLINING
  sharedTable = IntMap::New(inlineInfo->GetNNodes());
#else
  sharedTable = IntMap::New(256);
#endif
 
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
  TagVal *outArityOpt = TagVal::FromWord(abstractCode->Sel(4));
  currentOutArity = ((outArityOpt == INVALID_POINTER) ? INVALID_INT :
		     Store::DirectWordToInt(outArityOpt->Sel(0)));
  Vector *args = Vector::FromWordDirect(abstractCode->Sel(3));
  CompileCCC(args,INVALID_INT);
  skipCCCPC = PC;
  currentFormalInArgs = args;

  // compile function body
  CompileInstr(TagVal::FromWordDirect(abstractCode->Sel(5)));

  // create compiled concrete code
  Chunk *code = WriteBuffer::FlushCode();
  // convert hot spot code to byte code
  ByteConcreteCode::Convert(hsc,
			    code,
			    imEnv.ExportEnv(),
			    Store::IntToWord(nRegisters),
#ifdef DO_INLINING
			    inlineInfo->ToWord()
#else
			    Store::IntToWord(0)
#endif
			    );

//   static u_int sumNRegisters = 0;
//   sumNRegisters += nRegisters;
//   fprintf(stderr,"nLocals %d, nRegisters %d, sumNRegisters %d\n",
// 	  currentNLocals,nRegisters,sumNRegisters);

//   static u_int codeSize = 0;
//   codeSize += code->GetSize();
//   fprintf(stderr,"codeSize %d\n",codeSize);
#ifdef DEBUG_DISASSEMBLE
//   if(invocations>100) {
  fprintf(stderr,"-----------------\ncompiled code:\n");
#ifdef THREADED
  ByteCode::Disassemble(stderr,(u_int *)code->GetBase(),code,
			Tuple::FromWordDirect(imEnv.ExportEnv()));
#else
  ByteCode::Disassemble(stderr,0,code,
			Tuple::FromWordDirect(imEnv.ExportEnv()));
#endif
  fprintf(stderr,"-------------\n");
#endif
//   }
//   static double totalTime = 0;
//   timeval stopTime;
//   gettimeofday(&stopTime,0);
//   totalTime += 0.000001 * ((double) (stopTime.tv_usec - startTime.tv_usec))
//     + ((double) (stopTime.tv_sec - startTime.tv_sec));
//   fprintf(stderr,"compile time %f seconds\n",totalTime);
}

