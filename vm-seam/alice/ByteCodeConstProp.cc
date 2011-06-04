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
#include "alice/ByteCodeConstProp.hh"
#include "alice/Types.hh"

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
  enum { VISIT, SET_CONSTTABLE, INLINE_EXIT, STOP };
  ControlStack(u_int s = 400) : size(s), top(-1) { stack = new u_int[size]; }
  u_int PopCommand() { return Pop(); }
  TagVal *PopInstr() { return reinterpret_cast<TagVal *>(Pop()); }
  Tuple *PopInlineExit() { return reinterpret_cast<Tuple *>(Pop()); }
  Array *PopConstTable() { return reinterpret_cast<Array *>(Pop()); }
  void PushInstr(TagVal *instr) {
    Push(reinterpret_cast<u_int>(instr));
    Push(VISIT);
  }
  void PushInlineExit(Tuple *tup) {
    Push(reinterpret_cast<u_int>(tup));
    Push(INLINE_EXIT);
  }
  void PushStop() { Push(STOP); }
  void PushConstTable(Array *constants) {
    Push(reinterpret_cast<u_int>(constants));
    Push(SET_CONSTTABLE);
  }
  bool Empty() { return top == -1; }
  u_int GetSize() { return top; }
};

// collect information about the in artiy of shared nodes
// This is needed for constant propagation. On shared nodes we have to
// merge the collected constants from every branch. So we first have to 
// visit every branch. 

class ConstProp_PrePass {
private:
  InlineInfo *inlineInfo;
  Map *sharedInArity;
  TagVal *root;
public:
  ConstProp_PrePass(TagVal *abstractCode, InlineInfo *info) : inlineInfo(info) {
    sharedInArity = Map::New(128);
    root = TagVal::FromWordDirect(abstractCode->Sel(5));
//     fprintf(stderr,"pre pass over:\n");
//     AbstractCode::Disassemble(stderr,root);
  }
  void Run();
  Map *GetSharedInArity() { return sharedInArity; }
};

void ConstProp_PrePass::Run() {
  ControlStack stack;
  // use sharedInArity for book-keeping of shared nodes
  stack.PushStop();
  stack.PushInstr(root);
  for(;;) {
    switch(stack.PopCommand()) {
    case ControlStack::STOP:
      return;
    case ControlStack::SET_CONSTTABLE:
      {
	Error("SET_CONSTTABLE invalid for pre pass\n");
      }
      break;
    case ControlStack::INLINE_EXIT:
      {
	// this is a dummy as we do not have anything interesting to do
	// in pre pass
	Tuple *state = stack.PopInlineExit();
	inlineInfo = InlineInfo::FromWordDirect(state->Sel(0));
      }
      break;
    case ControlStack::VISIT:
      {
	TagVal *instr = stack.PopInstr();
	switch (AbstractCode::GetInstr(instr)) {
    case AbstractCode::Entry:
      stack.PushInstr(TagVal::FromWordDirect(instr->Sel(2)));
      break;
    case AbstractCode::Exit:
      stack.PushInstr(TagVal::FromWordDirect(instr->Sel(3)));
      break;
	case AbstractCode::Raise:
	case AbstractCode::Reraise:
	case AbstractCode::Return:
	  break;
	case AbstractCode::EndHandle:
	case AbstractCode::EndTry:
	  stack.PushInstr(TagVal::FromWordDirect(instr->Sel(0))); 
	  break;
	case AbstractCode::Kill:
	  stack.PushInstr(TagVal::FromWordDirect(instr->Sel(1)));
	  break;
	case AbstractCode::GetRef:
	case AbstractCode::GetTup:
	case AbstractCode::PutNew:
	case AbstractCode::PutRef:
	case AbstractCode::PutTup:
	case AbstractCode::PutVar:
	case AbstractCode::PutVec:
	  stack.PushInstr(TagVal::FromWordDirect(instr->Sel(2))); 
	  break;
	case AbstractCode::Close:
	case AbstractCode::LazyPolySel:
	case AbstractCode::PutCon:
	case AbstractCode::PutPolyRec:
	case AbstractCode::Sel: 
	case AbstractCode::Specialize:
	  stack.PushInstr(TagVal::FromWordDirect(instr->Sel(3))); 
	  break;
	case AbstractCode::PutTag:
	  stack.PushInstr(TagVal::FromWordDirect(instr->Sel(4))); 
	  break;
	case AbstractCode::AppPrim:
	  {
	    TagVal *contOpt = TagVal::FromWord(instr->Sel(2));
	    if(contOpt != INVALID_POINTER) {
	      Tuple *tup = Tuple::FromWordDirect(contOpt->Sel(0));
	      stack.PushInstr(TagVal::FromWordDirect(tup->Sel(1)));
	    }
	  }
	  break;
	case AbstractCode::AppVar:
	  {
	    TagVal *contOpt = TagVal::FromWord(instr->Sel(3));
	    if(contOpt != INVALID_POINTER) {
	      Tuple *tup = Tuple::FromWordDirect(contOpt->Sel(0));
	      stack.PushInstr(TagVal::FromWordDirect(tup->Sel(1)));
	    }
	    // check if the function can be inlined
	    // app var nodes become shared nodes if the function
	    // can be inlined.
	    Map *inlineMap = inlineInfo->GetInlineMap();
	    if(inlineMap->IsMember(instr->ToWord())) {
	      // enter the inlined function
	      Tuple *state = Tuple::New(1);
	      state->Init(0, inlineInfo->ToWord());
	      stack.PushInlineExit(state);
	      Tuple *tup = 
		Tuple::FromWordDirect(inlineMap->Get(instr->ToWord()));
	      TagVal *abstractCode = TagVal::FromWordDirect(tup->Sel(0));
	      inlineInfo = InlineInfo::FromWordDirect(tup->Sel(3));
// 	      fprintf(stderr,"inline function %p:\n",abstractCode);
// 	      AbstractCode::Disassemble(stderr,
// 				 TagVal::FromWordDirect(abstractCode->Sel(5)));
	      stack.PushInstr(TagVal::FromWordDirect(abstractCode->Sel(5)));
	    }
	  }
	  break;
	case AbstractCode::Try:
	  stack.PushInstr(TagVal::FromWordDirect(instr->Sel(3)));
	  stack.PushInstr(TagVal::FromWordDirect(instr->Sel(0)));
	  break;
	case AbstractCode::IntTest:
	case AbstractCode::RealTest:
	case AbstractCode::StringTest:
	case AbstractCode::VecTest:
	  {
	    stack.PushInstr(TagVal::FromWordDirect(instr->Sel(2)));
	    Vector *tests = Vector::FromWordDirect(instr->Sel(1));
	    for (u_int i = tests->GetLength(); i--; ) {
	      Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
	      stack.PushInstr(TagVal::FromWordDirect(pair->Sel(1)));
	    }
	  }
	  break;
	case AbstractCode::CompactIntTest:
	  {
	    stack.PushInstr(TagVal::FromWordDirect(instr->Sel(3)));
	    Vector *tests = Vector::FromWordDirect(instr->Sel(2));
	    for (u_int i = tests->GetLength(); i--; ) {
	      stack.PushInstr(TagVal::FromWordDirect(tests->Sub(i)));
	    }	
	  }
	  break;
	case AbstractCode::ConTest:
	  {
	    stack.PushInstr(TagVal::FromWordDirect(instr->Sel(3)));
	    Vector *testsN = Vector::FromWordDirect(instr->Sel(2));
	    for(u_int i = testsN->GetLength(); i--; ) {
	      Tuple *triple = Tuple::FromWordDirect(testsN->Sub(i));
	      stack.PushInstr(TagVal::FromWordDirect(triple->Sel(2)));
	    }
	    Vector *tests0 = Vector::FromWordDirect(instr->Sel(1));
	    for(u_int i = tests0->GetLength(); i--; ) {
	      Tuple *pair = Tuple::FromWordDirect(tests0->Sub(i));	  
	      stack.PushInstr(TagVal::FromWordDirect(pair->Sel(1)));
	    }
	  }
	  break;
	case AbstractCode::TagTest:
	  {
	    stack.PushInstr(TagVal::FromWordDirect(instr->Sel(4)));
	    Vector *testsN = Vector::FromWordDirect(instr->Sel(3));
	    for(u_int i = testsN->GetLength(); i--; ) {
	      Tuple *triple = Tuple::FromWordDirect(testsN->Sub(i));
	      stack.PushInstr(TagVal::FromWordDirect(triple->Sel(2)));
	    }
	    Vector *tests0 = Vector::FromWordDirect(instr->Sel(2));
	    for(u_int i = tests0->GetLength(); i--; ) {
	      Tuple *pair = Tuple::FromWordDirect(tests0->Sub(i));	  
	      stack.PushInstr(TagVal::FromWordDirect(pair->Sel(1)));
	    }
	  }
	  break;
	case AbstractCode::CompactTagTest:
	  {
	    TagVal *elseInstrOpt = TagVal::FromWord(instr->Sel(3));
	    if(elseInstrOpt != INVALID_POINTER)
	      stack.PushInstr(TagVal::FromWordDirect(elseInstrOpt->Sel(0)));
	    Vector *tests = Vector::FromWordDirect(instr->Sel(2));
	    for(u_int i = tests->GetLength(); i--; ) {
	      Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
	      stack.PushInstr(TagVal::FromWordDirect(pair->Sel(1)));
	    }	
	  }
	  break;
	case AbstractCode::Shared:
	  {
	    word key = instr->ToWord(); // instr->Sel(0);
	    if(!sharedInArity->IsMember(key)) {
	      stack.PushInstr(TagVal::FromWordDirect(instr->Sel(1)));
	      sharedInArity->Put(key,Store::IntToWord(1)); 
	    } else {
	      u_int inArity = Store::DirectWordToInt(sharedInArity->Get(key));
	      sharedInArity->Put(key,Store::IntToWord(++inArity));
	    }
	  }
	  break;
	default:
	  fprintf(stderr,"PrePass: invalid abstractCode tag %"U_INTF"\n",
		  static_cast<u_int>(AbstractCode::GetInstr(instr)));
	  return;
	}
      }
      break;
    default:
      {
	Error("internal consistancy error in control stack\n");
      }
    }
  }
}


//
class ConstProp_MainPass {
private:
  enum const_kind { 
    INT,
    TAGVAL,            // stores the tag     
    UNKNOWN       
  };

  u_int nLocals;
  Vector *subst;
  Array *constants;
  word wUnkownVal;
  Map *sharedInArity;
  TagVal *root;
  u_int localOffset;
  u_int inlineDepth;
  Vector *noFormalArgs;
  Vector *formalArgs;
  Array *oldConstants;
  InlineInfo *inlineInfo;

  Map *tagtestInfo;

  // some helpers
  u_int IdToId(word id) {
    return Store::DirectWordToInt(id) + localOffset;
  }
  TagVal *LookupSubst(word idRef) {
    TagVal *tagVal = TagVal::FromWordDirect(idRef);
    if(AbstractCode::GetIdRef(tagVal) == AbstractCode::Global) {
      u_int addr = Store::DirectWordToInt(tagVal->Sel(0));
      TagVal *valueOpt = TagVal::FromWord(subst->Sub(addr));
      if(valueOpt != INVALID_POINTER) {
	TagVal *immediate = TagVal::New(AbstractCode::Immediate,1);
	immediate->Init(0,valueOpt->Sel(0));
	return immediate;
      }
    }
    return tagVal;
  }
  void RegisterValue(Array *constsTable,
		     word wIdDef, word val, const_kind kind) {
    TagVal *idDef = TagVal::FromWord(wIdDef);
    if(idDef != INVALID_POINTER) {
      u_int index = Store::DirectWordToInt(idDef->Sel(0));
      TagVal *newVal = TagVal::New(kind,1);
      newVal->Init(0,val);
      constsTable->Update(index, newVal->ToWord());
    }
  }
  void RegisterValue(word wIdDef, word val, const_kind kind) {
    RegisterValue(constants,wIdDef,val,kind);
  }
  void ResetLocation(word wIdDef) {
    TagVal *idDef = TagVal::FromWord(wIdDef);
    if(idDef != INVALID_POINTER) {
      u_int index = Store::DirectWordToInt(idDef->Sel(0));
      constants->Update(index, wUnkownVal);
    }
  }
  const_kind ExtractKind(Array *table, u_int index) {
    TagVal *valKind = TagVal::FromWordDirect(table->Sub(index));
    return static_cast<const_kind>(valKind->GetTag());
  }
  const_kind ExtractKind(u_int index) {
    TagVal *valKind = TagVal::FromWordDirect(constants->Sub(index));
    return static_cast<const_kind>(valKind->GetTag());
  }
  word ExtractValue(u_int index) {
    Assert(ExtractKind(index) != UNKNOWN);
    TagVal *info = TagVal::FromWordDirect(constants->Sub(index));
    return info->Sel(0);
  }
  const_kind ExtractKind(word idDef) {
    TagVal *tagVal = TagVal::FromWord(idDef);
    if(tagVal != INVALID_POINTER) {
      u_int index = Store::DirectWordToInt(tagVal->Sel(0));
      TagVal *valKind = TagVal::FromWordDirect(constants->Sub(index));
      return static_cast<const_kind>(valKind->GetTag());
    }
    return UNKNOWN;
  }
  void CreateConstFromIdRef(word wIdDef, word wIdRef, const_kind kind) {
    TagVal *idRef = TagVal::FromWordDirect(wIdRef);
    switch(AbstractCode::GetIdRef(idRef)) {
    case AbstractCode::LastUseLocal:
    case AbstractCode::Local:
      {
	u_int index = IdToId(idRef->Sel(0));
	if(ExtractKind(index) != UNKNOWN) {
	  RegisterValue(wIdDef, ExtractValue(index), kind);
	}
      }
    case AbstractCode::Immediate:
      {
	RegisterValue(wIdDef, idRef->Sel(0), kind);
      }
      break;
    case AbstractCode::Global:
      {
	u_int addr = Store::DirectWordToInt(idRef->Sel(0));
	TagVal *valueOpt = TagVal::FromWord(subst->Sub(addr));
	if(valueOpt != INVALID_POINTER) {
	  RegisterValue(wIdDef, valueOpt->Sel(0), kind);
	}
      }
      break;
    default:
      ;
    }
  }

  // merge the main constant table with another one
  void MergeConstantTable(Array *constants2) {
    Assert(constants->GetLength() == constants2->GetLength());
    // this currently only works for tag values
    for(u_int i = constants->GetLength(); i--; ) {
      u_int kind1 = ExtractKind(i);
      u_int kind2 = ExtractKind(constants2,i);
      if(kind1 == TAGVAL && kind2 == TAGVAL) {	
	TagVal *info1 = TagVal::FromWordDirect(constants->Sub(i));
	TagVal *info2 = TagVal::FromWordDirect(constants2->Sub(i));
	u_int tag1 = Store::DirectWordToInt(info1->Sel(0));
	u_int tag2 = Store::DirectWordToInt(info2->Sel(0));	
	if(tag1 == tag2) // keep the information
	  continue;
      } else if (kind1 == INT && kind2 == INT) {
	TagVal *info1 = TagVal::FromWordDirect(constants->Sub(i));
	TagVal *info2 = TagVal::FromWordDirect(constants2->Sub(i));
	if(Store::DirectWordToInt(info1->Sel(0)) == 
	   Store::DirectWordToInt(info2->Sel(0))) {
	  continue; // keep this information
	}
      }
      constants->Update(i, wUnkownVal); // reset
    }
  }

  Array *CloneConstants() {
    Assert(constants->GetLength() == nLocals);
    Array *constantsCopy = Array::New(nLocals);
    for(u_int i = nLocals; i--; ) {
      constantsCopy->Init(i, constants->Sub(i));
    }
    return constantsCopy;
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
  
  void InlineCCC(Vector *args, Vector *formalArgs);
  
public:
  ConstProp_MainPass(TagVal *abstractCode, 
		     Map *inArityMap,
		     InlineInfo *info) 
    : sharedInArity(inArityMap), inlineInfo(info) {
    // start with local offset and inline depth equal to zero
    localOffset = 0;
    inlineDepth = 0;
    // set number of locals
    nLocals = inlineInfo->GetNLocals(); 
    // extract substitution
    subst = Vector::FromWordDirect(abstractCode->Sel(1));
    // prepare local information
    wUnkownVal = TagVal::New(UNKNOWN,0)->ToWord();
    constants = Array::New(nLocals);
    for(u_int i = nLocals; i--; ) {
      constants->Init(i,wUnkownVal);
    }
    oldConstants = constants;
    // set root node    
    root = TagVal::FromWordDirect(abstractCode->Sel(5));
    // set outgoing formal args
    noFormalArgs = Vector::New(0);
    formalArgs = INVALID_POINTER;
    // prepare tag test information map
    tagtestInfo = Map::New(20);
  }
  void Run();
  Map *GetTagTestInfo() { return tagtestInfo; }
};

// the following implements a depth first pass through the abstract code graph

void ConstProp_MainPass::InlineCCC(Vector *args, Vector *formalArgs) {
  if(formalArgs == noFormalArgs)
    return;
  u_int nArgs = args->GetLength();
  u_int nFormalArgs = formalArgs->GetLength();
  if(nArgs == nFormalArgs) {
    for(u_int i = nArgs; i--; ) {
      TagVal *idDef = TagVal::FromWord(formalArgs->Sub(i));
      if(idDef != INVALID_POINTER) {	
	u_int id = IdToId(idDef->Sel(0));
	TagVal *idRef = TagVal::FromWordDirect(args->Sub(i));
	switch(AbstractCode::GetIdRef(idRef)) {
	case AbstractCode::LastUseLocal:
	case AbstractCode::Local:
	  {
	    u_int index = IdToId(idRef->Sel(0));
	    constants->Update(id, constants->Sub(index));
	  }
	  break;
	case AbstractCode::Global:
	  {
	    u_int index = Store::DirectWordToInt(idRef->Sel(0));
	    TagVal *valOpt = TagVal::FromWord(subst->Sub(index));
	    if(valOpt == INVALID_POINTER)
	      break;
	    idRef = valOpt;
	    // fall through
	  }
	case AbstractCode::Immediate:
	  {
	    word val = idRef->Sel(0);
	    if (PointerOp::IsInt(val)) {
	      // an integer can also be a nullary constructor
	      TagVal *info = TagVal::New(INT, 1);
	      info->Init(0, val);
	      constants->Update(id, info->ToWord());
	    }
	  }
	  break;
	default:
	  Error("unknown idRef tag\n");
	}
      }
    }
  } else {
//     switch(nFormalArgs) {
//     case 0:
//       break;
//     case 1:
//       {
// 	if(nArgs == 0) { // this should mainly an assertion
// 	  // in this case argument 0 is set to unit
// 	  TagVal *info = TagVal::New(INT,1);
// 	  info->Init(0, Store::IntToWord(0));
// 	  u_int index = IdToId(0);
// 	  constants->Update(index, info->ToWord());
// 	}
//       }
//       break;
//     default:
//       // We could pass some information about the constructed or 
//       // deconstructed tuple.
//       ;
//     }
  }
}

void ConstProp_MainPass::Run() {
  Map *sharedInfo = Map::New(200);
  ControlStack stack;
  stack.PushStop();
  stack.PushInstr(root);
  for(;;) {
    switch(stack.PopCommand()) {
    case ControlStack::STOP:
      return;
    case ControlStack::SET_CONSTTABLE:
      {
	constants = stack.PopConstTable();
      }
      break;
    case ControlStack::INLINE_EXIT:
      {
	Tuple *state = stack.PopInlineExit();
	localOffset = Store::DirectWordToInt(state->Sel(0));
	subst = Vector::FromWordDirect(state->Sel(1));
	formalArgs = Vector::FromWordDirect(state->Sel(2));
	oldConstants = Array::FromWordDirect(state->Sel(3));
	inlineInfo = InlineInfo::FromWordDirect(state->Sel(4));
	inlineDepth--;
      }
      break;
    case ControlStack::VISIT:
      {
	TagVal *instr = stack.PopInstr();
	switch (AbstractCode::GetInstr(instr)) {
    case AbstractCode::Entry:
      stack.PushInstr(TagVal::FromWordDirect(instr->Sel(2)));
      break;
    case AbstractCode::Exit:
      stack.PushInstr(TagVal::FromWordDirect(instr->Sel(3)));
      break;
	case AbstractCode::Raise:
	case AbstractCode::Reraise:
	  break;
	case AbstractCode::Return:
	  {
	    // exit from an inlined callee
	    if(inlineDepth > 0) {
	      Vector *args = Vector::FromWordDirect(instr->Sel(0));
	      InlineCCC(args,formalArgs);
	      // merge constant tables
	      if(constants != oldConstants) {
		MergeConstantTable(oldConstants);
	      }
	      oldConstants = constants;
	    }
	  }
	  break;
	case AbstractCode::EndHandle:
	case AbstractCode::EndTry:
	  stack.PushInstr(TagVal::FromWordDirect(instr->Sel(0))); 
	  break;
	case AbstractCode::Kill:
	  stack.PushInstr(TagVal::FromWordDirect(instr->Sel(1)));
	  break;
	case AbstractCode::GetRef:
	case AbstractCode::GetTup:
	case AbstractCode::PutNew:
	case AbstractCode::PutRef:
	case AbstractCode::PutTup:
	case AbstractCode::PutVar:
	  {
	    // check if we can propagate information
	    TagVal *tagVal = TagVal::FromWordDirect(instr->Sel(1));
	    switch(AbstractCode::GetIdRef(tagVal)) {
	    case AbstractCode::LastUseLocal:
	    case AbstractCode::Local:
	      {
		// propagate information
		u_int src = IdToId(tagVal->Sel(0));
		TagVal *idDef = TagVal::FromWord(instr->Sel(0));
		if(idDef != INVALID_POINTER) {
		  u_int dst = Store::DirectWordToInt(idDef->Sel(0));
		  constants->Update(dst, constants->Sub(src));
		}
	      }
	      break;
	    default:
	      // we cannot do anything because we need to know the type
	      // of the value
	      ;
	    }	
	    // push continuation
	    stack.PushInstr(TagVal::FromWordDirect(instr->Sel(2))); 
	  }
	  break;
	case AbstractCode::PutVec:
	  stack.PushInstr(TagVal::FromWordDirect(instr->Sel(2))); 
	  break;
	case AbstractCode::Close:
	case AbstractCode::LazyPolySel:
	case AbstractCode::PutCon:
	case AbstractCode::PutPolyRec:
	case AbstractCode::Sel: 
	case AbstractCode::Specialize:
	  stack.PushInstr(TagVal::FromWordDirect(instr->Sel(3))); 
	  break;
	case AbstractCode::PutTag:
	  {
	    // create information about the tag
	    u_int tag = Store::DirectWordToInt(instr->Sel(2));
	    RegisterValue(instr->Sel(0), Store::IntToWord(tag), TAGVAL);
	    // push continuation
	    stack.PushInstr(TagVal::FromWordDirect(instr->Sel(4))); 
	  }
	  break;
	case AbstractCode::AppPrim:
	  {
	    TagVal *contOpt = TagVal::FromWord(instr->Sel(2));
	    if(contOpt != INVALID_POINTER) {
	      Tuple *tup = Tuple::FromWordDirect(contOpt->Sel(0));
	      stack.PushInstr(TagVal::FromWordDirect(tup->Sel(1)));
	    }
	  }
	  break;
	case AbstractCode::AppVar:
	  {	    
	    TagVal *contOpt = TagVal::FromWord(instr->Sel(3));
	    if(contOpt != INVALID_POINTER) {
	      Tuple *tup = Tuple::FromWordDirect(contOpt->Sel(0));
	      stack.PushInstr(TagVal::FromWordDirect(tup->Sel(1)));
	    }
	    // check if the function can be inlined
	    Map *inlineMap = inlineInfo->GetInlineMap();
	    if(inlineMap->IsMember(instr->ToWord())) {
	      Tuple *tup = 
		Tuple::FromWordDirect(inlineMap->Get(instr->ToWord()));
	      TagVal *abstractCode = TagVal::FromWordDirect(tup->Sel(0));
// 	      fprintf(stderr,"inline for %p at depth %d\n",instr,inlineDepth);
// 	      AbstractCode::Disassemble(stderr,
// 					TagVal::FromWordDirect(abstractCode->Sel(5)));
	      // enter the inlined function
	      Tuple *state = Tuple::New(5);
	      state->Init(0, Store::IntToWord(localOffset));
	      state->Init(1, subst->ToWord());
	      state->Init(2, formalArgs->ToWord());
	      state->Init(3, oldConstants->ToWord());
	      state->Init(4, inlineInfo->ToWord());
	      stack.PushInlineExit(state);
	      stack.PushInstr(TagVal::FromWordDirect(abstractCode->Sel(5)));
	      u_int offset = Store::DirectWordToInt(tup->Sel(2));
	      inlineInfo = InlineInfo::FromWordDirect(tup->Sel(3));
	      // update the constant table
	      Vector *args = Vector::FromWordDirect(instr->Sel(1));
	      Vector *formalInArgs = 
		ShiftIdDefs(Vector::FromWordDirect(abstractCode->Sel(3)),
			    offset);
	      InlineCCC(args,formalInArgs);
	      localOffset += offset;
	      subst = Vector::FromWordDirect(tup->Sel(1));
	      inlineDepth++;
	      if(contOpt != INVALID_POINTER) {
		Tuple *tup = Tuple::FromWordDirect(contOpt->Sel(0));
		formalArgs = 
		  ShiftIdDefs(Vector::FromWordDirect(tup->Sel(0)),-offset);
	      } else if(inlineDepth == 1) {
		formalArgs = noFormalArgs;
	      } else {
		// If we have a tailcall at depth > 1, we just keep
		// the formal arguments of the caller.
	      }
	      oldConstants = constants;
	    }
	  }
	  break;
	case AbstractCode::Try:
	  {
	    stack.PushInstr(TagVal::FromWordDirect(instr->Sel(3)));
	    stack.PushConstTable(CloneConstants());
	    stack.PushInstr(TagVal::FromWordDirect(instr->Sel(0)));
	  }
	  break;
	case AbstractCode::IntTest:
	case AbstractCode::RealTest:
	case AbstractCode::StringTest:
	case AbstractCode::VecTest:
	  {
	    stack.PushInstr(TagVal::FromWordDirect(instr->Sel(2)));
	    stack.PushConstTable(CloneConstants());
	    Vector *tests = Vector::FromWordDirect(instr->Sel(1));
	    for (u_int i = tests->GetLength(); i--; ) {
	      Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
	      stack.PushInstr(TagVal::FromWordDirect(pair->Sel(1)));
	      stack.PushConstTable(CloneConstants());
	    }
	  }
	  break;
	case AbstractCode::CompactIntTest:
	  {
	    stack.PushInstr(TagVal::FromWordDirect(instr->Sel(3)));
	    stack.PushConstTable(CloneConstants());
	    Vector *tests = Vector::FromWordDirect(instr->Sel(2));
	    for (u_int i = tests->GetLength(); i--; ) {
	      stack.PushInstr(TagVal::FromWordDirect(tests->Sub(i)));
	      stack.PushConstTable(CloneConstants());
	    }	
	  }
	  break;
	case AbstractCode::ConTest:
	  {
	    stack.PushInstr(TagVal::FromWordDirect(instr->Sel(3)));
	    stack.PushConstTable(CloneConstants());
	    Vector *testsN = Vector::FromWordDirect(instr->Sel(2));
	    for(u_int i = testsN->GetLength(); i--; ) {
	      Tuple *triple = Tuple::FromWordDirect(testsN->Sub(i));
	      stack.PushInstr(TagVal::FromWordDirect(triple->Sel(2)));
	      stack.PushConstTable(CloneConstants());
	    }
	    Vector *tests0 = Vector::FromWordDirect(instr->Sel(1));
	    for(u_int i = tests0->GetLength(); i--; ) {
	      Tuple *pair = Tuple::FromWordDirect(tests0->Sub(i));	  
	      stack.PushInstr(TagVal::FromWordDirect(pair->Sel(1)));
	      stack.PushConstTable(CloneConstants());
	    }
	  }
	  break;
	case AbstractCode::TagTest:
	  {
	    TagVal *idRef = TagVal::FromWordDirect(instr->Sel(0));
	    u_int tag;
	    bool isValue = false;
	    u_int localIndex;
	    bool isLocal = false;
	    switch(AbstractCode::GetIdRef(idRef)) {
	    case AbstractCode::LastUseLocal:
	    case AbstractCode::Local:
	      {
		u_int index = IdToId(idRef->Sel(0));
		switch(ExtractKind(index)) {
		case INT:
		case TAGVAL:
		  {	
		    tag = Store::DirectWordToInt(ExtractValue(index));
		    isValue = true;
		  }
		  break;
		default:
		  ;
		}
		isLocal = true;
		localIndex = index;
	      }
	      break;
	    case AbstractCode::Global:
	      {
		u_int addr = Store::DirectWordToInt(idRef->Sel(0));
		TagVal *valueOpt = TagVal::FromWord(subst->Sub(addr));
		if(valueOpt == INVALID_POINTER)
		  break;
		idRef = valueOpt;
		// fall through
	      }
	    case AbstractCode::Immediate:
	      {
		word immediate = idRef->Sel(0);
		if(PointerOp::IsInt(immediate)) {
		  tag = Store::DirectWordToInt(immediate);
		} else {
		  Block *block = Store::WordToBlock(immediate);
		  if (block->GetLabel() == Alice::BIG_TAG) {
		    BigTagVal *bigTagVal = 
		      BigTagVal::FromWordDirect(immediate);
		    tag = bigTagVal->GetTag();
		  } else {
		    TagVal *tagVal = TagVal::FromWordDirect(immediate);
		    tag = tagVal->GetTag();
		  }
		}
		isValue = true;
	      }
	      break;
	    default:
	      ;
	    }
	    if(isValue) {
	      static u_int counter = 0;
	      Vector *tests = Vector::FromWordDirect(instr->Sel(2));
	      bool tagFound = false;
	      Vector *testsN = Vector::FromWordDirect(instr->Sel(3));
	      for(u_int i = testsN->GetLength(); i--; ) {
		Tuple *triple = Tuple::FromWordDirect(testsN->Sub(i));
		if(Store::DirectWordToInt(triple->Sel(0)) == tag) {
		  fprintf(stderr,"%"U_INTF". could eliminate tag test %p:",
			  ++counter,instr);
		  fprintf(stderr," value has tag %"U_INTF"\n",tag);		  
		  tagFound = true;
		  break;
		}
	      }
	      Vector *tests0 = Vector::FromWordDirect(instr->Sel(2));
	      for(u_int i = tests0->GetLength(); i--; ) {
		Tuple *pair = Tuple::FromWordDirect(tests0->Sub(i));	  
		if(Store::DirectWordToInt(pair->Sel(0)) == tag) {
		  fprintf(stderr,"%"U_INTF". could eliminate tag test %p:",
			  ++counter,instr);
		  fprintf(stderr," value has tag %"U_INTF"\n",tag);		  
		  tagFound = true;
		  break;
		}
	      }
	      if(!tagFound) {
		// choose else branch
		fprintf(stderr,"%"U_INTF". could eliminate tag test %p:",
			++counter,instr);		  
		fprintf(stderr," value has tag %"U_INTF"\n",tag);
	      }
	    } else if (isLocal) {
	      stack.PushInstr(TagVal::FromWordDirect(instr->Sel(4)));
	      stack.PushConstTable(CloneConstants());
	      Vector *testsN = Vector::FromWordDirect(instr->Sel(3));
	      for(u_int i = testsN->GetLength(); i--; ) {
		Tuple *triple = Tuple::FromWordDirect(testsN->Sub(i));
		Array *constantsCopy = CloneConstants();
		TagVal *info = TagVal::New(TAGVAL,1);
		info->Init(0, triple->Sel(0));
		constantsCopy->Update(localIndex, info->ToWord());
		stack.PushInstr(TagVal::FromWordDirect(triple->Sel(2)));
		stack.PushConstTable(constantsCopy);
	      }
	      Vector *tests0 = Vector::FromWordDirect(instr->Sel(2));
	      for(u_int i = tests0->GetLength(); i--; ) {
		Tuple *pair = Tuple::FromWordDirect(tests0->Sub(i));	  
		Array *constantsCopy = CloneConstants();
		TagVal *info = TagVal::New(TAGVAL,1);
		info->Init(0, pair->Sel(0));
		constantsCopy->Update(localIndex, info->ToWord());
		stack.PushInstr(TagVal::FromWordDirect(pair->Sel(1)));
		stack.PushConstTable(constantsCopy);
	      }
	    } else {
	      stack.PushInstr(TagVal::FromWordDirect(instr->Sel(4)));
	      stack.PushConstTable(CloneConstants());
	      Vector *testsN = Vector::FromWordDirect(instr->Sel(3));
	      for(u_int i = testsN->GetLength(); i--; ) {
		Tuple *triple = Tuple::FromWordDirect(testsN->Sub(i));
		stack.PushInstr(TagVal::FromWordDirect(triple->Sel(2)));
		stack.PushConstTable(CloneConstants());
	      }
	      Vector *tests0 = Vector::FromWordDirect(instr->Sel(2));
	      for(u_int i = tests0->GetLength(); i--; ) {
		Tuple *pair = Tuple::FromWordDirect(tests0->Sub(i));	  
		stack.PushInstr(TagVal::FromWordDirect(pair->Sel(1)));
		stack.PushConstTable(CloneConstants());
	      }
	    }
	  }
	  break;
	case AbstractCode::CompactTagTest:
	  {
	    TagVal *idRef = TagVal::FromWordDirect(instr->Sel(0));
	    u_int tag;
	    bool isValue = false;
	    u_int localIndex;
	    bool isLocal = false;
	    switch(AbstractCode::GetIdRef(idRef)) {
	    case AbstractCode::LastUseLocal:
	    case AbstractCode::Local:
	      {
		u_int index = IdToId(idRef->Sel(0));
		switch(ExtractKind(index)) {
		case INT:
		case TAGVAL:
		  {	
		    tag = Store::DirectWordToInt(ExtractValue(index));
		    isValue = true;
		  }
		  break;
		default:
		  ;
		}
		isLocal = true;
		localIndex = index;
	      }
	      break;
	    case AbstractCode::Global:
	      {
		u_int addr = Store::DirectWordToInt(idRef->Sel(0));
		TagVal *valueOpt = TagVal::FromWord(subst->Sub(addr));
		if(valueOpt == INVALID_POINTER)
		  break;
		idRef = valueOpt;
		// fall through
	      }
	    case AbstractCode::Immediate:
	      {
		word immediate = idRef->Sel(0);
		if(PointerOp::IsInt(immediate)) {
		  tag = Store::DirectWordToInt(immediate);
		} else {
		  Block *block = Store::WordToBlock(immediate);
		  if (block->GetLabel() == Alice::BIG_TAG) {
		    BigTagVal *bigTagVal = 
		      BigTagVal::FromWordDirect(immediate);
		    tag = bigTagVal->GetTag();
		  } else {
		    TagVal *tagVal = TagVal::FromWordDirect(immediate);
		    tag = tagVal->GetTag();
		  }
		}
		isValue = true;
	      }
	      break;
	    default:
	      ;
	    }
	    TagVal *elseInstrOpt = TagVal::FromWord(instr->Sel(3));
	    if(isValue) {
	      static u_int counter = 0;
	      Vector *tests = Vector::FromWordDirect(instr->Sel(2));
	      bool tagFound = false;
	      for(u_int i = tests->GetLength(); i--; ) {
		Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
		if(i == tag) {
		  fprintf(stderr,"%"U_INTF". eliminate compact tag %p test:",
			  ++counter,instr);
		  fprintf(stderr," value has tag %"U_INTF"\n",tag);		  
		  // save information
		  tagtestInfo->Put(instr->ToWord(),pair->ToWord());
		  tagFound = true;
		  break;
		}
	      }
	      if(!tagFound) {
		if(elseInstrOpt != INVALID_POINTER) {
		  fprintf(stderr,"%"U_INTF". eliminate compact tag %p test:",
			  ++counter,instr);		  
		  fprintf(stderr," value has tag %"U_INTF"\n",tag);
		  Tuple *pair = Tuple::New(2);
		  pair->Init(0, Store::IntToWord(0));
		  pair->Init(1, elseInstrOpt->Sel(0));
		  tagtestInfo->Put(instr->ToWord(),pair->ToWord());
		} else {
		  fprintf(stderr,"ERROR: tag %"U_INTF"\n",tag);
		  AbstractCode::Disassemble(stderr,instr);
		  Error("internal consistancy error\n");
		}
	      }
	    } else if (isLocal) {
	      // push continuations and propagate information
	      // into the branches
	      if(elseInstrOpt != INVALID_POINTER) {		
		stack.PushInstr(TagVal::FromWordDirect(elseInstrOpt->Sel(0)));
		stack.PushConstTable(constants);
	      }
	      Vector *tests = Vector::FromWordDirect(instr->Sel(2));
	      for(u_int i = tests->GetLength(); i--; ) {
		Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
		Array *constantsCopy = CloneConstants();
		TagVal *info = TagVal::New(TAGVAL,1);
		info->Init(0, Store::IntToWord(i));
		constantsCopy->Update(localIndex, info->ToWord());
		stack.PushInstr(TagVal::FromWordDirect(pair->Sel(1)));
		stack.PushConstTable(constantsCopy);
	      }
	    } else {
	      if(elseInstrOpt != INVALID_POINTER) {
		stack.PushInstr(TagVal::FromWordDirect(elseInstrOpt->Sel(0)));
		stack.PushConstTable(constants);
	      }
	      Vector *tests = Vector::FromWordDirect(instr->Sel(2));
	      for(u_int i = tests->GetLength(); i--; ) {
		Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
		stack.PushInstr(TagVal::FromWordDirect(pair->Sel(1)));
		stack.PushConstTable(CloneConstants());
	      }	      
	    }
	  }
	  break;
	case AbstractCode::Shared:
	  {
	    word key = instr->ToWord(); // instr->Sel(0);
	    if(sharedInArity->IsMember(key)) {
	      u_int inArity = Store::DirectWordToInt(sharedInArity->Get(key));
	      sharedInArity->Put(key,Store::IntToWord(--inArity));
	      if(inArity == 0) {
		stack.PushInstr(TagVal::FromWordDirect(instr->Sel(1)));
	      } 
	    } else {
	      Error("Internal consistancy error: unkown shared node!\n");
	    }
	    // merge information of the branches
	    if(sharedInfo->IsMember(key)) {
	      Array *constants2 = 
		Array::FromWordDirect(sharedInfo->Get(key));
	      MergeConstantTable(constants2);
	    }
	    sharedInfo->Put(key, constants->ToWord());
	  }
	  break;
	default:
	  fprintf(stderr,"main pass: invalid abstractCode tag %"U_INTF"\n",
		  static_cast<u_int>(AbstractCode::GetInstr(instr)));
	  return;
	}
      }
      break;
    default:
      {
	Error("internal consistancy error in control stack\n");
      }
    }
  }
}

Map *ByteCodeConstProp::RunAnalysis(TagVal *abstractCode, 
				    InlineInfo *inlineInfo) {
//   static u_int c = 0;
//   Tuple *coord = Tuple::FromWordDirect(abstractCode->Sel(0));
//   std::fprintf(stderr, "%d. do constant propagation for %p %s:%d.%d\n",
// 	       ++c,
// 	       abstractCode,
// 	       String::FromWordDirect(coord->Sel(0))->ExportC(),
// 	       Store::DirectWordToInt(coord->Sel(1)),
// 	       Store::DirectWordToInt(coord->Sel(2))); 
//   AbstractCode::Disassemble(stderr,
//  			    TagVal::FromWordDirect(abstractCode->Sel(5)));
  TagVal *root = TagVal::FromWordDirect(abstractCode->Sel(5));
  ConstProp_PrePass prePass(abstractCode, inlineInfo);
//   fprintf(stderr,"run pre pass\n");
  prePass.Run();
  ConstProp_MainPass mainPass(abstractCode,
			      prePass.GetSharedInArity(),
			      inlineInfo);
//   fprintf(stderr,"run main pass\n");
  mainPass.Run();
  return mainPass.GetTagTestInfo();
}

