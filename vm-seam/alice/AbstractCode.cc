//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "alice/AbstractCode.hh"
#endif

#include <stack>
#include "alice/AbstractCode.hh"


static const char *opcodeNames[AbstractCode::nInstrs] = {
  "AppPrim", "AppVar", "Close", "CompactIntTest", "CompactTagTest", "ConTest",
  "Coord", "EndHandle", "EndTry", "Entry", "Exit", "GetRef", "GetTup", "IntTest",
  "Kill", "LazyPolySel", "PutCon", "PutNew", "PutPolyRec", "PutRef", "PutTag",
  "PutTup", "PutVar", "PutVec", "Raise", "RealTest", "Reraise", "Return", "Sel",
  "Shared", "Specialize", "StringTest", "TagTest", "Try", "VecTest"
};


const char *AbstractCode::GetOpcodeName(instr opcode) {
  return opcodeNames[opcode];
}


const char *AbstractCode::GetOpcodeName(TagVal *pc) {
  return GetOpcodeName(AbstractCode::GetInstr(pc));
}


s_int AbstractCode::GetContinuationPos(instr instr) {
  switch (instr) {
    case EndTry:
    case EndHandle:
      return 0;
    case Coord:
    case Kill:
    case Shared:
      return 1;
    case Entry:
    case PutVar:
    case PutNew:
    case PutRef:
    case PutTup:
    case PutVec:
    case GetRef:
    case GetTup:
      return 2;
    case Exit:
    case PutCon:
    case PutPolyRec:
    case Close:
    case Specialize:
    case Sel:
    case LazyPolySel:
      return 3;
    case PutTag:
      return 4;
    case Raise:
    case Reraise:
    case Try:
    case IntTest:
    case CompactIntTest:
    case RealTest:
    case StringTest:
    case Return:
    case AppPrim:
    case AppVar:
    case TagTest:
    case CompactTagTest:
    case VecTest:
      return -1;
    default:
      Assert(false);
  }
}


s_int AbstractCode::GetNumProgramPoints(instr instr) {
  switch (instr) {
    case Coord:
    case Kill:
    case EndTry:
    case EndHandle:
    case Shared:
      return 0;
    case Entry:
    case Exit:
    case PutNew:
    case GetTup:
    case Raise:
    case Reraise:
    case Try:
    case IntTest:
    case CompactIntTest:
    case RealTest:
    case StringTest:
    case Return:
      return 1;
    case PutVar:
    case PutTag:
    case PutCon:
    case PutRef:
    case PutTup:
    case PutPolyRec:
    case PutVec:
    case Close:
    case Specialize:
    case GetRef:
    case Sel:
    case LazyPolySel:
      return 2;
    case AppPrim:
    case AppVar:
    case TagTest:
    case CompactTagTest:
    case VecTest:
      return -1;
    default:
      Assert(false);
  }
}

IntMap *AbstractCode::SharedInArity(TagVal *abstractCode) {
  
  IntMap *sharedInArity = IntMap::New(16);
  std::stack<word> toVisit;
  
  toVisit.push(abstractCode->Sel(5));
  while (!toVisit.empty()) {
    TagVal *instr = TagVal::FromWordDirect(toVisit.top());
    toVisit.pop();
    AbstractCode::instr instrOp = GetInstr(instr);
    switch (instrOp) {
      case Raise:
      case Reraise:
      case Return:
	break;
      case Coord:
      case Entry:
      case Exit:
      case EndHandle:
      case EndTry:
      case Kill:
      case GetRef:
      case GetTup:
      case PutNew:
      case PutRef:
      case PutTup:
      case PutVar:
      case PutVec:
      case Close:
      case LazyPolySel:
      case PutCon:
      case PutPolyRec:
      case Sel: 
      case Specialize:
      case PutTag: {
	u_int cp = GetContinuationPos(instrOp);
	toVisit.push(instr->Sel(cp)); 
	break;
      }
      case AppPrim: {
	TagVal *contOpt = TagVal::FromWord(instr->Sel(2));
	if(contOpt != INVALID_POINTER) {
	  Tuple *cont = Tuple::FromWordDirect(contOpt->Sel(0));
	  toVisit.push(cont->Sel(1));
	}
	break;
      }
      case AppVar: {
	TagVal *contOpt = TagVal::FromWord(instr->Sel(3));
	if(contOpt != INVALID_POINTER) {
	  Tuple *cont = Tuple::FromWordDirect(contOpt->Sel(0));
	  toVisit.push(cont->Sel(1));
	}
	break;
      }
      case Try: {
	toVisit.push(instr->Sel(3));
	toVisit.push(instr->Sel(0));
	break;
      }
      case IntTest:
      case RealTest:
      case StringTest:
      case VecTest: {
	Vector *tests = Vector::FromWordDirect(instr->Sel(1));
	for (u_int i = tests->GetLength(); i--; ) {
	  Tuple *test = Tuple::FromWordDirect(tests->Sub(i));
	  toVisit.push(test->Sel(1));
	}
	toVisit.push(instr->Sel(2));
	break;
      }
      case CompactIntTest: {
	toVisit.push(instr->Sel(3));
	Vector *tests = Vector::FromWordDirect(instr->Sel(2));
	for (u_int i = tests->GetLength(); i--; ) {
	  toVisit.push(tests->Sub(i));
	}
	break;
      }
      case ConTest: {
	Vector *tests0 = Vector::FromWordDirect(instr->Sel(1));
	for(u_int i = tests0->GetLength(); i--; ) {
	  Tuple *test = Tuple::FromWordDirect(tests0->Sub(i));	  
	  toVisit.push(test->Sel(1));
	}
	Vector *testsN = Vector::FromWordDirect(instr->Sel(2));
	for(u_int i = testsN->GetLength(); i--; ) {
	  Tuple *test = Tuple::FromWordDirect(testsN->Sub(i));
	  toVisit.push(test->Sel(2));
	}
	toVisit.push(instr->Sel(3));
	break;
      }
      case TagTest: {
	Vector *tests0 = Vector::FromWordDirect(instr->Sel(2));
	for(u_int i = tests0->GetLength(); i--; ) {
	  Tuple *test = Tuple::FromWordDirect(tests0->Sub(i));	  
	  toVisit.push(test->Sel(1));
	}
	Vector *testsN = Vector::FromWordDirect(instr->Sel(3));
	for(u_int i = testsN->GetLength(); i--; ) {
	  Tuple *test = Tuple::FromWordDirect(testsN->Sub(i));
	  toVisit.push(test->Sel(2));
	}
	toVisit.push(instr->Sel(4));
	break;
      }
      case CompactTagTest: {
	TagVal *elseInstrOpt = TagVal::FromWord(instr->Sel(3));
	if(elseInstrOpt != INVALID_POINTER)
	  toVisit.push(elseInstrOpt->Sel(0));
	Vector *tests = Vector::FromWordDirect(instr->Sel(2));
	for(u_int i = tests->GetLength(); i--; ) {
	  Tuple *pair = Tuple::FromWordDirect(tests->Sub(i));
	  toVisit.push(pair->Sel(1));
	}
	break;
      }
      case Shared: {
	word stamp = instr->Sel(0);
	word arity = sharedInArity->CondGet(stamp);
	
	if(arity == INVALID_POINTER) {
	  toVisit.push(instr->Sel(1));
	  sharedInArity->Put(stamp, Store::IntToWord(1)); 
	} else {
	  u_int inArity = Store::DirectWordToInt(arity);
	  sharedInArity->Put(stamp, Store::IntToWord(inArity + 1));
	}
	break;
      }
      default: {
	Assert(false);
      }
    }
  }
  
  return sharedInArity;
}
  

#define OPT(w, X) {				\
  TagVal *opt = TagVal::FromWord(w);		\
  if (opt == INVALID_POINTER)			\
    std::fprintf(file, " NONE");		\
  else {					\
    std::fprintf(file, " SOME");		\
    X(opt->Sel(0));				\
  }						\
}

#define TUPLE(w, X, Y) {			\
  Tuple *tuple = Tuple::FromWordDirect(w);	\
  std::fprintf(file, " (");			\
  X(tuple->Sel(0));				\
  Y(tuple->Sel(1));				\
  std::fprintf(file, " )");			\
}

#define TRIPLE(w, X, Y, Z) {			\
  Tuple *tuple = Tuple::FromWordDirect(w);	\
  std::fprintf(file, " (");			\
  X(tuple->Sel(0));				\
  Y(tuple->Sel(1));				\
  Z(tuple->Sel(2));				\
  std::fprintf(file, " )");			\
}

#define VECTOR(w, X) {					\
  Vector *vector = Vector::FromWordDirect(w);		\
  std::fprintf(file, " %"U_INTF"#[", vector->GetLength());	\
  for (u_int i = 0; i < vector->GetLength(); i++) {	\
    X(vector->Sub(i));					\
  }							\
  std::fprintf(file, " ]");				\
}

#define INSTR             Instr(pc->Sel(operand++));
#define LASTINSTR         LastInstr(pc->Sel(operand++));
#define LASTINSTROPT      OPT(pc->Sel(operand++), LastInstr);
#define INSTRS            VECTOR(pc->Sel(operand++), Instr);
#define INT               Int(pc->Sel(operand++));
#define INTOPT            OPT(pc->Sel(operand++), Int);
#define STRING            Value(pc->Sel(operand++));
#define VALUE             Value(pc->Sel(operand++));
#define TEMPLATE          Template(pc->Sel(operand++));
#define STAMP             Int(pc->Sel(operand++));
#define ID                Int(pc->Sel(operand++));
#define IDS               VECTOR(pc->Sel(operand++), Int);
#define IDREF             IdRef(pc->Sel(operand++));
#define IDREFS            VECTOR(pc->Sel(operand++), IdRef);
#define IDDEF             IdDef(pc->Sel(operand++));
#define IDDEFS            VECTOR(pc->Sel(operand++), IdDef);
#define IDDEFINSTROPT     OPT(pc->Sel(operand++), IdDefInstr);
#define IDDEFSINSTROPT    OPT(pc->Sel(operand++), IdDefsInstr);
#define LABEL             Label(pc->Sel(operand++));
#define LABELS            VECTOR(pc->Sel(operand++), Label);
#define INTINSTRVEC       VECTOR(pc->Sel(operand++), IntInstr);
#define REALINSTRVEC      VECTOR(pc->Sel(operand++), RealInstr);
#define STRINGINSTRVEC    VECTOR(pc->Sel(operand++), StringInstr);
#define NULLARYTAGTESTS   INTINSTRVEC
#define NARYTAGTESTS      VECTOR(pc->Sel(operand++), NaryTagTest);
#define IDDEFSOPTINSTRVEC VECTOR(pc->Sel(operand++), IdDefsOptInstr);
#define NULLARYCONTESTS   VECTOR(pc->Sel(operand++), IdRefInstr);
#define NARYCONTESTS      VECTOR(pc->Sel(operand++), IdRefIdDefsInstr);
#define IDDEFSINSTRVEC    VECTOR(pc->Sel(operand++), IdDefsInstr);
#define COORD             Coord(pc->Sel(operand++));
#define ENTRYPOINT        EntryPoint(pc->Sel(operand++));
#define EXITPOINT         ExitPoint(pc->Sel(operand++));

class Disassembler {
private:
  static const u_int initialSize = 19; //--** to be determined

  std::FILE *file;
  Stack *todo;
  Map *done;
  Queue *immediates;
  TagVal *pc;
  u_int operand;

  void Instr(word w) {
    TagVal *instr = TagVal::FromWordDirect(w);
    todo->SlowPush(instr->ToWord());
    std::fprintf(file, " %p", instr);
  }
  void LastInstr(word w) {
    TagVal *instr = TagVal::FromWordDirect(w);
    if (done->IsMember(w))
      std::fprintf(file, " %p", instr);
    else
      todo->SlowPush(instr->ToWord());
  }
  void Int(word w) {
    std::fprintf(file, " %"S_INTF, Store::DirectWordToInt(w));
  }
  void Value(word value) {
    s_int i = Store::WordToInt(value);
    if (i != INVALID_INT)
      std::fprintf(file, " int(%"S_INTF")", i);
    else {
      //--** treat chunks specially
      immediates->Enqueue(value);
      std::fprintf(file, " %p", value);
    }
  }
  void IdDef(word w) {
    TagVal *idDef = TagVal::FromWord(w);
    if (idDef == INVALID_POINTER)
      std::fprintf(file, " Wildcard");
    else
      std::fprintf(file, " IdDef(%"S_INTF")", Store::DirectWordToInt(idDef->Sel(0)));
  }
  void IdDefs(word w) {
    VECTOR(w, IdDef);
  }
  void IdRef(word w) {
    TagVal *idRef = TagVal::FromWordDirect(w);
    switch (AbstractCode::GetIdRef(idRef)) {
    case AbstractCode::Immediate:
      std::fprintf(file, " Immediate(");
      Value(idRef->Sel(0));
      std::fprintf(file, " )");
      break;
    case AbstractCode::Local:
      std::fprintf(file, " Local(%"S_INTF")", Store::DirectWordToInt(idRef->Sel(0)));
      break;
    case AbstractCode::LastUseLocal:
      std::fprintf(file, " LastUseLocal(%"S_INTF")",
		   Store::DirectWordToInt(idRef->Sel(0)));
      break;
    case AbstractCode::Global:
      std::fprintf(file, " Global(%"S_INTF")", Store::DirectWordToInt(idRef->Sel(0)));
      break;
    default:
      Assert(false);
    }
  }
  void IdDefInstr(word w) {
    TUPLE(w, IdDef, Instr);
  }
  void Label(word w) {
    std::fprintf(file, " %s",
		 UniqueString::FromWordDirect(w)->ToString()->ExportC());
  }
  void FunCoord(word w) {
    Tuple *funCoord = Tuple::FromWord(w);
    String *path = String::FromWordDirect(funCoord->Sel(0));
    String *name = String::FromWordDirect(funCoord->Sel(1));
    s_int line   = Store::WordToInt(funCoord->Sel(2));
    s_int col    = Store::WordToInt(funCoord->Sel(3));
    std::fprintf(file, " (%s at %s:%"S_INTF".%"S_INTF"", name->ExportC(), path->ExportC(), line, col);
  }
  void Coord(word w) {
    Tuple *coord = Tuple::FromWord(w);
    s_int line = Store::WordToInt(coord->Sel(0));
    s_int col  = Store::WordToInt(coord->Sel(1));
    std::fprintf(file, " (%"S_INTF".%"S_INTF")", line, col);
  }
  void Type(word w) {
    std::fprintf(file, " <type>");
  }
  void EntryPoint(word w) {
    TagVal *tagVal = TagVal::FromWord(w);
    AbstractCode::entryPoint tag;
    if (tagVal == INVALID_POINTER) {
      Assert(Store::DirectWordToInt(w) == AbstractCode::SpawnEntry);
      tag = AbstractCode::SpawnEntry;
    } else {
      tag = AbstractCode::GetEntryPoint(tagVal);
    }
    switch (tag) {
    case AbstractCode::ConEntry: // of Type.t * idRef * idRef vector
      std::fprintf(file, " Con(");
      Type(tagVal->Sel(0));
      IdRef(tagVal->Sel(1));
      VECTOR(tagVal->Sel(2), IdRef);
      std::fprintf(file, " )");
      break;
    case AbstractCode::SelEntry: // of int * Type.t * idRef
      std::fprintf(file, " Sel(");
      Int(tagVal->Sel(0));
      Type(tagVal->Sel(1));
      IdRef(tagVal->Sel(2));
      std::fprintf(file, " )");
      break;
    case AbstractCode::StrictEntry: // of Type.t * idRef
      std::fprintf(file, " Strict(");
      Type(tagVal->Sel(0));
      IdRef(tagVal->Sel(1));
      std::fprintf(file, " )");
      break;
    case AbstractCode::AppEntry: // of Type.t * idRef * idRef vector
      std::fprintf(file, " App(");
      Type(tagVal->Sel(0));
      IdRef(tagVal->Sel(1));
      VECTOR(tagVal->Sel(2), IdRef);
      std::fprintf(file, " )");
      break;
    case AbstractCode::CondEntry: // of Type.t * idRef
      std::fprintf(file, " Cond(");
      Type(tagVal->Sel(0));
      IdRef(tagVal->Sel(1));
      std::fprintf(file, " )");
      break;
    case AbstractCode::RaiseEntry: // of idRef
      std::fprintf(file, " Raise(");
      IdRef(tagVal->Sel(0));
      std::fprintf(file, " )");
      break;
    case AbstractCode::HandleEntry: // of idRef
      std::fprintf(file, " Handle(");
      IdRef(tagVal->Sel(0));
      std::fprintf(file, " )");
      break;
    case AbstractCode::SpawnEntry:
      std::fprintf(file, " spawn");
      break;
    }
  }
  void ExitPoint(word w) {
    TagVal *tagVal = TagVal::FromWord(w);
    AbstractCode::exitPoint tag;
    if (tagVal == INVALID_POINTER) {
      tag = static_cast<AbstractCode::exitPoint>(Store::WordToInt(w));
    } else {
      tag = AbstractCode::GetExitPoint(tagVal);
    }
    switch (tag) {
    case AbstractCode::ConExit:
      std::fprintf(file, " Con");
      break;
    case AbstractCode::SelExit: // of Type.t
      std::fprintf(file, " Sel(");
      Type(tagVal->Sel(0));
      std::fprintf(file, " )");
      break;
    case AbstractCode::StrictExit:
      std::fprintf(file, " Strict");
      break;
    case AbstractCode::AppExit:
      std::fprintf(file, " App");
      break;
    case AbstractCode::CondExit: // of Type.t
      std::fprintf(file, " Cond(");
      Type(tagVal->Sel(0));
      std::fprintf(file, " )");
      break;
    case AbstractCode::RaiseExit: // of Type.t
      std::fprintf(file, " Raise(");
      Type(tagVal->Sel(0));
      std::fprintf(file, " )");
      break;
    case AbstractCode::HandleExit: // of Type.t
      std::fprintf(file, " Handle(");
      Type(tagVal->Sel(0));
      std::fprintf(file, " )");
      break;
    case AbstractCode::SpawnExit: // of Type.t
      std::fprintf(file, " Spawn(");
      Type(tagVal->Sel(0));
      std::fprintf(file, " )");
      break;
    } 
  }
  void Template(word w) {
    TagVal *templ = TagVal::FromWordDirect(w);
    std::fprintf(file, " Template(");
    FunCoord(templ->Sel(0));
    Int(templ->Sel(1));
    std::fprintf(file, " Debug Annotation\n");
    VECTOR(templ->Sel(3), IdDef);
    OPT(templ->Sel(4), Int);
    Instr(templ->Sel(5));
    std::fprintf(file, " )");
  }
  void IntInstr(word w) {
    TUPLE(w, Int, Instr);
  }
  void RealInstr(word w) {
    TUPLE(w, Value, Instr);
  }
  void StringInstr(word w) {
    TUPLE(w, Value, Instr);
  }
  void IdDefsOpt(word w) {
    OPT(w, IdDefs);
  }
  void IdDefsOptInstr(word w) {
    TUPLE(w, IdDefsOpt, Instr);
  }
  void IdRefInstr(word w) {
    TUPLE(w, IdRef, Instr);
  }
  void IdRefIdDefsInstr(word w) {
    TRIPLE(w, IdRef, IdDefs, Instr);
  }
  void IdDefsInstr(word w) {
    TUPLE(w, IdDefs, Instr);
  }
  void NaryTagTest(word w) {
    TRIPLE(w, Int, IdDefs, Instr);
  }
public:
  Disassembler(std::FILE *f, TagVal *pc): file(f) {
    todo = Stack::New(initialSize);
    todo->SlowPush(pc->ToWord());
    done = Map::New(initialSize);
    immediates = Queue::New(initialSize);
  }

  void Start();
  void DumpImmediates();
};

void Disassembler::Start() {
  while (!todo->IsEmpty()) {
    pc = TagVal::FromWordDirect(todo->Pop());
    if (done->IsMember(pc->ToWord()))
      continue;
    done->Put(pc->ToWord(), Store::IntToWord(0));
    operand = 0;
    std::fprintf(file, "%p %s", pc, AbstractCode::GetOpcodeName(pc));
    switch (AbstractCode::GetInstr(pc)) {
    case AbstractCode::Kill:
      IDS LASTINSTR break;
    case AbstractCode::PutVar:
      ID IDREF LASTINSTR break;
    case AbstractCode::PutNew:
      ID STRING LASTINSTR break;
    case  AbstractCode::PutTag:
      ID INT INT IDREFS LASTINSTR break;
    case AbstractCode::PutCon:
      ID IDREF IDREFS LASTINSTR break;
    case AbstractCode::PutRef:
      ID IDREF LASTINSTR break;
    case AbstractCode::PutTup:
      ID IDREFS LASTINSTR break;
    case AbstractCode::PutPolyRec:
      ID LABELS IDREFS LASTINSTR break;
    case AbstractCode::PutVec:
      ID IDREFS LASTINSTR break;
    case AbstractCode::Close:
      ID IDREFS VALUE LASTINSTR break;
    case AbstractCode::Specialize:
      ID IDREFS TEMPLATE LASTINSTR break;
    case AbstractCode::AppPrim:
      VALUE IDREFS IDDEFINSTROPT break;
    case AbstractCode::AppVar:
      IDREF IDREFS INT IDDEFSINSTROPT break;
    case AbstractCode::GetRef:
      ID IDREF LASTINSTR break;
    case AbstractCode::GetTup:
      IDDEFS IDREF LASTINSTR break;
    case AbstractCode::Sel:
      ID IDREF INT LASTINSTR break;
    case AbstractCode::LazyPolySel:
      IDS IDREF LABELS LASTINSTR break;
    case AbstractCode::Raise:
      IDREF break;
    case AbstractCode::Reraise:
      IDREF break;
    case AbstractCode::Try:
      INSTR IDDEF IDDEF INSTR break;
    case AbstractCode::EndTry:
      LASTINSTR break;
    case AbstractCode::Coord:
      COORD LASTINSTR break;
    case AbstractCode::Entry:
      COORD ENTRYPOINT LASTINSTR break;
    case AbstractCode::Exit:
      COORD EXITPOINT IDREF LASTINSTR break;
    case AbstractCode::EndHandle:
      LASTINSTR break;
    case AbstractCode::IntTest:
      IDREF INTINSTRVEC LASTINSTR break;
    case AbstractCode::CompactIntTest:
      IDREF INT INSTRS LASTINSTR break;
    case AbstractCode::RealTest:
      IDREF REALINSTRVEC LASTINSTR break;
    case AbstractCode::StringTest:
      IDREF STRINGINSTRVEC LASTINSTR break;
    case AbstractCode::TagTest:
      IDREF INT NULLARYTAGTESTS NARYTAGTESTS LASTINSTR break;
    case AbstractCode::CompactTagTest:
      IDREF INT IDDEFSOPTINSTRVEC LASTINSTROPT break;
    case AbstractCode::ConTest:
      IDREF NULLARYCONTESTS NARYCONTESTS LASTINSTR break;
    case AbstractCode::VecTest:
      IDREF IDDEFSINSTRVEC LASTINSTR break;
    case AbstractCode::Shared:
      STAMP LASTINSTR break;
    case AbstractCode::Return:
      IDREFS break;
    default:
      Error("AbstractCode::Disassemble: unknown instr tag");
    }
    std::fprintf(file, "\n");
  }
}

void Disassembler::DumpImmediates() {
  while (!immediates->IsEmpty()) {
    word value = immediates->Dequeue();
    std::fprintf(file, "\nValue at %p:\n\n", value);
    Debug::DumpTo(file, value);
  }
}

void AbstractCode::Disassemble(std::FILE *f, TagVal *pc) {
  Disassembler disassembler(f, pc);
  disassembler.Start();
  disassembler.DumpImmediates();
  std::fflush(f);
}
