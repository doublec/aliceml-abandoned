/*
 * Authors:
 *  Guido Tack <tack@ps.uni-sb.de>
 *
 * Copyright:
 *  Guido Tack, 2003
 *
 *  Last change:
 *    $Date$ by $Author$
 *    $Revision$
 * 
 */

#include "Alice.hh"
#include "GecodeSpace.hh"
#include "GecodeBAB.hh"

#define DECLARE_SPACE(s, x)                       \
  GecodeSpace *s;                                                  \
  if (Store::WordToTransient(x) != INVALID_POINTER) { REQUEST(x); } \
  { ConcreteRepresentation *cr = ConcreteRepresentation::FromWordDirect(x); \
    s = (GecodeSpace *) Store::WordToUnmanagedPointer(cr->Get(0)); }

#define DECLARE_ENGINE(se, x)                       \
  Search<GecodeSpace> *se;                                                  \
  if (Store::WordToTransient(x) != INVALID_POINTER) { REQUEST(x); } \
  { ConcreteRepresentation *cr = ConcreteRepresentation::FromWordDirect(x); \
    se = (Search<GecodeSpace> *) Store::WordToUnmanagedPointer(cr->Get(0)); }

#define DECLARE_BESTENGINE(se, x)                                           \
  SearchBestExplCallback *se;                                  \
  if (Store::WordToTransient(x) != INVALID_POINTER) { REQUEST(x); }         \
  { ConcreteRepresentation *cr = ConcreteRepresentation::FromWordDirect(x); \
    se = (SearchBestExplCallback *)                            \
        Store::WordToUnmanagedPointer(cr->Get(0)); }

#define DEFINE6(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::nArgs == 6);			\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];		\
    word x2 = Scheduler::currentArgs[2];		\
    word x3 = Scheduler::currentArgs[3];		\
    word x4 = Scheduler::currentArgs[4];                \
    word x5 = Scheduler::currentArgs[5];

namespace UnsafeGecode {


const BvarSel int2bvarsel[] =
  {
    BVAR_MAX_MAX, BVAR_MAX_MIN,
    BVAR_MIN_MAX, BVAR_MIN_MIN,
    BVAR_NONE, BVAR_SIZE_MAX,
    BVAR_SIZE_MIN, BVAR_WIDTH_MAX,
    BVAR_WIDTH_MIN
  };

const BvalSel int2bvalsel[] =
  {
    BVAL_MAX, BVAL_MED, BVAL_MIN,
    BVAL_SPLIT_MAX, BVAL_SPLIT_MIN,
    BVAL_VAL
  };

const reltype int2reltype[] =
  {
    REL_EQ, REL_GQ, REL_GR,
    REL_LE, REL_LQ, REL_NQ
  };

const conlevel int2cl[] =
  {
    CL_BND, CL_BND_EX, CL_DEF, CL_DOM,
    CL_DOM_EX, CL_OTR, CL_VAL, CL_VAL_EX
  };

class GecodeHandler : public ConcreteRepresentationHandler {
  Transform
  *GecodeHandler::GetAbstractRepresentation(ConcreteRepresentation *) {
    return INVALID_POINTER;
  }
};


static GecodeHandler *gecodeHandler;

class GecodeFinalizationSet: public FinalizationSet {
public:
  virtual void Finalize(word value);
};

static GecodeFinalizationSet *gecodeFinalizationSet;

void GecodeFinalizationSet::Finalize(word value) {
  ConcreteRepresentation *cr = ConcreteRepresentation::FromWordDirect(value);
  word ptr = cr->Get(0);
  int type = Store::DirectWordToInt(cr->Get(1));
  switch(type) {
  case 0:
    {
      GecodeSpace *s = (GecodeSpace *)Store::WordToUnmanagedPointer(ptr);
      delete s;
    }
    break;
  case 1:
    {
      Search<GecodeSpace> *s =
	(Search<GecodeSpace> *)Store::WordToUnmanagedPointer(ptr);
      delete s;
    }
    break;
  case 2:
    {
      SearchBestExplCallback *s =
	(SearchBestExplCallback *)
	Store::WordToUnmanagedPointer(ptr);
      delete s;
    }
    break;
  }
}

}

DEFINE0(gc_makespace) {
  GecodeSpace *s = new GecodeSpace();

  ConcreteRepresentation *cr =
    ConcreteRepresentation::New(UnsafeGecode::gecodeHandler,2);
  cr->Init(0, Store::UnmanagedPointerToWord(s));
  cr->Init(1, Store::IntToWord(0));
  UnsafeGecode::gecodeFinalizationSet->Register(cr->ToWord());
  RETURN(cr->ToWord());
} END

DEFINE3(gc_intvar) {
  DECLARE_SPACE(s, x0);
  DECLARE_INT(i, x1);
  DECLARE_INT(j, x2);

  int newVar = s->AddIntVariable(i, j);
  RETURN_INT(newVar);
} END

DEFINE4(gc_intvarr) {
  DECLARE_SPACE(s, x0);
  DECLARE_INT(i, x1);
  DECLARE_INT(j, x2);
  DECLARE_INT(boolVar, x3);

  int newVar = s->AddIntVariableR(i, j, boolVar);
  RETURN_INT(newVar);
} END

DEFINE1(gc_boolvar) {
  DECLARE_SPACE(s, x0);

  int newVar = s->AddBoolVariable();
  RETURN_INT(newVar);
} END

DEFINE2(gc_getmin) {
  DECLARE_SPACE(s, x0);
  DECLARE_INT(var, x1);
  RETURN_INT(s->vmin(var));
} END

DEFINE2(gc_getmax) {
  DECLARE_SPACE(s, x0);
  DECLARE_INT(var, x1);
  RETURN_INT(s->vmax(var));
} END

DEFINE4(gc_rel) {
  DECLARE_SPACE(s, x0);
  DECLARE_INT(i, x1);
  DECLARE_INT(rel, x2);
  DECLARE_INT(j, x3);
  s->trel(i, UnsafeGecode::int2reltype[rel], j);
} END

DEFINE4(gc_reli) {
  DECLARE_SPACE(s, x0);
  DECLARE_INT(i, x1);
  DECLARE_INT(rel, x2);
  DECLARE_INT(j, x3);
  s->treli(i, UnsafeGecode::int2reltype[rel], j);
} END

DEFINE5(gc_relr) {
  DECLARE_SPACE(s, x0);
  DECLARE_INT(i, x1);
  DECLARE_INT(rel, x2);
  DECLARE_INT(j, x3);
  DECLARE_INT(boolVar, x4);
  s->trelR(i, UnsafeGecode::int2reltype[rel], j, boolVar);
} END

DEFINE5(gc_relir) {
  DECLARE_SPACE(s, x0);
  DECLARE_INT(i, x1);
  DECLARE_INT(rel, x2);
  DECLARE_INT(j, x3);
  DECLARE_INT(boolVar, x4);
  s->treliR(i, UnsafeGecode::int2reltype[rel], j, boolVar);
} END

DEFINE4(gc_eq) {
  DECLARE_SPACE(s, x0);
  DECLARE_INT(i, x1);
  DECLARE_INT(j, x2);
  DECLARE_INT(cl, x3);
  s->teq(i, j, UnsafeGecode::int2cl[cl]);
} END

DEFINE4(gc_eqv) {
  DECLARE_SPACE(s, x0);
  DECLARE_VECTOR(v, x1);
  DECLARE_INT(cl, x2);
  int noOfVars = v->GetLength();

  int *vars = new int[noOfVars];
  for (int i=noOfVars; i--;) {
    DECLARE_INT(tmp, v->Sub(i));
    vars[i] = tmp;
  }

  s->teq(vars, noOfVars, UnsafeGecode::int2cl[cl]);
} END

DEFINE5(gc_eqr) {
  DECLARE_SPACE(s, x0);
  DECLARE_INT(i, x1);
  DECLARE_INT(j, x2);
  DECLARE_INT(cl, x3);
  DECLARE_INT(boolVar, x4);
  s->teqR(i, j, boolVar, UnsafeGecode::int2cl[cl]);
} END

DEFINE5(gc_eqvr) {
  DECLARE_SPACE(s, x0);
  DECLARE_VECTOR(v, x1);
  DECLARE_INT(cl, x2);
  DECLARE_INT(boolVar, x4);

  int noOfVars = v->GetLength();

  int *vars = new int[noOfVars];
  for (int i=noOfVars; i--;) {
    DECLARE_INT(tmp, v->Sub(i));
    vars[i] = tmp;
  }

  s->teqR(vars, noOfVars, boolVar, UnsafeGecode::int2cl[cl]);
} END

DEFINE3(gc_distinct) {
  DECLARE_SPACE(s, x0);
  DECLARE_VECTOR(v, x1);
  DECLARE_INT(cl, x2);

  int noOfVars = v->GetLength();

  int *vars = new int[noOfVars];
  for (int i=noOfVars; i--;) {
    DECLARE_INT(tmp, v->Sub(i));
    vars[i] = tmp;
  }
  s->tdistinct(vars, noOfVars, UnsafeGecode::int2cl[cl]);
  delete vars;
  RETURN_UNIT;
} END

DEFINE3(gc_distincti) {
  DECLARE_SPACE(s, x0);
  DECLARE_VECTOR(v, x1);
  DECLARE_INT(cl, x2);

  int noOfVars = v->GetLength();

  int *vars = new int[noOfVars];
  int *offsets = new int[noOfVars];
  for (int i=noOfVars; i--;) {
    DECLARE_TUPLE(t, v->Sub(i));
    DECLARE_INT(tmp1, t->Sel(0));
    offsets[i] = tmp1;
    DECLARE_INT(tmp2, t->Sel(1));
    vars[i] = tmp2;
  }
  s->tdistinct(offsets, vars, noOfVars, UnsafeGecode::int2cl[cl]);
  delete offsets;
  delete vars;
  RETURN_UNIT;
} END

DEFINE5(gc_linear) {
  DECLARE_SPACE(s, x0);
  DECLARE_VECTOR(v, x1);
  DECLARE_INT(rel, x2);
  DECLARE_INT(c, x3);
  DECLARE_INT(cl, x4);

  int noOfVars = v->GetLength();
  int *vars = new int[noOfVars];
  int *offsets = new int[noOfVars];
  for (int i=noOfVars; i--;) {
    DECLARE_TUPLE(t, v->Sub(i));
    DECLARE_INT(tmp1, t->Sel(0));
    offsets[i] = tmp1;
    DECLARE_INT(tmp2, t->Sel(1));
    vars[i] = tmp2;
  }
  s->tlinear(offsets, vars, noOfVars, UnsafeGecode::int2reltype[rel], c,
	     UnsafeGecode::int2cl[cl]);
} END

DEFINE6(gc_linearr) {
  DECLARE_SPACE(s, x0);
  DECLARE_VECTOR(v, x1);
  DECLARE_INT(rel, x2);
  DECLARE_INT(c, x3);
  DECLARE_INT(boolVar, x4);
  DECLARE_INT(cl, x5);

  int noOfVars = v->GetLength();
  int *vars = new int[noOfVars];
  int *offsets = new int[noOfVars];
  for (int i=noOfVars; i--;) {
    DECLARE_TUPLE(t, v->Sub(i));
    DECLARE_INT(tmp1, t->Sel(0));
    offsets[i] = tmp1;
    DECLARE_INT(tmp2, t->Sel(1));
    vars[i] = tmp2;
  }
  s->tlinearR(offsets, vars, noOfVars, UnsafeGecode::int2reltype[rel], c,
	      boolVar, UnsafeGecode::int2cl[cl]);
} END
    
DEFINE3(gc_bool_not) {
  DECLARE_SPACE(s, x0);
  DECLARE_INT(a, x1);
  DECLARE_INT(b, x2);
  s->tbool_not(a, b);
} END

DEFINE4(gc_bool_and) {
  DECLARE_SPACE(s, x0);
  DECLARE_INT(a, x1);
  DECLARE_INT(b, x2);
  DECLARE_INT(c, x3);
  s->tbool_and(a, b, c);
} END

DEFINE4(gc_bool_or) {
  DECLARE_SPACE(s, x0);
  DECLARE_INT(a, x1);
  DECLARE_INT(b, x2);
  DECLARE_INT(c, x3);
  s->tbool_or(a, b, c);
} END

DEFINE4(gc_bool_imp) {
  DECLARE_SPACE(s, x0);
  DECLARE_INT(a, x1);
  DECLARE_INT(b, x2);
  DECLARE_INT(c, x3);
  s->tbool_imp(a, b, c);
} END

DEFINE4(gc_bool_eq) {
  DECLARE_SPACE(s, x0);
  DECLARE_INT(a, x1);
  DECLARE_INT(b, x2);
  DECLARE_INT(c, x3);
  s->tbool_eq(a, b, c);
} END

DEFINE4(gc_bool_xor) {
  DECLARE_SPACE(s, x0);
  DECLARE_INT(a, x1);
  DECLARE_INT(b, x2);
  DECLARE_INT(c, x3);
  s->tbool_xor(a, b, c);
} END

DEFINE3(gc_bool_andv) {
  DECLARE_SPACE(s, x0);
  DECLARE_VECTOR(v, x1);
  DECLARE_INT(b, x2);

  int noOfVars = v->GetLength();
  int *vars = new int[noOfVars];

  for (int i=noOfVars; i--;) {
    DECLARE_INT(tmp1, v->Sub(i));
    vars[i] = tmp1;
  }
  s->tbool_and(vars, noOfVars, b);
  delete vars;
} END

DEFINE3(gc_bool_orv) {
  DECLARE_SPACE(s, x0);
  DECLARE_VECTOR(v, x1);
  DECLARE_INT(b, x2);

  int noOfVars = v->GetLength();
  int *vars = new int[noOfVars];

  for (int i=noOfVars; i--;) {
    DECLARE_INT(tmp1, v->Sub(i));
    vars[i] = tmp1;
  }
  s->tbool_or(vars, noOfVars, b);
  delete vars;
} END

DEFINE4(gc_branch) {
  DECLARE_SPACE(s, x0);
  DECLARE_VECTOR(v, x1);
  DECLARE_INT(varsel, x2);
  DECLARE_INT(valsel, x3);

  int noOfVars = v->GetLength();

  int *vars = new int[noOfVars];

  for (int i=noOfVars; i--;) {
    DECLARE_INT(tmp1, v->Sub(i));
    vars[i] = tmp1;
  }
  s->tbranch(vars, noOfVars, UnsafeGecode::int2bvarsel[varsel],
	     UnsafeGecode::int2bvalsel[valsel]);
  delete vars;
  RETURN_UNIT;
} END

DEFINE1(gc_getsearchengine) {
  DECLARE_SPACE(s, x0);

  GecodeSpace *c = static_cast<GecodeSpace *>(s->clone());

  Search<GecodeSpace> *se =
    new Search<GecodeSpace>(c);

  ConcreteRepresentation *cr =
    ConcreteRepresentation::New(UnsafeGecode::gecodeHandler,2);
  cr->Init(0, Store::UnmanagedPointerToWord(se));
  cr->Init(1, Store::IntToWord(1));
  UnsafeGecode::gecodeFinalizationSet->Register(cr->ToWord());
  RETURN(cr->ToWord());
} END

DEFINE1(gc_next) {
  DECLARE_ENGINE(se, x0);
  GecodeSpace *sol = se->next();
  if(sol) {
    ConcreteRepresentation *cr =
      ConcreteRepresentation::New(UnsafeGecode::gecodeHandler,2);
    cr->Init(0, Store::UnmanagedPointerToWord(sol));
    cr->Init(1, Store::IntToWord(0));
    UnsafeGecode::gecodeFinalizationSet->Register(cr->ToWord());

    TagVal *t = TagVal::New(1,1);
    t->Init(0, cr->ToWord());
    RETURN(t->ToWord());
  } else {
    RETURN_INT(0);
  }
} END

DEFINE1(gc_getsearchbestengine) {
  DECLARE_SPACE(s, x0);
  SearchBestExplCallback *se =
    new SearchBestExplCallback
    (s->clone());

  ConcreteRepresentation *cr =
    ConcreteRepresentation::New(UnsafeGecode::gecodeHandler,2);
  cr->Init(0, Store::UnmanagedPointerToWord(se));
  cr->Init(1, Store::IntToWord(2));
  UnsafeGecode::gecodeFinalizationSet->Register(cr->ToWord());
  RETURN(cr->ToWord());
} END

DEFINE1(gc_nextbest) {
  DECLARE_BESTENGINE(se, x0);
  Space *s1;
  Space *s2;

  switch(se->next(&s1, &s2)) {
  case SB_CONSTRAIN:
    {
      ConcreteRepresentation *cr1 =
	ConcreteRepresentation::New(UnsafeGecode::gecodeHandler,2);
      cr1->Init(0, Store::UnmanagedPointerToWord(s1));
      cr1->Init(1, Store::IntToWord(0));
      UnsafeGecode::gecodeFinalizationSet->Register(cr1->ToWord());

      ConcreteRepresentation *cr2 =
	ConcreteRepresentation::New(UnsafeGecode::gecodeHandler,2);
      cr2->Init(0, Store::UnmanagedPointerToWord(s2));
      cr2->Init(1, Store::IntToWord(0));
      UnsafeGecode::gecodeFinalizationSet->Register(cr2->ToWord());

      TagVal *t = TagVal::New(SB_CONSTRAIN,2);
      t->Init(0, cr1->ToWord());
      t->Init(1, cr2->ToWord());
      RETURN(t->ToWord());      
    }
    break;
  case SB_DONE:
    {
      RETURN_INT(SB_DONE);
    }
    break;
  case SB_SOLUTION:
    {
      ConcreteRepresentation *cr =
	ConcreteRepresentation::New(UnsafeGecode::gecodeHandler,2);
      cr->Init(0, Store::UnmanagedPointerToWord(s1));
      cr->Init(1, Store::IntToWord(0));
      UnsafeGecode::gecodeFinalizationSet->Register(cr->ToWord());

      TagVal *t = TagVal::New(SB_SOLUTION,1);
      t->Init(0, cr->ToWord());
      RETURN(t->ToWord());      
    }
    break;
  }
  RETURN_UNIT;
} END

DEFINE1(gc_status) {
  DECLARE_SPACE(s, x0);
  switch(s->status()) {
  case SS_BRANCH:
    RETURN_INT(0);
  case SS_FAILED:
    RETURN_INT(1);
    break;
  case SS_SOLVED:
    RETURN_INT(2);
  }
} END

DEFINE2(gc_commit) {
  DECLARE_SPACE(s, x0);
  DECLARE_INT(i, x1);
  s->commit(i);
  RETURN_UNIT;
} END

DEFINE1(gc_clone) {
  DECLARE_SPACE(s, x0);

  ConcreteRepresentation *cr =
    ConcreteRepresentation::New(UnsafeGecode::gecodeHandler,2);
  cr->Init(0, Store::UnmanagedPointerToWord(s->clone()));
  cr->Init(1, Store::IntToWord(0));
  UnsafeGecode::gecodeFinalizationSet->Register(cr->ToWord());
  RETURN(cr->ToWord());
} END

DEFINE1(gc_discard) {
  GecodeSpace *s;
  if (Store::WordToTransient(x0) != INVALID_POINTER) { REQUEST(x0); }
  ConcreteRepresentation *cr = ConcreteRepresentation::FromWordDirect(x0);
  s = (GecodeSpace *) Store::WordToUnmanagedPointer(cr->Get(0));
  
  delete s;
  cr->Replace(0, Store::IntToWord(0));
  RETURN_UNIT;
} END

DEFINE1(gc_alive) {
  if (Store::WordToTransient(x0) != INVALID_POINTER) { REQUEST(x0); }
  ConcreteRepresentation *cr = ConcreteRepresentation::FromWordDirect(x0);
  
  RETURN_BOOL(Store::WordToInt(cr->Get(0))!=0);
} END

word InitComponent() {
  UnsafeGecode::gecodeFinalizationSet = new UnsafeGecode::GecodeFinalizationSet();
  UnsafeGecode::gecodeHandler = new UnsafeGecode::GecodeHandler();

  Record *record = Record::New(37);

  INIT_STRUCTURE(record, "UnsafeGecode", "makeSpace",
		 gc_makespace, 0);
  INIT_STRUCTURE(record, "UnsafeGecode", "intvar",
		 gc_intvar, 3);
  INIT_STRUCTURE(record, "UnsafeGecode", "intvarR",
		 gc_intvarr, 4);
  INIT_STRUCTURE(record, "UnsafeGecode", "boolvar",
		 gc_boolvar, 1);
  INIT_STRUCTURE(record, "UnsafeGecode", "getMin",
		 gc_getmin, 2);
  INIT_STRUCTURE(record, "UnsafeGecode", "getMax",
		 gc_getmax, 2);
  INIT_STRUCTURE(record, "UnsafeGecode", "rel",
		 gc_rel, 4);
  INIT_STRUCTURE(record, "UnsafeGecode", "relI",
		 gc_reli, 4);
  INIT_STRUCTURE(record, "UnsafeGecode", "relR",
		 gc_relr, 5);
  INIT_STRUCTURE(record, "UnsafeGecode", "relIR",
		 gc_relir, 5);
  INIT_STRUCTURE(record, "UnsafeGecode", "eq",
		 gc_eq, 4);
  INIT_STRUCTURE(record, "UnsafeGecode", "eqV",
		 gc_eqv, 3);
  INIT_STRUCTURE(record, "UnsafeGecode", "eqR",
		 gc_eqr, 5);
  INIT_STRUCTURE(record, "UnsafeGecode", "eqVR",
		 gc_eqvr, 4);
  INIT_STRUCTURE(record, "UnsafeGecode", "distinct",
		 gc_distinct, 3);
  INIT_STRUCTURE(record, "UnsafeGecode", "distinctI",
		 gc_distincti, 3);
  INIT_STRUCTURE(record, "UnsafeGecode", "linear",
		 gc_linear, 5);  
  INIT_STRUCTURE(record, "UnsafeGecode", "linearR",
		 gc_linearr, 6);  

  INIT_STRUCTURE(record, "UnsafeGecode", "bool_not",
		 gc_bool_not, 3);
  INIT_STRUCTURE(record, "UnsafeGecode", "bool_and",
		 gc_bool_and, 4);
  INIT_STRUCTURE(record, "UnsafeGecode", "bool_or",
		 gc_bool_or, 4);
  INIT_STRUCTURE(record, "UnsafeGecode", "bool_imp",
		 gc_bool_imp, 4);
  INIT_STRUCTURE(record, "UnsafeGecode", "bool_eq",
		 gc_bool_eq, 4);
  INIT_STRUCTURE(record, "UnsafeGecode", "bool_xor",
		 gc_bool_xor, 4);
  INIT_STRUCTURE(record, "UnsafeGecode", "bool_andV",
		 gc_bool_andv, 3);
  INIT_STRUCTURE(record, "UnsafeGecode", "bool_orV",
		 gc_bool_orv, 3);

  INIT_STRUCTURE(record, "UnsafeGecode", "branch",
		 gc_branch, 4);
  INIT_STRUCTURE(record, "UnsafeGecode", "getSearchEngine",
		 gc_getsearchengine, 1);
  INIT_STRUCTURE(record, "UnsafeGecode", "next",
		 gc_next, 1);
  INIT_STRUCTURE(record, "UnsafeGecode", "getSearchBestEngine",
		 gc_getsearchbestengine, 1);
  INIT_STRUCTURE(record, "UnsafeGecode", "nextBest",
		 gc_nextbest, 1);

  INIT_STRUCTURE(record, "UnsafeGecode", "status",
		 gc_status, 1);
  INIT_STRUCTURE(record, "UnsafeGecode", "commit",
		 gc_commit, 2);
  INIT_STRUCTURE(record, "UnsafeGecode", "clone",
		 gc_clone, 1);
  INIT_STRUCTURE(record, "UnsafeGecode", "discard",
		 gc_discard, 1);
  INIT_STRUCTURE(record, "UnsafeGecode", "alive",
		 gc_alive, 1);

  RETURN_STRUCTURE("UnsafeGecode$", record);
}
