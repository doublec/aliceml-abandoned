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
#include "gecode-int.hh"

// STAMPS
// 
// Spaces are stamped uniquely. This makes it possible to know in which space
// a variable was introduced, and in which spaces it may be used:
//
// A space created by gc_makespace gets the stamps (-1, <newStamp>)
// As soon as this space is cloned, the -1 is replaced by a <newStamp>
// If a space s with stamps (st1, st2) is cloned, the clone will get the
// stamps (<newStamp>, st2)
//
// Variables that are created in a space with stamps (st1, st2) get st1, unless
// st1==-1, in which case they get st2 as their stamp.
//
// Interpretation:
// A variable may be used 
//   * in the space where it was created
//   * or if it was created in a root space
//     (one obtained through gc_makespace), in any space
//     obtained by cloning that root space
// When a root space is cloned, it looses its root character,
// and every variable that is created in the original space after
// cloning will not be considered a root variable.

#define DBGMSG(m)

//#define DBGMSG(m) { fprintf(stderr,"---> "); \
// fprintf(stderr, m); fprintf(stderr, "\n"); }


#define RETURN_IPAIR(i,j)        \
{Tuple *t = Tuple::New(2);       \
 t->Init(0,Store::IntToWord(i)); \
 t->Init(1,Store::IntToWord(j)); \
 RETURN(t->ToWord()); }

#define RETURN_GECODE_VAR(i,j)   \
{Tuple *t = Tuple::New(3);       \
 t->Init(0,Store::IntToWord(i)); \
 t->Init(1,Store::IntToWord(j)); \
 t->Init(2,UnsafeGecode::SitedMarker);   \
 RETURN(t->ToWord()); }

#define DECLARE_SPACE(s, stamp, pstamp, x)                                  \
  GecodeSpace *s;                                                           \
  int stamp, pstamp;                                                        \
  if (Store::WordToTransient(x) != INVALID_POINTER) { REQUEST(x); }         \
  { ConcreteRepresentation *cr = ConcreteRepresentation::FromWordDirect(x); \
    s = (GecodeSpace *) Store::WordToUnmanagedPointer(cr->Get(0));          \
    stamp = Store::DirectWordToInt(cr->Get(1));                             \
    pstamp = Store::DirectWordToInt(cr->Get(2));                            \
  } \
  DBGMSG("DECLARE SPACE done");

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


#define CHECK_SPACE(s) if (!s) RAISE(UnsafeGecode::InvalidSpaceConstructor);

#define DECLARE_VAR(v, stamp, pstamp, x)                \
 u_int v;                                               \
 {                                                      \
   DECLARE_TUPLE(varIntern, x);                         \
   s_int myStamp = Store::DirectWordToInt(varIntern->Sel(1)); \
   if (myStamp != stamp && myStamp != pstamp)           \
     RAISE(UnsafeGecode::InvalidVarConstructor);                      \
   v = Store::DirectWordToInt(varIntern->Sel(0));             \
 } \
 DBGMSG("DECLARE_VAR done.");


namespace UnsafeGecode {

  static word SitedMarker;
  static word InvalidVarConstructor;
  static word InvalidSpaceConstructor;
  static s_int SpaceStamp = 0;
  
  class VectorValIterator {
  private:
    Vector *v;
    u_int pos;
    u_int size;
  public:
    VectorValIterator(Vector *vv) : v(vv), pos(0), size(v->GetLength()) {}

    void operator++(void);
    bool operator()(void) const;
    int val(void) const;
  };

  void VectorValIterator::operator++(void) {
    pos++;
  }
  
  bool VectorValIterator::operator()(void) const {
    return pos<size; 
  }

  int VectorValIterator::val(void) const {
    return Store::DirectWordToInt(v->Sub(pos));
  }

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
    CL_BND, CL_DEF, CL_DOM,
    CL_VAL
  };

const AvalSel int2avalsel[] =
  {
    AVAL_MAX, AVAL_MED, AVAL_MIN
  };

const SetBvarSel int2fsbvarsel[] =
  {
    SETBVAR_MAX_CARD,
    SETBVAR_MIN_CARD,
    SETBVAR_MIN_UNKNOWN_ELEM,
    SETBVAR_NONE
  };

const SetBvalSel int2fsbvalsel[] =
  {
    SETBVAL_MAX,
    SETBVAL_MIN
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
  GecodeSpace *s = (GecodeSpace *)Store::WordToUnmanagedPointer(ptr);
  delete s;
}

}

DEFINE0(gc_makespace) {
  DBGMSG("makespace");
  GecodeSpace *s = new GecodeSpace();

  ConcreteRepresentation *cr =
    ConcreteRepresentation::New(UnsafeGecode::gecodeHandler,3);
  cr->Init(0, Store::UnmanagedPointerToWord(s));
  cr->Init(1, Store::IntToWord(-1));
  cr->Init(2, Store::IntToWord(UnsafeGecode::SpaceStamp++));
  UnsafeGecode::gecodeFinalizationSet->Register(cr->ToWord());
  DBGMSG("done");
  RETURN(cr->ToWord());
} END

DEFINE2(gc_fdvar) {
  DBGMSG("gc_fdvar");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  
  if (stamp==-1) stamp=pstamp;

  DECLARE_VECTOR(v, x1);

  int noOfPairs = v->GetLength();
  int pairs[noOfPairs][2];

  for (int i=noOfPairs; i--;) {
    DECLARE_TUPLE(tmp, v->Sub(i));
    DECLARE_INT(tmp0, tmp->Sel(0));
    DECLARE_INT(tmp1, tmp->Sel(1));
    pairs[i][0] = tmp0;
    pairs[i][1] = tmp1;
  }
  DomSpec ds(pairs, noOfPairs);
  int newVar = s->AddIntVariable(ds);
  DBGMSG("done");
  RETURN_GECODE_VAR(newVar, stamp);
} END

DEFINE3(gc_fdvarr) {
  DBGMSG("fdvarr");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  
  if (stamp==-1) stamp=pstamp;

  DECLARE_VECTOR(v, x1);
  DECLARE_VAR(boolVar, stamp, pstamp, x2);

  int noOfPairs = v->GetLength();
  int pairs[noOfPairs][2];

  for (int i=noOfPairs; i--;) {
    DECLARE_TUPLE(tmp, v->Sub(i));
    DECLARE_INT(tmp0, tmp->Sel(0));
    DECLARE_INT(tmp1, tmp->Sel(1));
    pairs[i][0] = tmp0;
    pairs[i][1] = tmp1;
  }
  
  DomSpec ds(pairs, noOfPairs);
  int newVar = s->AddIntVariableR(ds, boolVar);
  DBGMSG("done");
  RETURN_GECODE_VAR(newVar, stamp);
} END

DEFINE1(gc_boolvar) {
  DBGMSG("boolvar");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  if (stamp==-1) stamp=pstamp;

  int newVar = s->AddBoolVariable();
  DBGMSG("done");
  RETURN_GECODE_VAR(newVar, stamp);
} END

DEFINE2(gc_getmin) {
  DBGMSG("getmin");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var, stamp, pstamp, x1);
  DBGMSG("done");
  RETURN_INT(s->vmin(var));
} END

DEFINE2(gc_getmax) {
  DBGMSG("getmax");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var, stamp, pstamp, x1);
  DBGMSG("done");
  RETURN_INT(s->vmax(var));
} END

DEFINE2(gc_getdom) {
  DBGMSG("getdom");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var, stamp, pstamp, x1);
  DBGMSG("done");

  VarRanges<IntVar> r1 = s->vranges(var);
  int size=0;
  for (; r1(); ++r1, size++);

  DBGMSG("ranges");

  VarRanges<IntVar> r2 = s->vranges(var);

  Vector *v = Vector::New(size);
  for(int count=0; r2(); ++r2, count++) {
    Tuple *t = Tuple::New(2);
    t->Init(0, Store::IntToWord(r2.min()));
    t->Init(1, Store::IntToWord(r2.max()));
    v->Init(count, t->ToWord());
  }
  DBGMSG("return");
  RETURN(v->ToWord());
} END

DEFINE3(gc_dom) {
  DBGMSG("dom");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VAR(i, stamp, pstamp, x1);
  DECLARE_VECTOR(v, x2);

  int noOfPairs = v->GetLength();
  int pairs[noOfPairs][2];

  for (int j=noOfPairs; j--;) {
    DECLARE_TUPLE(tmp, v->Sub(i));
    DECLARE_INT(tmp0, tmp->Sel(0));
    DECLARE_INT(tmp1, tmp->Sel(1));
    pairs[j][0] = tmp0;
    pairs[j][1] = tmp1;
  }

  DomSpec ds(pairs, noOfPairs);
  s->dom(i, ds);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE4(gc_domr) {
  DBGMSG("domr");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VAR(i, stamp, pstamp, x1);
  DECLARE_VECTOR(v, x2);
  DECLARE_VAR(boolvar, stamp, pstamp, x3);

  int noOfPairs = v->GetLength();
  int pairs[noOfPairs][2];

  for (int i=noOfPairs; i--;) {
    DECLARE_TUPLE(tmp, v->Sub(i));
    DECLARE_INT(tmp0, tmp->Sel(0));
    DECLARE_INT(tmp1, tmp->Sel(1));
    pairs[i][0] = tmp0;
    pairs[i][1] = tmp1;
  }

  DomSpec ds(pairs, noOfPairs);
  s->domR(i, ds, boolvar);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE4(gc_rel) {
  DBGMSG("rel");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VAR(i, stamp, pstamp, x1);
  DECLARE_INT(rel, x2);
  DECLARE_VAR(j, stamp, pstamp, x3);
  s->rel(i, UnsafeGecode::int2reltype[rel], j);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE4(gc_reli) {
  DBGMSG("reli");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VAR(i, stamp, pstamp, x1);
  DECLARE_INT(rel, x2);
  DECLARE_INT(j, x3);
  s->relI(i, UnsafeGecode::int2reltype[rel], j);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE5(gc_relr) {
  DBGMSG("relr");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VAR(i, stamp, pstamp, x1);
  DECLARE_INT(rel, x2);
  DECLARE_VAR(j, stamp, pstamp, x3);
  DECLARE_VAR(boolVar, stamp, pstamp, x4);
  s->relR(i, UnsafeGecode::int2reltype[rel], j, boolVar);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE5(gc_relir) {
  DBGMSG("relir");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VAR(i, stamp, pstamp, x1);
  DECLARE_INT(rel, x2);
  DECLARE_INT(j, x3);
  DECLARE_VAR(boolVar, stamp, pstamp, x4);
  s->relIR(i, UnsafeGecode::int2reltype[rel], j, boolVar);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE4(gc_eq) {
  DBGMSG("eq");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VAR(i, stamp, pstamp, x1);
  DECLARE_VAR(j, stamp, pstamp, x2);
  DECLARE_INT(cl, x3);
  s->eq(i, j, UnsafeGecode::int2cl[cl]);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE4(gc_eqv) {
  DBGMSG("eqv");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VECTOR(v, x1);
  DECLARE_INT(cl, x2);
  int noOfVars = v->GetLength();

  if (noOfVars==0) RETURN_UNIT;

  IntArgs vars(noOfVars);
  for (int i=noOfVars; i--;) {
    DECLARE_VAR(tmp, stamp, pstamp, v->Sub(i));
    vars[i] = tmp;
  }
  s->eqV(vars, UnsafeGecode::int2cl[cl]);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE5(gc_eqr) {
  DBGMSG("eqr");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VAR(i, stamp, pstamp, x1);
  DECLARE_VAR(j, stamp, pstamp, x2);
  DECLARE_INT(cl, x3);
  DECLARE_VAR(boolVar, stamp, pstamp, x4);
  s->eqR(i, j, boolVar, UnsafeGecode::int2cl[cl]);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE3(gc_distinct) {
  DBGMSG("distinct");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VECTOR(v, x1);
  DECLARE_INT(cl, x2);

  int noOfVars = v->GetLength();

  if (noOfVars==0) RETURN_UNIT;

  IntArgs vars(noOfVars);
  for (int i=noOfVars; i--;) {
    DECLARE_VAR(tmp, stamp, pstamp, v->Sub(i));
    vars[i] = tmp;
  }
  s->distinct(vars, UnsafeGecode::int2cl[cl]);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE3(gc_distincti) {
  DBGMSG("distincti");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VECTOR(v, x1);
  DECLARE_INT(cl, x2);

  int noOfVars = v->GetLength();

  if (noOfVars==0) RETURN_UNIT;

  IntArgs vars(noOfVars);
  IntArgs offsets(noOfVars);

  for (int i=noOfVars; i--;) {
    DECLARE_TUPLE(t, v->Sub(i));
    DECLARE_INT(tmp1, t->Sel(0));
    offsets[i] = tmp1;
    DECLARE_VAR(tmp2, stamp, pstamp, t->Sel(1));
    vars[i] = tmp2;
  }
  s->distinctI(offsets, vars, UnsafeGecode::int2cl[cl]);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE5(gc_linear) {
  DBGMSG("linear");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VECTOR(v, x1);
  DECLARE_INT(rel, x2);
  DECLARE_INT(c, x3);
  DECLARE_INT(cl, x4);

  int noOfVars = v->GetLength();

  if (noOfVars==0) RETURN_UNIT;

  IntArgs vars(noOfVars);
  IntArgs offsets(noOfVars);
  for (int i=noOfVars; i--;) {
    DECLARE_TUPLE(t, v->Sub(i));
    DECLARE_INT(tmp1, t->Sel(0));
    offsets[i] = tmp1;
    DECLARE_VAR(tmp2, stamp, pstamp, t->Sel(1));
    vars[i] = tmp2;
  }
  s->linear(offsets, vars, UnsafeGecode::int2reltype[rel], c,
	    UnsafeGecode::int2cl[cl]);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE6(gc_linearr) {
  DBGMSG("linearr");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VECTOR(v, x1);
  DECLARE_INT(rel, x2);
  DECLARE_INT(c, x3);
  DECLARE_VAR(boolVar, stamp, pstamp, x4);
  DECLARE_INT(cl, x5);

  int noOfVars = v->GetLength();

  if (noOfVars==0) RETURN_UNIT; /** post boolvar=true? **/

  IntArgs vars(noOfVars);
  IntArgs offsets(noOfVars);
  for (int i=noOfVars; i--;) {
    DECLARE_TUPLE(t, v->Sub(i));
    DECLARE_INT(tmp1, t->Sel(0));
    offsets[i] = tmp1;
    DECLARE_VAR(tmp2, stamp, pstamp, t->Sel(1));
    vars[i] = tmp2;
  }
  s->linearR(offsets, vars, UnsafeGecode::int2reltype[rel], c,
	     boolVar, UnsafeGecode::int2cl[cl]);
  DBGMSG("done");
  RETURN_UNIT;
} END
    
DEFINE3(gc_bool_not) {
  DBGMSG("bool_not");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VAR(a, stamp, pstamp, x1);
  DECLARE_VAR(b, stamp, pstamp, x2);
  s->bool_not(a, b);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE4(gc_bool_and) {
  DBGMSG("bool_and");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VAR(a, stamp, pstamp, x1);
  DECLARE_VAR(b, stamp, pstamp, x2);
  DECLARE_VAR(c, stamp, pstamp, x3);
  s->bool_and(a, b, c);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE4(gc_bool_or) {
  DBGMSG("bool_or");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VAR(a, stamp, pstamp, x1);
  DECLARE_VAR(b, stamp, pstamp, x2);
  DECLARE_VAR(c, stamp, pstamp, x3);
  s->bool_or(a, b, c);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE4(gc_bool_imp) {
  DBGMSG("bool_imp");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VAR(a, stamp, pstamp, x1);
  DECLARE_VAR(b, stamp, pstamp, x2);
  DECLARE_VAR(c, stamp, pstamp, x3);
  s->bool_imp(a, b, c);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE4(gc_bool_eq) {
  DBGMSG("bool_eq");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VAR(a, stamp, pstamp, x1);
  DECLARE_VAR(b, stamp, pstamp, x2);
  DECLARE_VAR(c, stamp, pstamp, x3);
  s->bool_eq(a, b, c);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE4(gc_bool_xor) {
  DBGMSG("bool_xor");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VAR(a, stamp, pstamp, x1);
  DECLARE_VAR(b, stamp, pstamp, x2);
  DECLARE_VAR(c, stamp, pstamp, x3);
  s->bool_xor(a, b, c);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE3(gc_bool_andv) {
  DBGMSG("bool_andv");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VECTOR(v, x1);
  DECLARE_VAR(b, stamp, pstamp, x2);

  int noOfVars = v->GetLength();
  if (noOfVars==0) RETURN_UNIT;

  IntArgs vars(noOfVars);

  for (int i=noOfVars; i--;) {
    DECLARE_VAR(tmp1, stamp, pstamp, v->Sub(i));
    vars[i] = tmp1;
  }
  s->bool_andV(vars, b);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE3(gc_bool_orv) {
  DBGMSG("bool_orv");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VECTOR(v, x1);
  DECLARE_VAR(b, stamp, pstamp, x2);

  int noOfVars = v->GetLength();
  if (noOfVars==0) RETURN_UNIT;

  IntArgs vars(noOfVars);
  for (int i=noOfVars; i--;) {
    DECLARE_VAR(tmp1, stamp, pstamp, v->Sub(i));
    vars[i] = tmp1;
  }
  s->bool_orV(vars, b);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE4(gc_branch) {
  DBGMSG("branch");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VECTOR(v, x1);
  DECLARE_INT(varsel, x2);
  DECLARE_INT(valsel, x3);

  int noOfVars = v->GetLength();
  if (noOfVars==0) RETURN_UNIT;

  IntArgs vars(noOfVars);

  for (int i=noOfVars; i--;) {
    DECLARE_VAR(tmp1, stamp, pstamp, v->Sub(i));
    vars[i] = tmp1;
  }
  s->branch(vars, UnsafeGecode::int2bvarsel[varsel],
	    UnsafeGecode::int2bvalsel[valsel]);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE1(gc_status) {
  DBGMSG("status");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  switch(s->status()) {
  case SS_BRANCH:
    DBGMSG("done");
    RETURN_INT(0);
  case SS_FAILED:
    DBGMSG("done");
    RETURN_INT(1);
    break;
  case SS_SOLVED:
    DBGMSG("done");
    RETURN_INT(2);
  }
  DBGMSG("strange error!!!");

} END

DEFINE2(gc_commit) {
  DBGMSG("commit");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_INT(i, x1);
  DBGMSG("commit");
  s->commit(i);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE1(gc_clone) {
  DBGMSG("clone");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  if (stamp==-1) {
    ConcreteRepresentation *cr = ConcreteRepresentation::FromWordDirect(x0);
    cr->Replace(1, Store::IntToWord(UnsafeGecode::SpaceStamp++));
  }

  ConcreteRepresentation *cr =
    ConcreteRepresentation::New(UnsafeGecode::gecodeHandler,3);

  Space *newSpace = s->clone();

  CHECK_SPACE(newSpace);
  DBGMSG("clone successful");

  cr->Init(0, Store::UnmanagedPointerToWord(newSpace));
  cr->Init(1, Store::IntToWord(UnsafeGecode::SpaceStamp++));
  cr->Init(2, Store::IntToWord(pstamp));
  UnsafeGecode::gecodeFinalizationSet->Register(cr->ToWord());


  DBGMSG("done");
  RETURN(cr->ToWord());
} END

DEFINE1(gc_discard) {
  DBGMSG("discard");
  GecodeSpace *s;
  if (Store::WordToTransient(x0) != INVALID_POINTER) { REQUEST(x0); }
  ConcreteRepresentation *cr = ConcreteRepresentation::FromWordDirect(x0);
  s = (GecodeSpace *) Store::WordToUnmanagedPointer(cr->Get(0));
  
  delete s;
  cr->Replace(0, Store::IntToWord(0));
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE1(gc_alive) {
  DBGMSG("alive");
  if (Store::WordToTransient(x0) != INVALID_POINTER) { REQUEST(x0); }
  ConcreteRepresentation *cr = ConcreteRepresentation::FromWordDirect(x0);
  
  DBGMSG("done");
  RETURN_BOOL(Store::WordToInt(cr->Get(0))!=0);
} END

DEFINE6(gc_countii) {
  DBGMSG("countii");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VECTOR(v, x1);
  DECLARE_INT(rel, x2);
  DECLARE_INT(i, x3);
  DECLARE_INT(rel2, x4);
  DECLARE_INT(j, x5);

  int noOfVars = v->GetLength();
  if (noOfVars==0) RETURN_UNIT;

  IntArgs vars(noOfVars);
  for (int i=noOfVars; i--;) {
    DECLARE_VAR(tmp, stamp, pstamp, v->Sub(i));
    vars[i] = tmp;
  }

  s->countII(vars, UnsafeGecode::int2reltype[rel],
	     i, UnsafeGecode::int2reltype[rel2], j);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE6(gc_countvi) {
  DBGMSG("countvi");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VECTOR(v, x1);
  DECLARE_INT(rel, x2);
  DECLARE_VAR(i, stamp, pstamp, x3);
  DECLARE_INT(rel2, x4);
  DECLARE_INT(j, x5);

  int noOfVars = v->GetLength();
  if (noOfVars==0) RETURN_UNIT; /** is this correct? **/

  IntArgs vars(noOfVars);
  for (int i=noOfVars; i--;) {
    DECLARE_VAR(tmp, stamp, pstamp, v->Sub(i));
    vars[i] = tmp;
  }

  s->countVI(vars, UnsafeGecode::int2reltype[rel],
	      i, UnsafeGecode::int2reltype[rel2], j);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE6(gc_countiv) {
  DBGMSG("countiv");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VECTOR(v, x1);
  DECLARE_INT(rel, x2);
  DECLARE_INT(i, x3);
  DECLARE_INT(rel2, x4);
  DECLARE_VAR(j, stamp, pstamp, x5);

  int noOfVars = v->GetLength();
  if (noOfVars==0) RETURN_UNIT; /** is this correct? **/

  IntArgs vars(noOfVars);
  for (int i=noOfVars; i--;) {
    DECLARE_VAR(tmp, stamp, pstamp, v->Sub(i));
    vars[i] = tmp;
  }

  s->countIV(vars, UnsafeGecode::int2reltype[rel],
	      i, UnsafeGecode::int2reltype[rel2], j);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE6(gc_countvv) {
  DBGMSG("countvv");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VECTOR(v, x1);
  DECLARE_INT(rel, x2);
  DECLARE_VAR(i, stamp, pstamp, x3);
  DECLARE_INT(rel2, x4);
  DECLARE_VAR(j, stamp, pstamp, x5);

  int noOfVars = v->GetLength();
  if (noOfVars==0) RETURN_UNIT; /** is this correct? **/

  IntArgs vars(noOfVars);
  for (int i=noOfVars; i--;) {
    DECLARE_VAR(tmp, stamp, pstamp, v->Sub(i));
    vars[i] = tmp;
  }

  s->countVV(vars, UnsafeGecode::int2reltype[rel],
	      i, UnsafeGecode::int2reltype[rel2], j);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE4(gc_element) {
  DBGMSG("element");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VECTOR(v, x1);
  DECLARE_VAR(i, stamp, pstamp, x2);
  DECLARE_VAR(j, stamp, pstamp, x3);

  int noOfVars = v->GetLength();
  if (noOfVars==0) RETURN_UNIT; /** is this correct? **/

  IntArgs vars(noOfVars);
  for (int i=noOfVars; i--;) {
    DECLARE_VAR(tmp, stamp, pstamp, v->Sub(i));
    vars[i] = tmp;
  }

  s->element(vars, i, j);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE4(gc_elementi) {
  DBGMSG("elementi");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VECTOR(v, x1);
  DECLARE_VAR(i, stamp, pstamp, x2);
  DECLARE_VAR(j, stamp, pstamp, x3);

  int noOfArgs = v->GetLength();
  if (noOfArgs==0) RETURN_UNIT; /** is this correct? **/

  IntArgs args(noOfArgs);
  for (int i=noOfArgs; i--;) {
    DECLARE_INT(tmp, v->Sub(i));
    args[i] = tmp;
  }

  s->elementI(args, i, j);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE4(gc_lex) {
  DBGMSG("lex");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VECTOR(v1, x1);
  DECLARE_INT(rel, x2);
  DECLARE_VECTOR(v2, x3);

  int noOfVars1 = v1->GetLength();
  if (noOfVars1==0) RETURN_UNIT; /** is this correct? **/

  IntArgs vars1(noOfVars1);
  for (int i=noOfVars1; i--;) {
    DECLARE_VAR(tmp, stamp, pstamp, v1->Sub(i));
    vars1[i] = tmp;
  }

  int noOfVars2 = v2->GetLength();
  if (noOfVars2==0) RETURN_UNIT; /** is this correct? **/

  IntArgs vars2(noOfVars2);
  for (int i=noOfVars2; i--;) {
    DECLARE_VAR(tmp, stamp, pstamp, v2->Sub(i));
    vars2[i] = tmp;
  }

  s->lex(vars1, UnsafeGecode::int2reltype[rel], vars2);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE3(gc_min) {
  DBGMSG("min");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VECTOR(v, x1);
  DECLARE_VAR(i, stamp, pstamp, x2);
  int noOfVars = v->GetLength();
  if (noOfVars==0) RETURN_UNIT; /** is this correct? **/

  IntArgs vars(noOfVars);
  for (int i=noOfVars; i--;) {
    DECLARE_VAR(tmp, stamp, pstamp, v->Sub(i));
    vars[i] = tmp;
  }

  s->min(vars, i);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE3(gc_max) {
  DBGMSG("max");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VECTOR(v, x1);
  DECLARE_VAR(i, stamp, pstamp, x2);
  int noOfVars = v->GetLength();
  if (noOfVars==0) RETURN_UNIT; /** is this correct? **/

  IntArgs vars(noOfVars);
  for (int i=noOfVars; i--;) {
    DECLARE_VAR(tmp, stamp, pstamp, v->Sub(i));
    vars[i] = tmp;
  }

  s->max(vars, i);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE4(gc_abs) {
  DBGMSG("abs");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VAR(i, stamp, pstamp, x1);
  DECLARE_VAR(j, stamp, pstamp, x2);
  DECLARE_INT(cl, x3);

  s->abs(i, j, UnsafeGecode::int2cl[cl]);

  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE4(gc_mult) {
  DBGMSG("mult");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VAR(i, stamp, pstamp, x1);
  DECLARE_VAR(j, stamp, pstamp, x2);
  DECLARE_VAR(k, stamp, pstamp, x3);

  s->mult(i, j, k);

  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE4(gc_power) {
  DBGMSG("power");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VAR(i, stamp, pstamp, x1);
  DECLARE_VAR(j, stamp, pstamp, x2);
  DECLARE_VAR(k, stamp, pstamp, x3);

  s->power(i, j, k);

  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE3(gc_assign) {
  DBGMSG("assign");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VECTOR(v, x1);
  DECLARE_INT(avalsel, x2);

  int noOfVars = v->GetLength();
  if (noOfVars==0) RETURN_UNIT; /** is this correct? **/

  IntArgs vars(noOfVars);
  for (int i=noOfVars; i--;) {
    DECLARE_VAR(tmp, stamp, pstamp, v->Sub(i));
    vars[i] = tmp;
  }

  s->assign(vars, UnsafeGecode::int2avalsel[avalsel]);
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE1(gc_fail) {
  DBGMSG("fail");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  s->fail();
  DBGMSG("done");
  RETURN_UNIT;
} END


DEFINE1(gc_fsvar) {
  DBGMSG("gc_fsvar");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  
  if (stamp==-1) stamp=pstamp;

  int newVar = s->AddSetVariable();
  DBGMSG("done");
  RETURN_GECODE_VAR(newVar,stamp);
} END

DEFINE2(gc_fsUB) {
  DBGMSG("fsUB");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var, stamp, pstamp, x1);
  DBGMSG("done");

  unsigned int ubsize = 0;
  for(UBIter<SetVar> ubs = s->fs_upperBound(var);
      ubs(); ++ubs) ubsize++;

  UBIter<SetVar> ub = s->fs_upperBound(var);
  Vector *v = 
    Vector::New(ubsize);
  if(ubsize>0) {
    u_int count = 0;
    for (; ub(); ++ub) {
      Tuple *t = Tuple::New(2);
      t->Init(0, Store::IntToWord(ub.min()));
      t->Init(1, Store::IntToWord(ub.max()));
      v->Init(count, t->ToWord());
      count++;
    }
  }

  RETURN(v->ToWord());
} END

DEFINE2(gc_fsLB) {
  DBGMSG("fsLB");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var, stamp, pstamp, x1);
  DBGMSG("done");

  unsigned int lbsize = 0;
  for(LBIter<SetVar> lbs = s->fs_lowerBound(var);
      lbs(); ++lbs) lbsize++;

  LBIter<SetVar> lb = s->fs_lowerBound(var);
  Vector *v = 
    Vector::New(lbsize);

  if(lbsize>0) {
    u_int count = 0;
    for (; lb(); ++lb) {
      Tuple *t = Tuple::New(2);
      t->Init(0, Store::IntToWord(lb.min()));
      t->Init(1, Store::IntToWord(lb.max()));
      v->Init(count, t->ToWord());
      count++;
    }
  }

  RETURN(v->ToWord());
} END

DEFINE2(gc_fsUnknown) {
  DBGMSG("fsLB");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var, stamp, pstamp, x1);
  DBGMSG("done");

  RangesMinus<UBIter<SetVar>, LBIter<SetVar> > unknown =
    s->fs_unknown(var);
  RangesCache<RangesMinus<UBIter<SetVar>, LBIter<SetVar> > >
    unknownC(unknown);

  unsigned int usize = 0;
  for(;unknownC(); ++unknownC) usize++;

  Vector *v = 
    Vector::New(usize);
  unknownC.reset();

  if(usize>0) {
    u_int count = 0;
    for (; unknownC(); ++unknownC) {
      Tuple *t = Tuple::New(2);
      t->Init(0, Store::IntToWord(unknownC.min()));
      t->Init(1, Store::IntToWord(unknownC.max()));
      v->Init(count, t->ToWord());
      count++;
    }
  }

  RETURN(v->ToWord());
} END

DEFINE2(gc_fsGetCard) {
  DBGMSG("fsGetCard");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var, stamp, pstamp, x1);
  DBGMSG("done");
  RETURN_IPAIR(s->fs_cardinalityMin(var),
	       s->fs_cardinalityMax(var));
} END

DEFINE2(gc_fsGetCardLB) {
  DBGMSG("fsGetCardLB");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var, stamp, pstamp, x1);
  DBGMSG("done");

  RETURN_INT(s->fs_lowerBoundSize(var));
} END

DEFINE2(gc_fsGetCardUB) {
  DBGMSG("fsGetCardUB");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var, stamp, pstamp, x1);
  DBGMSG("done");

  RETURN_INT(s->fs_upperBoundSize(var));
} END

DEFINE2(gc_fsGetCardUnknown) {
  DBGMSG("fsGetCardUnknown");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var, stamp, pstamp, x1);
  DBGMSG("done");

  RangesMinus<UBIter<SetVar>, LBIter<SetVar> > unknown =
    s->fs_unknown(var);

  RETURN_INT(iteratorSize(unknown));
} END

DEFINE3(gc_fsLowerBound) {
  DBGMSG("fsLowerBound");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);

  DECLARE_VECTOR(v, x2);
  int setSize = v->GetLength();

  // Request all futures so that we can iterate over the vector
  for (int i=setSize; i--;) {
    DECLARE_INT(tmp, v->Sub(i));
  }

  UnsafeGecode::VectorValIterator is(v);
  ValuesToRanges<UnsafeGecode::VectorValIterator> is2(is);
  s->fs_lowerBound(var1, is2);
  
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE3(gc_fsUpperBound) {
  DBGMSG("fsUpperBound");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);

  DECLARE_VECTOR(v, x2);
  int setSize = v->GetLength();

  // Request all futures so that we can iterate over the vector
  for (int i=setSize; i--;) {
    DECLARE_INT(tmp, v->Sub(i));
  }

  UnsafeGecode::VectorValIterator is(v);
  ValuesToRanges<UnsafeGecode::VectorValIterator> is2(is);
  s->fs_upperBound(var1, is2);

  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE3(gc_fsInclude) {
  DBGMSG("fsInclude");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);
  DECLARE_VAR(var2, stamp, pstamp, x2);
  DBGMSG("done");
  s->fs_include(var1, var2);
  RETURN_UNIT;
} END

DEFINE3(gc_fsExclude) {
  DBGMSG("fsExclude");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);
  DECLARE_VAR(var2, stamp, pstamp, x2);
  DBGMSG("done");
  s->fs_exclude(var1, var2);
  RETURN_UNIT;
} END

DEFINE3(gc_fsThe) {
  DBGMSG("fsThe");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);
  DECLARE_VAR(var2, stamp, pstamp, x2);
  DBGMSG("done");
  s->fs_the(var1, var2);
  RETURN_UNIT;
} END

DEFINE3(gc_fsMin) {
  DBGMSG("fsMin");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);
  DECLARE_VAR(var2, stamp, pstamp, x2);
  DBGMSG("done");
  s->fs_min(var1, var2);
  RETURN_UNIT;
} END

DEFINE3(gc_fsMax) {
  DBGMSG("fsMax");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);
  DECLARE_VAR(var2, stamp, pstamp, x2);
  DBGMSG("done");
  s->fs_max(var1, var2);
  RETURN_UNIT;
} END

DEFINE3(gc_fsMatch) {
  DBGMSG("fsMatch");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);

  DECLARE_VECTOR(v, x2);
  int noOfVars = v->GetLength();

  if (noOfVars==0) {
    s->fs_cardRange(var1,0,0);
    RETURN_UNIT;
  }

  IntArgs vars(noOfVars);
  for (int i=noOfVars; i--;) {
    DECLARE_VAR(tmp, stamp, pstamp, v->Sub(i));
    vars[i] = tmp;
  }

  DBGMSG("done");
  s->fs_match(var1, vars);
  RETURN_UNIT;
} END

DEFINE3(gc_fsCard) {
  DBGMSG("fsCard");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);
  DECLARE_VAR(var2, stamp, pstamp, x2);
  DBGMSG("done");
  s->fs_card(var2, var1);
  RETURN_UNIT;
} END

DEFINE4(gc_fsCardRange) {
  DBGMSG("fsCardRange");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_INT(min, x1);
  DECLARE_INT(max, x2);
  DECLARE_VAR(var1, stamp, pstamp, x3);

  DBGMSG("done");
  s->fs_cardRange(var1, min, max);
  RETURN_UNIT;
} END

DEFINE4(gc_fsSuperOfInter) {
  DBGMSG("fsSuperOfInter");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);
  DECLARE_VAR(var2, stamp, pstamp, x2);
  DECLARE_VAR(var3, stamp, pstamp, x3);

  DBGMSG("done");
  s->fs_superOfInter(var1, var2, var3);
  RETURN_UNIT;
} END

DEFINE4(gc_fsSubOfUnion) {
  DBGMSG("fsSubOfUnion");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);
  DECLARE_VAR(var2, stamp, pstamp, x2);
  DECLARE_VAR(var3, stamp, pstamp, x3);

  DBGMSG("done");
  s->fs_subOfUnion(var1, var2, var3);
  RETURN_UNIT;
} END

DEFINE3(gc_fsSubset) {
  DBGMSG("fsSubset");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);
  DECLARE_VAR(var2, stamp, pstamp, x2);

  DBGMSG("done");
  s->fs_subset(var1, var2);
  RETURN_UNIT;
} END

DEFINE3(gc_fsNoSubset) {
  DBGMSG("fsNoSubset");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);
  DECLARE_VAR(var2, stamp, pstamp, x2);

  DBGMSG("done");
  s->fs_nosubset(var1, var2);
  RETURN_UNIT;
} END

DEFINE3(gc_fsDisjoint) {
  DBGMSG("fsDisjoint");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);
  DECLARE_VAR(var2, stamp, pstamp, x2);

  DBGMSG("done");
  s->fs_disjoint(var1, var2);
  RETURN_UNIT;
} END

DEFINE3(gc_fsDistinct) {
  DBGMSG("fsDistinct");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);
  DECLARE_VAR(var2, stamp, pstamp, x2);

  DBGMSG("done");
  s->fs_distinct(var1, var2);
  RETURN_UNIT;
} END

DEFINE2(gc_fsDistinctN) {
  DBGMSG("fsDistinctN");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VECTOR(v, x1);
  int noOfVars = v->GetLength();

  if (noOfVars==0) RETURN_UNIT;

  IntArgs vars(noOfVars);
  for (int i=noOfVars; i--;) {
    DECLARE_VAR(tmp, stamp, pstamp, v->Sub(i));
    vars[i] = tmp;
  }

  DBGMSG("done");
  s->fs_distinctn(vars);
  RETURN_UNIT;
} END

DEFINE3(gc_fsEquals) {
  DBGMSG("fsEquals");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);
  DECLARE_VAR(var2, stamp, pstamp, x2);

  DBGMSG("done");
  s->fs_equals(var1, var2);
  RETURN_UNIT;
} END

DEFINE2(gc_fsConvex) {
  DBGMSG("fsConvex");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);

  DBGMSG("done");
  s->fs_convex(var1);
  RETURN_UNIT;
} END

DEFINE3(gc_fsConvexHull) {
  DBGMSG("fsConvexHull");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);
  DECLARE_VAR(var2, stamp, pstamp, x2);

  DBGMSG("done");
  s->fs_convexHull(var1, var2);
  RETURN_UNIT;
} END

DEFINE4(gc_fsUnion) {
  DBGMSG("fsUnion");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);
  DECLARE_VAR(var2, stamp, pstamp, x2);
  DECLARE_VAR(var3, stamp, pstamp, x3);

  DBGMSG("done");
  s->fs_union(var1, var2, var3);
  RETURN_UNIT;
} END

DEFINE3(gc_fsComplement) {
  DBGMSG("fsComplement");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);
  DECLARE_VAR(var2, stamp, pstamp, x2);

  DBGMSG("done");
  s->fs_complement(var1, var2);
  RETURN_UNIT;
} END

DEFINE4(gc_fsIntersection) {
  DBGMSG("fsIntersection");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  DBGMSG("declare done");
  CHECK_SPACE(s);
  DBGMSG("check done");

  DECLARE_VAR(var1, stamp, pstamp, x1);
  DBGMSG("var 1");
  DECLARE_VAR(var2, stamp, pstamp, x2);
  DBGMSG("var 2");
  DECLARE_VAR(var3, stamp, pstamp, x3);
  DBGMSG("var 3");

  DBGMSG("done");
  s->fs_intersection(var1, var2, var3);
  RETURN_UNIT;
} END

DEFINE4(gc_fsDifference) {
  DBGMSG("fsDifference");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);
  DECLARE_VAR(var2, stamp, pstamp, x2);
  DECLARE_VAR(var3, stamp, pstamp, x3);

  DBGMSG("done");
  s->fs_difference(var1, var2, var3);
  RETURN_UNIT;
} END

DEFINE4(gc_fsPartition) {
  DBGMSG("fsPartition");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);
  DECLARE_VAR(var2, stamp, pstamp, x2);
  DECLARE_VAR(var3, stamp, pstamp, x3);

  DBGMSG("done");
  s->fs_partition(var1, var2, var3);
  RETURN_UNIT;
} END

DEFINE3(gc_fsUnionN) {
  DBGMSG("fsUnionN");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VECTOR(v, x1);
  DECLARE_VAR(var1, stamp, pstamp, x2);

  int noOfVars = v->GetLength();

  if (noOfVars==0) RETURN_UNIT;

  IntArgs vars(noOfVars);
  for (int i=noOfVars; i--;) {
    DECLARE_VAR(tmp, stamp, pstamp, v->Sub(i));
    vars[i] = tmp;
  }

  DBGMSG("done");
  s->fs_unionn(vars, var1);
  RETURN_UNIT;
} END

DEFINE3(gc_fsIntersectionN) {
  DBGMSG("fsIntersectionN");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VECTOR(v, x1);
  DECLARE_VAR(var1, stamp, pstamp, x2);

  int noOfVars = v->GetLength();

  if (noOfVars==0) RETURN_UNIT;

  IntArgs vars(noOfVars);
  for (int i=noOfVars; i--;) {
    DECLARE_VAR(tmp, stamp, pstamp, v->Sub(i));
    vars[i] = tmp;
  }

  DBGMSG("done");
  s->fs_intersectionn(vars, var1);
  RETURN_UNIT;
} END

DEFINE3(gc_fsPartitionN) {
  DBGMSG("fsIntersectionN");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VECTOR(v, x1);
  DECLARE_VAR(var1, stamp, pstamp, x2);

  int noOfVars = v->GetLength();

  if (noOfVars==0) RETURN_UNIT;

  IntArgs vars(noOfVars);
  for (int i=noOfVars; i--;) {
    DECLARE_VAR(tmp, stamp, pstamp, v->Sub(i));
    vars[i] = tmp;
  }

  DBGMSG("done");
  s->fs_partitionn(vars, var1);
  RETURN_UNIT;
} END

DEFINE1(gc_fsUniversal) {
  DBGMSG("fsUniversal");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  if (stamp==-1) stamp=pstamp;

  int newVar = s->AddSetVariable();

  // Creates a new set constant every time!
  // How should we handle constants?

  RangesFullSet full;
  s->fs_upperBound(newVar,full);
  s->fs_lowerBound(newVar,full);

  DBGMSG("done");
  RETURN_GECODE_VAR(newVar,stamp);
} END

DEFINE2(gc_fsIs) {
  DBGMSG("fsUniversal");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);

  RETURN_BOOL(s->fs_assigned(var1));
} END

DEFINE4(gc_fsIncludeR) {
  DBGMSG("fsIncludeR");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);
  DECLARE_VAR(var2, stamp, pstamp, x2);
  DECLARE_VAR(var3, stamp, pstamp, x3);

  DBGMSG("done");
  s->fs_includeR(var1, var2, var3);
  RETURN_UNIT;
} END

DEFINE4(gc_fsIncludeRI) {
  DBGMSG("fsIncludeRI");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_INT(i, x1);
  DECLARE_VAR(var1, stamp, pstamp, x2);
  DECLARE_VAR(var3, stamp, pstamp, x3);

  DBGMSG("done");
  s->fs_includeRI(i, var1, var3);
  RETURN_UNIT;
} END

DEFINE4(gc_fsEqualR) {
  DBGMSG("fsEqualR");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VAR(var1, stamp, pstamp, x1);
  DECLARE_VAR(var2, stamp, pstamp, x2);
  DECLARE_VAR(var3, stamp, pstamp, x3);

  DBGMSG("done");
  s->fs_equalR(var1, var2, var3);
  RETURN_UNIT;
} END

DEFINE4(gc_fsSubsetR) {
  DBGMSG("fsSubsetR");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_VAR(var1, stamp, pstamp, x1);
  DECLARE_VAR(var2, stamp, pstamp, x2);
  DECLARE_VAR(var3, stamp, pstamp, x3);

  DBGMSG("done");
  s->fs_subsetR(var1, var2, var3);
  RETURN_UNIT;
} END


DEFINE4(gc_fsSelectUnion) {
  DBGMSG("fsSelectUnion");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);
  DECLARE_VECTOR(v, x2);
  DECLARE_VAR(var2, stamp, pstamp, x3);

  int noOfVars = v->GetLength();

  if (noOfVars==0) RETURN_UNIT;

  IntArgs vars(noOfVars);
  for (int i=noOfVars; i--;) {
    DECLARE_VAR(tmp, stamp, pstamp, v->Sub(i));
    vars[i] = tmp;
  }

  DBGMSG("done");
  s->fs_selectUnion(var1, vars, var2);
  RETURN_UNIT;
} END

DEFINE4(gc_fsSelectInter) {
  DBGMSG("fsSelectInter");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);
  DECLARE_VECTOR(v, x2);
  DECLARE_VAR(var2, stamp, pstamp, x3);

  int noOfVars = v->GetLength();

  if (noOfVars==0) RETURN_UNIT;

  IntArgs vars(noOfVars);
  for (int i=noOfVars; i--;) {
    DECLARE_VAR(tmp, stamp, pstamp, v->Sub(i));
    vars[i] = tmp;
  }

  DBGMSG("done");
  s->fs_selectInter(var1, vars, var2);
  RETURN_UNIT;
} END

DEFINE4(gc_fsSelectSets) {
  DBGMSG("fsSelectSets");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);
  DECLARE_VECTOR(v, x2);
  DECLARE_VAR(var2, stamp, pstamp, x3);

  int noOfVars = v->GetLength();

  if (noOfVars==0) RETURN_UNIT;

  IntArgs vars(noOfVars);
  for (int i=noOfVars; i--;) {
    DECLARE_VAR(tmp, stamp, pstamp, v->Sub(i));
    vars[i] = tmp;
  }

  DBGMSG("done");
  s->fs_selectSets(var1, vars, var2);
  RETURN_UNIT;
} END

DEFINE4(gc_fsBranch) {
  DBGMSG("fsBranch");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VECTOR(v, x1);
  DECLARE_INT(varsel, x2);
  DECLARE_INT(valsel, x3);

  int noOfVars = v->GetLength();

  if (noOfVars==0) RETURN_UNIT;

  IntArgs vars(noOfVars);
  for (int i=noOfVars; i--;) {
    DECLARE_VAR(tmp, stamp, pstamp, v->Sub(i));
    vars[i] = tmp;
  }

  s->fs_branch(vars, UnsafeGecode::int2fsbvarsel[varsel],
	       UnsafeGecode::int2fsbvalsel[valsel]);

  DBGMSG("done");

  RETURN_UNIT;
} END

// These are only for debugging purposes
DEFINE1(gc_stamps) {
  DBGMSG("stamps");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  DBGMSG("done");
  RETURN_IPAIR(stamp, pstamp);
} END
DEFINE1(gc_varStamp) {
  DBGMSG("varStamp");
  DECLARE_TUPLE(var, x0);
  DBGMSG("done");
  RETURN(var->Sel(1));
} END
DEFINE2(gc_fsPrint) {
  DBGMSG("fsPrint");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  DECLARE_VAR(var1, stamp, pstamp, x1);

  s->fs_print(var1);
  RETURN_UNIT;
} END

static word UnsafeGecodeBase() {
  Record *record = Record::New(13);
  UnsafeGecode::InvalidSpaceConstructor =
    UniqueConstructor::New("InvalidSpace",
			   "UnsafeGecode.UnsafeGecodeBase.InvalidSpace")->ToWord();
  RootSet::Add(UnsafeGecode::InvalidSpaceConstructor);

  UnsafeGecode::InvalidVarConstructor =
    UniqueConstructor::New("InvalidVar",
			   "UnsafeGecode.UnsafeGecodeBase.InvalidVar")->ToWord();
  RootSet::Add(UnsafeGecode::InvalidVarConstructor);
  record->Init("'InvalidSpace", UnsafeGecode::InvalidSpaceConstructor);
  record->Init("InvalidSpace", UnsafeGecode::InvalidSpaceConstructor);

  record->Init("'InvalidVar", UnsafeGecode::InvalidVarConstructor);
  record->Init("InvalidVar", UnsafeGecode::InvalidVarConstructor);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeBase", "makeSpace",
		 gc_makespace, 0);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeBase", "fail",
		 gc_fail, 1);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeBase", "status",
		 gc_status, 1);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeBase", "commit",
		 gc_commit, 2);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeBase", "clone",
		 gc_clone, 1);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeBase", "discard",
		 gc_discard, 1);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeBase", "alive",
		 gc_alive, 1);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeBase", "stamps",
		 gc_stamps, 1);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeBase", "varStamp",
		 gc_varStamp, 1);
  return record->ToWord();
}

static word UnsafeGecodeFD() {
  Record *record = Record::New(41);

  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "intvar",
		 gc_fdvar, 2);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "intvarR",
		 gc_fdvarr, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "boolvar",
		 gc_boolvar, 1);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "getMin",
		 gc_getmin, 2);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "getMax",
		 gc_getmax, 2);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "getDom",
		 gc_getdom, 2);  
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "distinct",
		 gc_distinct, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "distinctOffset",
		 gc_distincti, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "countII",
		 gc_countii, 6);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "countVI",
		 gc_countvi, 6);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "countIV",
		 gc_countiv, 6);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "countVV",
		 gc_countvv, 6);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "element",
		 gc_element, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "elementI",
		 gc_elementi, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "lex",
		 gc_lex, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "dom",
		 gc_dom, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "domR",
		 gc_domr, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "rel",
		 gc_rel, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "relI",
		 gc_reli, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "relR",
		 gc_relr, 5);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "relIR",
		 gc_relir, 5);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "equal",
		 gc_eq, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "equalV",
		 gc_eqv, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "equalR",
		 gc_eqr, 5);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "linear",
		 gc_linear, 5);  
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "linearR",
		 gc_linearr, 6);  
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "min",
		 gc_min, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "max",
		 gc_max, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "abs",
		 gc_abs, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "mult",
		 gc_mult, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "power",
		 gc_power, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "nega",
		 gc_bool_not, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "conj",
		 gc_bool_and, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "disj",
		 gc_bool_or, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "impl",
		 gc_bool_imp, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "equi",
		 gc_bool_eq, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "exor",
		 gc_bool_xor, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "conjV",
		 gc_bool_andv, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "disjV",
		 gc_bool_orv, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "branch",
		 gc_branch, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFD", "assign",
		 gc_assign, 3);
  
  return record->ToWord();
}

static word UnsafeGecodeFS() {
  Record *record = Record::New(47);

  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "setvar",
		 gc_fsvar, 1);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "upperBound",
		 gc_fsUpperBound, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "lowerBound",
		 gc_fsLowerBound, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "incl",
		 gc_fsInclude, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "excl",
		 gc_fsExclude, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "the",
		 gc_fsThe, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "min",
		 gc_fsMin, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "max",
		 gc_fsMax, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "match",
		 gc_fsMatch, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "card",
		 gc_fsCard, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "cardRange",
		 gc_fsCardRange, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "superOfInter",
		 gc_fsSuperOfInter, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "subOfUnion",
		 gc_fsSubOfUnion, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "compl",
		 gc_fsComplement, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "difference",
		 gc_fsDifference, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "intersect",
		 gc_fsIntersection, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "intersectN",
		 gc_fsIntersectionN, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "union",
		 gc_fsUnion, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "unionN",
		 gc_fsUnionN, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "subset",
		 gc_fsSubset, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "noSubset",
		 gc_fsNoSubset, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "disjoint",
		 gc_fsDisjoint, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "distinct",
		 gc_fsDistinct, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "distinctN",
		 gc_fsDistinctN, 2);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "equals",
		 gc_fsEquals, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "convex",
		 gc_fsConvex, 2);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "convexHull",
		 gc_fsConvexHull, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "partition",
		 gc_fsPartition, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "partitionN",
		 gc_fsPartitionN, 3);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "universal",
		 gc_fsUniversal, 1);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "is",
		 gc_fsIs, 2);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "inclR",
		 gc_fsIncludeR, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "isInR",
		 gc_fsIncludeRI, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "equalR",
		 gc_fsEqualR, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "subsetR",
		 gc_fsSubsetR, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "selectSetVar",
		 gc_fsSelectSets, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "selectUnion",
		 gc_fsSelectUnion, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "selectInter",
		 gc_fsSelectInter, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "getUpperBound",
		 gc_fsUB, 2);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "getLowerBound",
		 gc_fsLB, 2);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "getCard",
		 gc_fsGetCard, 2);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "getUnknown",
		 gc_fsUnknown, 2);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "getCardOfLowerBound",
		 gc_fsGetCardLB, 2);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "getCardOfUpperBound",
		 gc_fsGetCardUB, 2);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "getCardOfUnknown",
		 gc_fsGetCardUnknown, 2);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "setvarbranch",
		 gc_fsBranch, 4);
  INIT_STRUCTURE(record, "UnsafeGecode.UnsafeGecodeFS", "print",
		 gc_fsPrint, 2);

  return record->ToWord();
}

word InitComponent() {
  DBGMSG("init gecode");
  UnsafeGecode::gecodeFinalizationSet = new UnsafeGecode::GecodeFinalizationSet();
  UnsafeGecode::gecodeHandler = new UnsafeGecode::GecodeHandler();

  // This is used to mark variables as sited
  ConcreteRepresentation *cr =
    ConcreteRepresentation::New(UnsafeGecode::gecodeHandler,1);
  cr->Init(0, Store::IntToWord(0));
  UnsafeGecode::SitedMarker = cr->ToWord();
  RootSet::Add(UnsafeGecode::SitedMarker);

  Record *record = Record::New(3);

  record->Init("UnsafeGecodeBase$", UnsafeGecodeBase());
  record->Init("UnsafeGecodeFD$", UnsafeGecodeFD());
  record->Init("UnsafeGecodeFS$", UnsafeGecodeFS());

  DBGMSG("done");

  RETURN_STRUCTURE("UnsafeGecode$", record);
}
