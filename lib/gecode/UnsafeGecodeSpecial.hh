/*
 * Authors:
 *  Guido Tack <tack@ps.uni-sb.de>
 *
 * Copyright:
 *  Guido Tack, 2003
 *
 *  See the file "debian/copyright" for information on usage and
 *  redistribution of this file, and for a
 *     DISCLAIMER OF ALL WARRANTIES.
 *
 *  Last change:
 *    $Date$ by $Author$
 *    $Revision$
 * 
 */

#include "GecodeSpace.hh"
#include "Alice.hh"

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

#define RETURN_GECODE_VAR(newVar,stamp,pstamp)   \
{Tuple *t = Tuple::New(3);       \
 if(stamp==-1) stamp=pstamp;     \
 t->Init(0,Store::IntToWord(newVar)); \
 t->Init(1,Store::IntToWord(stamp)); \
 t->Init(2,UnsafeGecode::SitedMarker);   \
 RETURN(t->ToWord()); }

#define DECLARE_SPACE(s, stamp, pstamp, x)                                  \
  GecodeSpace *s;                                                           \
  int stamp, pstamp;                                                        \
  if (Store::WordToTransient(x) != INVALID_POINTER) { REQUEST(x); }         \
  { ConcreteRepresentation *cr = ConcreteRepresentation::FromWord(x); \
    s = (GecodeSpace *) Store::WordToUnmanagedPointer(cr->Get(0));          \
    stamp = Store::DirectWordToInt(cr->Get(1));                             \
    pstamp = Store::DirectWordToInt(cr->Get(2));                            \
  } \
  DBGMSG("DECLARE SPACE done");

#define DEFINE6(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::GetNArgs() == 6);			\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::GetCurrentArg(0);		\
    word x1 = Scheduler::GetCurrentArg(1);		\
    word x2 = Scheduler::GetCurrentArg(2);		\
    word x3 = Scheduler::GetCurrentArg(3);		\
    word x4 = Scheduler::GetCurrentArg(4);              \
    word x5 = Scheduler::GetCurrentArg(5);

#define DEFINE7(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::GetNArgs() == 7);			\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::GetCurrentArg(0);		\
    word x1 = Scheduler::GetCurrentArg(1);		\
    word x2 = Scheduler::GetCurrentArg(2);		\
    word x3 = Scheduler::GetCurrentArg(3);		\
    word x4 = Scheduler::GetCurrentArg(4);              \
    word x5 = Scheduler::GetCurrentArg(5);              \
    word x6 = Scheduler::GetCurrentArg(6);

#define DEFINE8(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::GetNArgs() == 8);			\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::GetCurrentArg(0);		\
    word x1 = Scheduler::GetCurrentArg(1);		\
    word x2 = Scheduler::GetCurrentArg(2);		\
    word x3 = Scheduler::GetCurrentArg(3);		\
    word x4 = Scheduler::GetCurrentArg(4);              \
    word x5 = Scheduler::GetCurrentArg(5);              \
    word x6 = Scheduler::GetCurrentArg(6);              \
    word x7 = Scheduler::GetCurrentArg(7);

#define DEFINE9(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::GetNArgs() == 9);			\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::GetCurrentArg(0);		\
    word x1 = Scheduler::GetCurrentArg(1);		\
    word x2 = Scheduler::GetCurrentArg(2);		\
    word x3 = Scheduler::GetCurrentArg(3);		\
    word x4 = Scheduler::GetCurrentArg(4);              \
    word x5 = Scheduler::GetCurrentArg(5);              \
    word x6 = Scheduler::GetCurrentArg(6);              \
    word x7 = Scheduler::GetCurrentArg(7);              \
    word x8 = Scheduler::GetCurrentArg(8);

#define DEFINE10(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::GetNArgs() == 10);		\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::GetCurrentArg(0);		\
    word x1 = Scheduler::GetCurrentArg(1);		\
    word x2 = Scheduler::GetCurrentArg(2);		\
    word x3 = Scheduler::GetCurrentArg(3);		\
    word x4 = Scheduler::GetCurrentArg(4);              \
    word x5 = Scheduler::GetCurrentArg(5);              \
    word x6 = Scheduler::GetCurrentArg(6);              \
    word x7 = Scheduler::GetCurrentArg(7);              \
    word x8 = Scheduler::GetCurrentArg(8);              \
    word x9 = Scheduler::GetCurrentArg(9);

#define DEFINE11(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::GetNArgs() == 11);		\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::GetCurrentArg(0);		\
    word x1 = Scheduler::GetCurrentArg(1);		\
    word x2 = Scheduler::GetCurrentArg(2);		\
    word x3 = Scheduler::GetCurrentArg(3);		\
    word x4 = Scheduler::GetCurrentArg(4);              \
    word x5 = Scheduler::GetCurrentArg(5);              \
    word x6 = Scheduler::GetCurrentArg(6);              \
    word x7 = Scheduler::GetCurrentArg(7);              \
    word x8 = Scheduler::GetCurrentArg(8);              \
    word x9 = Scheduler::GetCurrentArg(9);              \
    word x10 = Scheduler::GetCurrentArg(10);

#define CHECK_SPACE(s) if (!s) RAISE(UnsafeGecode::InvalidSpaceConstructor);

#define DECLARE_INTVAR(v, s, stamp, pstamp, x)                  \
 IntVar v;                                                      \
 {                                                              \
   u_int varIdx;                                                \
   DECLARE_TUPLE(varIntern, x);                                 \
   s_int myStamp = Store::DirectWordToInt(varIntern->Sel(1));   \
   if (myStamp != stamp && myStamp != pstamp)                   \
     RAISE(UnsafeGecode::InvalidVarConstructor);                \
   varIdx = Store::DirectWordToInt(varIntern->Sel(0));          \
   v = s->is[varIdx];                                           \
 }                                                              \
 DBGMSG("DECLARE_VAR done.");

#define DECLARE_BOOLVAR(v, s, stamp, pstamp, x)                 \
 BoolVar v;                                                     \
 {                                                              \
   u_int varIdx;                                                \
   DECLARE_TUPLE(varIntern, x);                                 \
   s_int myStamp = Store::DirectWordToInt(varIntern->Sel(1));   \
   if (myStamp != stamp && myStamp != pstamp)                   \
     RAISE(UnsafeGecode::InvalidVarConstructor);                \
   varIdx = Store::DirectWordToInt(varIntern->Sel(0));          \
   intvar2boolvar(b, s->is[varIdx]);                            \
   v = b;                                                       \
 }                                                              \
 DBGMSG("DECLARE_VAR done.");

#define DECLARE_SETVAR(v, s, stamp, pstamp, x)                  \
 SetVar v;                                                      \
 {                                                              \
   u_int varIdx;                                                \
   DECLARE_TUPLE(varIntern, x);                                 \
   s_int myStamp = Store::DirectWordToInt(varIntern->Sel(1));   \
   if (myStamp != stamp && myStamp != pstamp)                   \
     RAISE(UnsafeGecode::InvalidVarConstructor);                \
   varIdx = Store::DirectWordToInt(varIntern->Sel(0));          \
   v = s->fss[varIdx];                                          \
 }                                                              \
 DBGMSG("DECLARE_VAR done.");

#define DECLARE_DESCRIPTION(desc, x) \
 BranchingDesc* desc; \
 if (Store::WordToTransient(x) != INVALID_POINTER) { REQUEST(x); } \
 { ConcreteRepresentation *cr = ConcreteRepresentation::FromWord(x); \
 desc = static_cast<BranchingDesc *>(Store::WordToUnmanagedPointer(cr->Get(0))); \
 }

namespace UnsafeGecode {

  static word SitedMarker;
  static word InvalidVarConstructor;
  static word InvalidSpaceConstructor;
  static word InvalidDomainConstructor;
  static word DescriptionConstructor;
  static word InternalExceptionConstructor;
  static s_int SpaceStamp = 0;

const BvarSel int2bvarsel[] =
  {
    BVAR_DEGREE_MAX, BVAR_DEGREE_MIN,
    BVAR_MAX_MAX, BVAR_MAX_MIN,
    BVAR_MIN_MAX, BVAR_MIN_MIN,
    BVAR_NONE,
    BVAR_REGRET_MAX_MAX,
    BVAR_REGRET_MAX_MIN,
    BVAR_REGRET_MIN_MAX,
    BVAR_REGRET_MIN_MIN,
    BVAR_SIZE_MAX,
    BVAR_SIZE_MIN
  };

const BvalSel int2bvalsel[] =
  {
    BVAL_MAX, BVAL_MED, BVAL_MIN,
    BVAL_SPLIT_MAX, BVAL_SPLIT_MIN
  };

const IntRelType int2intreltype[] =
  {
    IRT_EQ, IRT_GQ, IRT_GR,
    IRT_LE, IRT_LQ, IRT_NQ
  };

const SetRelType int2setreltype[] =
  {
    SRT_CMPL, SRT_DISJ, SRT_EQ,
    SRT_NQ, SRT_SUB, SRT_SUP
  };

const SetOpType int2setoptype[] =
  {
    SOT_DUNION, SOT_INTER, SOT_MINUS, SOT_UNION
  };

const IntConLevel int2cl[] =
  {
    ICL_BND, ICL_DEF, ICL_DOM,
    ICL_VAL
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
  Transform *GetAbstractRepresentation(ConcreteRepresentation *) {
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
  ConcreteRepresentation *cr = ConcreteRepresentation::FromWord(value);
  word ptr = cr->Get(0);
  GecodeSpace *s = (GecodeSpace *)Store::WordToUnmanagedPointer(ptr);
  delete s;
}

class GecodeBranchdescFinalizationSet: public FinalizationSet {
public:
  virtual void Finalize(word value);
};

static GecodeBranchdescFinalizationSet *gecodeBranchdescFinalizationSet;

void GecodeBranchdescFinalizationSet::Finalize(word value) {
  ConcreteRepresentation *cr = ConcreteRepresentation::FromWord(value);
  word ptr = cr->Get(0);
  BranchingDesc *desc =
    static_cast<BranchingDesc *>(Store::WordToUnmanagedPointer(ptr));
  delete desc;
}

void init() {
  gecodeFinalizationSet =
    new GecodeFinalizationSet();
  gecodeBranchdescFinalizationSet =
    new GecodeBranchdescFinalizationSet();
  gecodeHandler = new GecodeHandler();

  // This is used to mark variables as sited
  ConcreteRepresentation *cr =
    ConcreteRepresentation::New(gecodeHandler,1);
  cr->Init(0, Store::IntToWord(0));
  SitedMarker = cr->ToWord();
  RootSet::Add(SitedMarker);
  
  InvalidSpaceConstructor =
    UniqueConstructor::New("InvalidSpace",
			   "UnsafeGecode.InvalidSpace")->ToWord();
  RootSet::Add(InvalidSpaceConstructor);

  InvalidVarConstructor =
    UniqueConstructor::New("InvalidVar",
			   "UnsafeGecode.InvalidVar")->ToWord();
  RootSet::Add(InvalidVarConstructor);
  InvalidDomainConstructor =
    UniqueConstructor::New("InvalidDomain",
			   "UnsafeGecode.InvalidDomain")->ToWord();
  RootSet::Add(InvalidDomainConstructor);

  DescriptionConstructor =
    UniqueConstructor::New("Description",
			   "UnsafeGecode.Description")->ToWord();
  RootSet::Add(DescriptionConstructor);

  InternalExceptionConstructor =
    UniqueConstructor::New("InternalException",
			   "UnsafeGecode.InternalException")->ToWord();
  RootSet::Add(InternalExceptionConstructor);
}

}

DEFINE0(makespace) {
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

DEFINE1(status) {
  DBGMSG("status");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  unsigned int alternatives;
  switch(s->status(alternatives)) {
  case SS_BRANCH:
    DBGMSG("done");
    {
      ConcreteRepresentation *cr =
        ConcreteRepresentation::New(UnsafeGecode::gecodeHandler,1);
      cr->Init(0, Store::UnmanagedPointerToWord(s->description()));
      UnsafeGecode::gecodeBranchdescFinalizationSet->Register(cr->ToWord());
      TagVal *t = TagVal::New(0, 2);
      t->Init(0, Store::IntToWord(alternatives));
      t->Init(1, cr->ToWord());
      RETURN(t->ToWord());
    }
    break;
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

DEFINE1(description) {
  DBGMSG("description");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  unsigned int dummy;
  switch(s->status(dummy)) {
  case SS_BRANCH:
    break;
  default:
    RAISE(UnsafeGecode::DescriptionConstructor)
  }

  ConcreteRepresentation *cr =
    ConcreteRepresentation::New(UnsafeGecode::gecodeHandler,1);
  cr->Init(0, Store::UnmanagedPointerToWord(s->description()));
  UnsafeGecode::gecodeBranchdescFinalizationSet->Register(cr->ToWord());
  DBGMSG("done");
  RETURN(cr->ToWord());
} END

DEFINE1(clone) {
  DBGMSG("clone");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);

  unsigned int dummy;
  if (s->status(dummy) == SS_FAILED)
    RAISE(UnsafeGecode::InvalidSpaceConstructor)  

  if (stamp==-1) {
    ConcreteRepresentation *cr = ConcreteRepresentation::FromWord(x0);
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

DEFINE1(discard) {
  DBGMSG("discard");
  GecodeSpace *s;
  if (Store::WordToTransient(x0) != INVALID_POINTER) { REQUEST(x0); }
  ConcreteRepresentation *cr = ConcreteRepresentation::FromWord(x0);
  s = (GecodeSpace *) Store::WordToUnmanagedPointer(cr->Get(0));
  
  delete s;
  cr->Replace(0, Store::IntToWord(0));
  DBGMSG("done");
  RETURN_UNIT;
} END

DEFINE1(alive) {
  DBGMSG("alive");
  if (Store::WordToTransient(x0) != INVALID_POINTER) { REQUEST(x0); }
  ConcreteRepresentation *cr = ConcreteRepresentation::FromWord(x0);
  
  DBGMSG("done");
  RETURN_BOOL(Store::WordToInt(cr->Get(0))!=0);
} END

DEFINE1(fail) {
  DBGMSG("fail");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  s->fail();
  RETURN_UNIT;
} END

DEFINE2(int_getRanges) {
  DBGMSG("int_getRanges");
  DECLARE_SPACE(s, stamp, pstamp, x0);
  CHECK_SPACE(s);
  DECLARE_INTVAR(var1, s, stamp, pstamp, x1);
  unsigned int retIterSize = 0;
  Int::IntView iv(var1);
  for(Int::ViewRanges<Int::IntView> iters(iv);
      iters(); ++iters) retIterSize++;
  Int::ViewRanges<Int::IntView> ret(var1);
  Vector *vret = Vector::New(retIterSize);
  if(retIterSize>0) {
    u_int count = 0;
    for (; ret(); ++ret) {
      Tuple *t = Tuple::New(2);
      t->Init(0, Store::IntToWord(ret.min()));
      t->Init(1, Store::IntToWord(ret.max()));
      vret->Init(count, t->ToWord());
      count++;
    }
  }
  RETURN(vret->ToWord());
} END
