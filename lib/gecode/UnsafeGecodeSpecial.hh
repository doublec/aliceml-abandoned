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
    Assert(Scheduler::nArgs == 6);			\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];		\
    word x2 = Scheduler::currentArgs[2];		\
    word x3 = Scheduler::currentArgs[3];		\
    word x4 = Scheduler::currentArgs[4];                \
    word x5 = Scheduler::currentArgs[5];

#define DEFINE7(name)					\
  static Worker::Result name() {			\
    Assert(Scheduler::nArgs == 6);			\
    POP_PRIM_SELF(); \
    word x0 = Scheduler::currentArgs[0];		\
    word x1 = Scheduler::currentArgs[1];		\
    word x2 = Scheduler::currentArgs[2];		\
    word x3 = Scheduler::currentArgs[3];		\
    word x4 = Scheduler::currentArgs[4];                \
    word x5 = Scheduler::currentArgs[5];                \
    word x6 = Scheduler::currentArgs[6];

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

#define DECLARE_DESCRIPTION(desc, x) \
 BranchDesc* desc; \
 if (Store::WordToTransient(x) != INVALID_POINTER) { REQUEST(x); } \
 { ConcreteRepresentation *cr = ConcreteRepresentation::FromWord(x); \
 desc = static_cast<BranchDesc *>(Store::WordToUnmanagedPointer(cr->Get(0))); \
 }

namespace UnsafeGecode {

  static word SitedMarker;
  static word InvalidVarConstructor;
  static word InvalidSpaceConstructor;
  static word InvalidDomainConstructor;
  static word DescriptionConstructor;
  static s_int SpaceStamp = 0;
  
  class VectorRangeIterator {
  private:
    Vector *v;
    u_int pos;
    u_int size;
  public:
    VectorRangeIterator(Vector *vv) : v(vv), pos(0), size(v->GetLength()) {}

    void operator++(void) { pos++; }
    bool operator()(void) const { return pos < size; }
    int min(void) const {
      Tuple *t = Tuple::FromWord(v->Sub(pos));
      return Store::WordToInt(t->Sel(0));
    }
    int max(void) const {
      Tuple *t = Tuple::FromWord(v->Sub(pos));
      return Store::WordToInt(t->Sel(1));
    }
    unsigned int width(void) const {
      Tuple *t = Tuple::FromWord(v->Sub(pos));
      return Store::WordToInt(t->Sel(1))-Store::WordToInt(t->Sel(0));
    }
  };

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
  BranchDesc *desc =
    static_cast<BranchDesc *>(Store::WordToUnmanagedPointer(ptr));
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
  switch(s->status()) {
  case SS_BRANCH:
    DBGMSG("done");
    {
      ConcreteRepresentation *cr =
        ConcreteRepresentation::New(UnsafeGecode::gecodeHandler,1);
      cr->Init(0, Store::UnmanagedPointerToWord(s->description()));
      UnsafeGecode::gecodeBranchdescFinalizationSet->Register(cr->ToWord());
      TagVal *t = TagVal::New(0, 2);
      t->Init(0, Store::IntToWord(s->alternatives()));
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

  switch(s->status()) {
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

using namespace Iter::Ranges;
