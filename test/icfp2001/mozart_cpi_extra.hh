#ifndef __MOZART_CPI_EXTRA__HH__
#define __MOZART_CPI_EXTRA__HH__

#include "mozart_cpi.hh"

inline int min(int i,int j)
{
  return (i<j)?i:j;
}

inline int max(int i,int j)
{
  return (i<j)?j:i;
}

class OZ_FDIntVar_Snapshot {
private:
  int size;
public:
  OZ_FDIntVar_Snapshot(){};
  void operator = (OZ_FDIntVar&fd) {
    size = fd->getSize();
  }
  int operator == (OZ_FDIntVar&fd) {
    return (size==fd->getSize());
  }
  int operator != (OZ_FDIntVar&fd) {
    return !(*this == fd);
  }
};

class OZ_FSetVar_Snapshot {
private:
  int known_in,known_not_in,card_size;
public:
  OZ_FSetVar_Snapshot(){}
  void operator = (OZ_FSetVar& fs) {
    known_in     = fs->getKnownIn   ();
    known_not_in = fs->getKnownNotIn();
    card_size    = fs->getCardSize  ();
  }
  int operator == (OZ_FSetVar& fs) {
    return (known_in     == fs->getKnownIn   () &&
	    known_not_in == fs->getKnownNotIn() &&
	    card_size    == fs->getCardSize  ());
  }
  int operator != (OZ_FSetVar& fs) {
    return !(*this == fs);
  }
};

class OZ_FDIntVar_Iterator {
private:
  int size;
  OZ_FDIntVar* vect;
public:
  OZ_FDIntVar_Iterator(int s,OZ_FDIntVar* v):size(s),vect(v){}

  // vars[i]==0 means that this var was dropped earlier
  // vars[i]==1 means that this var was dropped during this run

  OZ_Boolean leave(OZ_Term *vars) {
    OZ_Boolean vars_left = OZ_FALSE;
    for (int i=size;i--;) {
      int t = vars[i];
      if (t) {
	OZ_Boolean maybe = vect[i].leave();
	// if var marked as having been dropped during this run
	// we must clear it, and not count it.
	if (t==1) vars[i]=0;
	else vars_left |= maybe;
      }
    }
    return vars_left;
  }
  void fail(OZ_Term *vars) {
    for (int i=size;i--;) {
      if (vars[i]) vect[i].fail();
    }
  }
  OZ_Boolean isTouched(OZ_Term*t) {
    for (int i=size;i--;) {
      OZ_Term v = t[i];
      if (v!=0 && v!=1) {
	if (vect[i].isTouched()) return OZ_TRUE;
      }
    }
    return OZ_FALSE;
  }
  void ask(OZ_Term*t) {
    for (int i=size;i--;) {
      OZ_Term v = t[i];
      if (v) vect[i].ask(v);
    }
  }
  int read(OZ_Term*t) {
    int n = 0;
    for (int i=size;i--;) {
      OZ_Term v = t[i];
      if (v) {
	int m = vect[i].read(v);
	if (m>n) n=m;
      }
    }
    return n;
  }
  int readEncap(OZ_Term*t) {
    int n = 0;
    for (int i=size;i--;) {
      OZ_Term v = t[i];
      if (v) {
	int m = vect[i].readEncap(v);
	if (m>n) n=m;
      }
    }
    return n;
  }
};

class OZ_FSetVar_Iterator {
private:
  int size;
  OZ_FSetVar* vect;
public:
  OZ_FSetVar_Iterator(int s,OZ_FSetVar* v):size(s),vect(v){}

  OZ_Boolean leave(OZ_Term*t) {
    OZ_Boolean vars_left = OZ_FALSE;
    for (int i=size;i--;) {
      OZ_Term v = t[i];
      if (v) {
	// don't count vars that are being dropped
	OZ_Boolean maybe = vect[i].leave();
	if (v==1) t[i]=0;
	else vars_left |= maybe;
      }
    }
    return vars_left;
  }
  void fail(OZ_Term *vars) {
    for (int i=size;i--;) {
      if (vars[i]) vect[i].fail();
    }
  }
  OZ_Boolean isTouched(OZ_Term*t) {
    for (int i=size;i--;) {
      OZ_Term v = t[i];
      if (v!=0 && v!=1) {
	if (vect[i].isTouched()) return OZ_TRUE;
      }
    }
    return OZ_FALSE;
  }
  void ask(OZ_Term*t) {
    for (int i=size;i--;) {
      OZ_Term v = t[i];
      if (v) vect[i].ask(v);
    }
  }
  void read(OZ_Term*t) {
    for (int i=size;i--;) {
      OZ_Term v = t[i];
      if (v) vect[i].read(v);
    }
  }
  void readEncap(OZ_Term*t) {
    for (int i=size;i--;) {
      OZ_Term v = t[i];
      if (v) vect[i].readEncap(v);
    }
  }
};

class SelectExpect : public OZ_Expect {
public:
  OZ_expect_t expectFD(OZ_Term t) {
    return expectIntVar(t,fd_prop_any);
  }
  OZ_expect_t expectFDVector(OZ_Term v) {
    return expectVector(v,(OZ_ExpectMeth) &SelectExpect::expectFD);
  }
  OZ_expect_t expectFS(OZ_Term t) {
    return expectFSetVar(t,fs_prop_bounds);
  }
  OZ_expect_t expectFSVector(OZ_Term v) {
    return expectVector(v,(OZ_ExpectMeth) &SelectExpect::expectFS);
  }
};

#define OZ_EXPECT_PROP(O, A, F, E)                      \
  {                                                     \
    OZ_Term     P = OZ_in(A);                           \
    OZ_expect_t r = O.F(P,E);                           \
    if (O.isFailing(r)) {                               \
      O.fail();                                         \
      return OZ_typeErrorCPI(expectedType, A, "");      \
    } else if (O.isSuspending(r) || O.isExceptional(r)) \
      return O.suspend();                               \
  }

#define OZ_EXPECT_VECTOR(O, A, F)                       \
  {                                                     \
    OZ_Term     P = OZ_in(A);                           \
    OZ_expect_t r = O.expectVector(P,(OZ_ExpectMeth)&F);\
    if (O.isFailing(r)) {                               \
      O.fail();                                         \
      return OZ_typeErrorCPI(expectedType, A, "");      \
    } else if (O.isSuspending(r) || O.isExceptional(r)) \
      return O.suspend();                               \
  }

#endif // __MOZART_CPI_EXTRA__HH__
