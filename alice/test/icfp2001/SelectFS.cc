#include "mozart_cpi_extra.hh"

class FSetSelectPropagator : public OZ_Propagator {
private:
  static OZ_PropagatorProfile profile;
  OZ_Term * _tuple;
  OZ_Term   _index;
  OZ_Term   _value;
  int size;
  OZ_Boolean initialized;
public:
  FSetSelectPropagator(OZ_Term tuple,OZ_Term index,OZ_Term value)
    : _index(index),_value(value),size(OZ_vectorSize(tuple)),
      initialized(OZ_FALSE)
  {
    _tuple = OZ_hallocOzTerms(size);
    OZ_getOzTermVector(tuple,_tuple);
  }
  virtual OZ_Return propagate(void);
  virtual size_t sizeOf() { return sizeof(FSetSelectPropagator); }
  virtual OZ_PropagatorProfile *getProfile(void) const { return &profile; }
  virtual OZ_Term getParameters(void) const;
  virtual void sClone(void);
  virtual void gCollect(void);
};

OZ_PropagatorProfile FSetSelectPropagator::profile = "Select.fs";

OZ_Term FSetSelectPropagator::getParameters(void) const
{
  OZ_Term lst = OZ_nil();
  for (int i=size;i--;) {
    OZ_Term t = _tuple[i];
    if (t==0 || t==1) t=OZ_unit();
    lst=OZ_cons(t,lst);
  }
  return OZ_cons(lst,OZ_cons(_index,OZ_cons(_value,OZ_nil())));
}

void FSetSelectPropagator::sClone(void) {
  _tuple = OZ_sCloneAllocBlock(size,_tuple);
  OZ_sCloneTerm(_index);
  OZ_sCloneTerm(_value);
}

void FSetSelectPropagator::gCollect(void) {
  _tuple = OZ_gCollectAllocBlock(size,_tuple);
  OZ_gCollectTerm(_index);
  OZ_gCollectTerm(_value);
}

class FSetSelectExpect: public OZ_Expect {
public:
  OZ_expect_t expectFD(OZ_Term t) {
    return expectIntVar(t,fd_prop_any);
  }
  OZ_expect_t expectFS(OZ_Term t) {
    return expectFSetVar(t,fs_prop_bounds);
  }
  OZ_expect_t expectFSVector(OZ_Term v) {
    return expectVector(v,(OZ_ExpectMeth) &FSetSelectExpect::expectFS);
  }
};

OZ_BI_define(set_select,3,0)
{
  OZ_EXPECTED_TYPE(OZ_EM_VECT OZ_EM_FSET ","
		   OZ_EM_FD "," OZ_EM_FSET);
  
  FSetSelectExpect pe;
  OZ_EXPECT(pe,0,expectFSVector);
  OZ_EXPECT(pe,1,expectFD);
  int dummy;
  OZ_EXPECT_SUSPEND(pe,2,expectFS,dummy);
  if (OZ_vectorSize(OZ_in(0))==0) return pe.fail();
  return pe.impose(new FSetSelectPropagator(OZ_in(0),OZ_in(1),OZ_in(2)));
}
OZ_BI_end

OZ_Return FSetSelectPropagator::propagate(void)
{
  OZ_FDIntVar index(_index);

  if (!initialized) {
    initialized = OZ_TRUE;
    if ((*index <= size)==0 || (*index >= 1)==0) {
      index.fail();
      return OZ_FAILED;
    }
  }

  OZ_FSetVar  tuple[size];
  OZ_FSetVar  value(_value);
  OZ_FDIntVar_Snapshot snap_index;
  OZ_FSetVar_Snapshot  snap_value;
  OZ_FSetVar_Iterator tuple_iter(size,tuple);

  tuple_iter.read(_tuple);

  // drop all variables that no longer correspond to indices in
  // the domain of index

  for (int i=size;i--;) {
    OZ_Term t = _tuple[i];
    if (t) {
      if (! index->isIn(i+1)) {
	tuple[i].dropParameter();
	_tuple[i]=1;
      }
    }
  }

  // iterate until fix point
  do {

    // if only one choice left, unify with it
    if (*index == fd_singl) {
      int i = index->getSingleElem()-1;
      tuple_iter.leave(_tuple);
      index.leave();
      value.leave();
      return replaceBy(_value,_tuple[i]);
    }

    // take snap shots
    snap_index = index;
    snap_value = value;

    // discard all choices that are inconsistent
    // and at the same time compute the value's upper
    // approximation by unioning all remaining choices, and
    // also its lower approximation by intersecting them.

    OZ_FSetConstraint upper(fs_empty),lower(fs_full);
    OZ_FSetConstraint zvalue = *value;
    // also keep track of cardinality approximation
    int lo = zvalue.getCardMax();
    int hi = zvalue.getCardMin();

    for (int i=index->getMinElem();i>0;i=index->getNextLargerElem(i)) {
      OZ_FSetConstraint zchoice = *tuple[i-1];
      if ((zvalue - zchoice).getKnownIn() > 0 ||
	  (zchoice - zvalue).getKnownIn() > 0 ||
	  (zchoice.getCardMin() > zvalue.getCardMax()) ||
	  (zchoice.getCardMax() < zvalue.getCardMin()))
	{
	  if ((*index -= i)==0) goto failed;
	  tuple[i-1].dropParameter();
	  _tuple[i-1]=1;
	}
      else {
	upper = upper | zchoice;
	lower = lower & zchoice;
	lo    = min(lo,zchoice.getCardMin());
	hi    = max(hi,zchoice.getCardMax());
      }
    }

    // now enforce that value must be between its approximations
    if (!(*value <= upper) || !(*value >= lower)) goto failed;
    // also enforce the cardinality approximations
    if (!(value->putCard(lo,hi))) goto failed;
  }
  // until nothing has changed
  while (snap_index!=index || snap_value!=value);

  // now we must suspend.  If we had succeeded, it would have been
  // noticed at the front of the do loop.
  tuple_iter.leave(_tuple);
  index.leave();
  value.leave();
  return OZ_SLEEP;

 failed:
  tuple_iter.fail(_tuple);
  index.fail();
  value.fail();
  return OZ_FAILED;
}

OZ_BI_proto(set_select);

OZ_C_proc_interface *oz_init_module(void)
{
  static OZ_C_proc_interface table[] = {
    {"select",3,0,set_select},
    {0,0,0,0}
  };
  return table;
}

char oz_module_name[] = "SelectFS";

