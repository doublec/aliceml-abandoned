#include "mozart_cpi_extra.hh"

class InterSelectPropagator : public OZ_Propagator {
private:
  static OZ_PropagatorProfile profile;
  OZ_Term * _tuple;
  OZ_Term   _index;
  OZ_Term   _value;
  int size;
  OZ_Boolean initialized;
public:
  InterSelectPropagator(OZ_Term tuple,OZ_Term index,OZ_Term value)
    : _index(index),_value(value),size(OZ_vectorSize(tuple)),
      initialized(OZ_FALSE)
  {
    _tuple = OZ_hallocOzTerms(size);
    OZ_getOzTermVector(tuple,_tuple);
  }
  virtual OZ_Return propagate(void);
  virtual size_t sizeOf() { return sizeof(InterSelectPropagator); }
  virtual OZ_PropagatorProfile *getProfile(void) const { return &profile; }
  virtual OZ_Term getParameters(void) const;
  virtual void sClone(void);
  virtual void gCollect(void);
};

OZ_PropagatorProfile InterSelectPropagator::profile = "Select.inter";

OZ_Term InterSelectPropagator::getParameters(void) const
{
  OZ_Term lst = OZ_nil();
  for (int i=size;i--;) {
    OZ_Term t = _tuple[i];
    if (t==0 || t==1) t=OZ_unit();
    lst=OZ_cons(t,lst);
  }
  return OZ_cons(lst,OZ_cons(_index,OZ_cons(_value,OZ_nil())));
}

void InterSelectPropagator::sClone(void) {
  _tuple = OZ_sCloneAllocBlock(size,_tuple);
  OZ_sCloneTerm(_index);
  OZ_sCloneTerm(_value);
}

void InterSelectPropagator::gCollect(void) {
  _tuple = OZ_gCollectAllocBlock(size,_tuple);
  OZ_gCollectTerm(_index);
  OZ_gCollectTerm(_value);
}

class InterSelectExpect: public OZ_Expect {
public:
  OZ_expect_t expectFS(OZ_Term t) {
    return expectFSetVar(t,fs_prop_bounds);
  }
  OZ_expect_t expectFSVector(OZ_Term v) {
    return expectVector(v,(OZ_ExpectMeth) &InterSelectExpect::expectFS);
  }
};

OZ_BI_define(set_select_inter,3,0)
{
  OZ_EXPECTED_TYPE(OZ_EM_VECT OZ_EM_FSET ","
		   OZ_EM_FSET "," OZ_EM_FSET);
  InterSelectExpect pe;
  OZ_EXPECT(pe,0,expectFSVector);
  int dummy;
  OZ_EXPECT_SUSPEND(pe,1,expectFS,dummy);
  OZ_EXPECT_SUSPEND(pe,2,expectFS,dummy);
  if (OZ_vectorSize(OZ_in(0))==0) return pe.fail();
  return pe.impose(new InterSelectPropagator(OZ_in(0),OZ_in(1),OZ_in(2)));
}
OZ_BI_end

OZ_Return InterSelectPropagator::propagate(void)
{
  OZ_FSetVar index(_index);

  // initialize, i.e. properly constrain index

  if (!initialized) {
    initialized = OZ_TRUE;
    if ((index->le(size))==0 || (index->ge(1))==0) {
      index.fail();
      return OZ_FAILED;
    }
  }

  OZ_FSetVar value(_value);
  OZ_FSetVar tuple[size];
  OZ_FSetVar_Iterator tuple_iter(size,tuple);
  OZ_FSetVar_Snapshot snap_index;
  OZ_FSetVar_Snapshot snap_value;
  OZ_FSetConstraint& zindex = *index;

  tuple_iter.read(_tuple);

  // drop all variables that no longer correspond to indices in
  // the set of indices

  for (int i=size;i--;) {
    OZ_Term t = _tuple[i];
    if (t) {
      if (zindex.isNotIn(i+1)) {
	tuple[i].dropParameter();
	_tuple[i]=1;
      }
    }
  }

  // iterate until fix point
  OZ_FSetValue before[size],sofarBefore,sofarAfter;
  OZ_FSetConstraint& zvalue = *value;

  do {

    // take snap shots
    snap_index = index;
    snap_value = value;

    sofarBefore = fs_full;

    for (int i=size;i--;) {

      // skip ints not in index, i.e. already dropped
      {
	OZ_Term t = _tuple[i];
	if (t==0 || t==1) continue;
      }

      OZ_FSetConstraint& zcandidate = *tuple[i];

      // check if zcandidate is inconsistent with zvalue, i.e.
      // if there is some int in value and not in candidate
      if (// is candidate too small?
	  (zcandidate.getCardMax() < zvalue.getCardMin()) ||
	  // does value contain an elt not in candidate?
	  (zcandidate.getNotInSet() & zvalue.getGlbSet()).getCard() > 0)
	{
	  // drop that position
	  if (!(zindex -= i+1)) goto failed;
	  tuple[i].dropParameter();
	  _tuple[i]=1;
	}
      else {
	// if position is known to be selected
	if (zindex.isIn(i+1)) {
	  // value must be a subset of candidate
	  // and candidate a superset of value
	  if (!(zvalue <= zcandidate) || !(zcandidate >= zvalue))
	    goto failed;
	}
	before[i]=sofarBefore;
	sofarBefore &= zcandidate.getGlbSet();
      }
    }

    // if the selector is empty, then the value must be empty
    // we check this here because positions may have been dropped
    // above
    if (zindex.getCardMax()==0) {
      if (!(zvalue <= fs_empty)) goto failed;
      tuple_iter.leave(_tuple);
      index.leave();
      value.leave();
      return OZ_ENTAILED;
    }

    // sofarBefore is the smallest set
    if (!(zvalue >= sofarBefore)) goto failed;

    // process sequence again, in reverse order to compute
    // sofarAfter. before[i] is the smallest set that can be
    // obtained by intersecting non dropped sets at earlier
    // positions. sofarAfter is the same for later positions.
    // (before[i] - sofarAfter) - zvalue.getLubSet()
    // is the set of unexplained values that _must_ be
    // removed here.

    sofarAfter = fs_full;

    {
      OZ_FSetValue zvalLub = zvalue.getLubSet();
      for (int i=size;i--;) {
	if (sofarAfter.getCard()==0) break;
	// skip ints not in index, i.e. already dropped
	{
	  OZ_Term t = _tuple[i];
	  if (t==0 || t==1) continue;
	}
	OZ_FSetValue Extra = (before[i] & sofarAfter) - zvalLub;
	OZ_FSetConstraint& zcandidate = *tuple[i];
	if (Extra.getCard()>0) {
	  if (!(zindex += i+1) || !(zcandidate != Extra)) goto failed;
	}
	sofarAfter &= zcandidate;
      }
    }
  }
  // until nothing has changed
  while (snap_index!=index || snap_value!=value);

  {
    OZ_Boolean has_vars = OZ_FALSE;
    has_vars |= tuple_iter.leave(_tuple);
    has_vars |= index.leave();
    has_vars |= value.leave();
    return (has_vars)?OZ_SLEEP:OZ_ENTAILED;
  }

 failed:
  tuple_iter.fail(_tuple);
  index.fail();
  value.fail();
  return OZ_FAILED;
}

OZ_BI_proto(set_select_inter);

OZ_C_proc_interface *oz_init_module(void)
{
  static OZ_C_proc_interface table[] = {
    {"inter",3,0,set_select_inter},
    {0,0,0,0}
  };
  return table;
}

char oz_module_name[] = "SelectInter";
