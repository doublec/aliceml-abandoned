#include "mozart_cpi_extra.hh"

class UnionSelectPropagator : public OZ_Propagator {
private:
  static OZ_PropagatorProfile profile;
  OZ_Term * _tuple;
  OZ_Term   _index;
  OZ_Term   _value;
  int size;
  OZ_Boolean initialized;
public:
  UnionSelectPropagator(OZ_Term tuple,OZ_Term index,OZ_Term value)
    : _index(index),_value(value),size(OZ_vectorSize(tuple)),
      initialized(OZ_FALSE)
  {
    _tuple = OZ_hallocOzTerms(size);
    OZ_getOzTermVector(tuple,_tuple);
  }
  virtual OZ_Return propagate(void);
  virtual size_t sizeOf() { return sizeof(UnionSelectPropagator); }
  virtual OZ_PropagatorProfile *getProfile(void) const { return &profile; }
  virtual OZ_Term getParameters(void) const;
  virtual void sClone(void);
  virtual void gCollect(void);
};

OZ_PropagatorProfile UnionSelectPropagator::profile = "Select.union";

OZ_Term UnionSelectPropagator::getParameters(void) const
{
  OZ_Term lst = OZ_nil();
  for (int i=size;i--;) {
    OZ_Term t = _tuple[i];
    if (t==0 || t==1) t=OZ_unit();
    lst=OZ_cons(t,lst);
  }
  return OZ_cons(lst,OZ_cons(_index,OZ_cons(_value,OZ_nil())));
}

void UnionSelectPropagator::sClone(void) {
  _tuple = OZ_sCloneAllocBlock(size,_tuple);
  OZ_sCloneTerm(_index);
  OZ_sCloneTerm(_value);
}

void UnionSelectPropagator::gCollect(void) {
  _tuple = OZ_gCollectAllocBlock(size,_tuple);
  OZ_gCollectTerm(_index);
  OZ_gCollectTerm(_value);
}

class UnionSelectExpect: public OZ_Expect {
public:
  OZ_expect_t expectFS(OZ_Term t) {
    return expectFSetVar(t,fs_prop_bounds);
  }
  OZ_expect_t expectFSVector(OZ_Term v) {
    return expectVector(v,(OZ_ExpectMeth) &UnionSelectExpect::expectFS);
  }
};

OZ_BI_define(set_select_union,3,0)
{
  OZ_EXPECTED_TYPE(OZ_EM_VECT OZ_EM_FSET ","
		   OZ_EM_FSET "," OZ_EM_FSET);
  UnionSelectExpect pe;
  OZ_EXPECT(pe,0,expectFSVector);
  int dummy;
  OZ_EXPECT_SUSPEND(pe,1,expectFS,dummy);
  OZ_EXPECT_SUSPEND(pe,2,expectFS,dummy);
  if (OZ_vectorSize(OZ_in(0))==0) return pe.fail();
  return pe.impose(new UnionSelectPropagator(OZ_in(0),OZ_in(1),OZ_in(2)));
}
OZ_BI_end

OZ_Return UnionSelectPropagator::propagate(void)
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
  OZ_FSetValue above[size];
  OZ_FSetValue upper;
  OZ_FSetConstraint& zvalue = *value;

  do {

    // take snap shots
    snap_index = index;
    snap_value = value;

    upper = fs_empty;

    for (int i=size;i--;) {

      // skip ints not in index, i.e. already dropped
      {
	OZ_Term t = _tuple[i];
	if (t==0 || t==1) continue;
      }

      OZ_FSetConstraint& zcandidate = *tuple[i];

      // check if zcandidate is inconsistent with zvalue, i.e.
      // if there is some int in candidate and not in value
      if ((zvalue.getCardMax() < zcandidate.getCardMin()) ||
	  (zvalue.getNotInSet() & zcandidate.getGlbSet()).getCard() > 0)
	{
	  // drop that position
	  if (!(zindex -= i+1)) goto failed;
	  tuple[i].dropParameter();
	  _tuple[i]=1;
	}
      else {
	// if position is known to be selected
	if (zindex.isIn(i+1)) {
	  // candidate set must be a subset of result
	  // and result a superset of candidate
	  if (!(zcandidate <= zvalue) ||
	      !(zvalue >= zcandidate)) goto failed;
	}
	// record upper bound above position i
	above[i] = upper;
	// update upper bound with candidate's upper bound
	upper |= zcandidate.getLubSet();
      }
    }

    // result must be subset of upper bound
    if (!(zvalue <= upper)) goto failed;

    // now we process the sequence again, but this time
    // upwards.  we keep track of the values known to be
    // in the result and that have not yet been accounted
    // for.

    OZ_FSetValue not_accounted_for = zvalue.getGlbSet();

    for (int j=size;j--;) {
      // if all of the result has been accounted for
      // we can stop early
      if (not_accounted_for == fs_empty) break;
      int i = size-j-1;
      // skip dropped positions
      {
	OZ_Term t = _tuple[i];
	if (t==0 || t==1) continue;
      }
      OZ_FSetConstraint& here = *tuple[i];
      // elements still to be accounted for, and that cannot be
      // accounted for by the sets at positions above i, must be
      // accounted for here
      OZ_FSetValue must_be_here = not_accounted_for - above[i];
      if (!(must_be_here == fs_empty)) {
	// in which case, i must be a selected position, and
	// the set here must at least contain the elements that
	// need to be accounted for here
	if (!(zindex += i+1) || !(here >= must_be_here)) goto failed;
      }
      // remove what might be accounted for by the set here
      not_accounted_for = not_accounted_for - here.getLubSet();
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

OZ_BI_proto(set_select_union);

OZ_C_proc_interface *oz_init_module(void)
{
  static OZ_C_proc_interface table[] = {
    {"union",3,0,set_select_union},
    {0,0,0,0}
  };
  return table;
}

char oz_module_name[] = "SelectUnion";
