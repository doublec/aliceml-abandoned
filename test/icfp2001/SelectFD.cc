#include "mozart_cpi_extra.hh"

class FDSelectPropagator : public OZ_Propagator {
private:
  static OZ_PropagatorProfile profile;
  OZ_Term * _tuple;
  OZ_Term   _index;
  OZ_Term   _value;
  int size;
  OZ_Boolean initialized;
public:
  FDSelectPropagator(OZ_Term tuple,OZ_Term index,OZ_Term value)
    : _index(index),_value(value),size(OZ_vectorSize(tuple)),
      initialized(OZ_FALSE)
  {
    _tuple = OZ_hallocOzTerms(size);
    OZ_getOzTermVector(tuple,_tuple);
  }
  virtual OZ_Return propagate(void);
  virtual size_t sizeOf() { return sizeof(FDSelectPropagator); }
  virtual OZ_PropagatorProfile *getProfile(void) const { return &profile; }
  virtual OZ_Term getParameters(void) const;
  virtual void sClone(void);
  virtual void gCollect(void);
};

OZ_PropagatorProfile FDSelectPropagator::profile;

OZ_Term FDSelectPropagator::getParameters(void) const
{
  OZ_Term lst = OZ_nil();
  for (int i=size;i--;) {
    OZ_Term t = _tuple[i];
    if (t==0 || t==1) t=OZ_unit();
    lst=OZ_cons(t,lst);
  }
  return OZ_cons(lst,OZ_cons(_index,OZ_cons(_value,OZ_nil())));
}

void FDSelectPropagator::sClone(void)
{
  _tuple = OZ_sCloneAllocBlock(size,_tuple);
  OZ_sCloneTerm(_index);
  OZ_sCloneTerm(_value);
}

void FDSelectPropagator::gCollect(void)
{
  _tuple = OZ_gCollectAllocBlock(size,_tuple);
  OZ_gCollectTerm(_index);
  OZ_gCollectTerm(_value);
}

class FDSelectExpect : public OZ_Expect {
public:
  OZ_expect_t expectFD(OZ_Term t) {
    return expectIntVar(t,fd_prop_any);
  }
  OZ_expect_t expectFDVector(OZ_Term v) {
    return expectVector(v,(OZ_ExpectMeth) &FDSelectExpect::expectFD);
  }
};

OZ_BI_define(fd_select,3,0)
{
  OZ_EXPECTED_TYPE(OZ_EM_VECT OZ_EM_FD ","
		   OZ_EM_FD "," OZ_EM_FD);
  FDSelectExpect pe;
  OZ_EXPECT(pe,0,expectFDVector);
  int dummy;
  OZ_EXPECT_SUSPEND(pe,1,expectFD,dummy);
  OZ_EXPECT_SUSPEND(pe,2,expectFD,dummy);
  return pe.impose(new FDSelectPropagator(OZ_in(0),OZ_in(1),OZ_in(2)));
}
OZ_BI_end

#define FailOnEmpty(X) if((X) == 0) goto failure;

OZ_Return FDSelectPropagator::propagate(void)
{
  OZ_FDIntVar index(_index);

  if (!initialized) {
    initialized = OZ_TRUE;
    if ((*index <= size)==0 || (*index >= 1)==0) {
      index.fail();
      return OZ_FAILED;
    }
  }

  OZ_FDIntVar tuple[size];
  OZ_FDIntVar_Iterator tuple_iter(size,tuple);
  OZ_FDIntVar value(_value);

  tuple_iter.read(_tuple);

  OZ_FiniteDomain index_dom(fd_empty);
  OZ_FiniteDomain value_dom(fd_empty);

  for (int i=size;i--;) {
    OZ_Term t = _tuple[i];
    if (t) {
      if (! index->isIn(i+1)) goto dropvar;
      {
	OZ_FiniteDomain dom = *(tuple[i]) & *value;
	if (dom==fd_empty) goto dropvar;
	index_dom += i+1;
	value_dom = value_dom|dom;
      }
    }
    continue;
  dropvar:
    tuple[i].dropParameter();
    _tuple[i]=1;
  }

  FailOnEmpty(*index &= index_dom);
  FailOnEmpty(*value &= value_dom);

  {
    int i = (index->getSize()==1)?index->getSingleElem()-1:-1;
    tuple_iter.leave(_tuple);
    index.leave();
    value.leave();
    return (i>=0)
      ? replaceBy(_value,_tuple[i])
    : OZ_SLEEP;
  }

 failure:
  tuple_iter.fail(_tuple);
  index.fail();
  value.fail();
  return OZ_FAILED;
}

OZ_BI_proto(fd_select);

OZ_C_proc_interface *oz_init_module(void)
{
  static OZ_C_proc_interface table[] = {
    {"select",3,0,fd_select},
    {0,0,0,0}
  };
  return table;
}

char oz_module_name[] = "SelectFD";
