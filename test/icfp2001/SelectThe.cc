#include "mozart_cpi_extra.hh"

class SelectThePropagator : public OZ_Propagator {
private:
  static OZ_PropagatorProfile profile;
  OZ_Term _s;
  OZ_Term _i;
public:
  SelectThePropagator(OZ_Term s,OZ_Term i) : _s(s),_i(i) {}
  virtual OZ_Return propagate(void);
  virtual size_t sizeOf() { return sizeof(SelectThePropagator); }
  virtual OZ_PropagatorProfile *getProfile(void) const { return &profile; }
  virtual OZ_Term getParameters(void) const;
  virtual void sClone(void);
  virtual void gCollect(void);
};

OZ_PropagatorProfile SelectThePropagator::profile = "Select.the";

OZ_Term SelectThePropagator::getParameters(void) const
{
  return OZ_cons(_s,OZ_cons(_i,OZ_nil()));
}

void SelectThePropagator::sClone(void)
{
  OZ_sCloneTerm(_s);
  OZ_sCloneTerm(_i);
}

void SelectThePropagator::gCollect(void)
{
  OZ_gCollectTerm(_s);
  OZ_gCollectTerm(_i);
}

OZ_BI_define(select_the,2,0)
{
  OZ_EXPECTED_TYPE(OZ_EM_FSET "," OZ_EM_FD);
  SelectExpect pe;
  int dummy;
  OZ_EXPECT_SUSPEND(pe,0,expectFS,dummy);
  OZ_EXPECT_SUSPEND(pe,1,expectFD,dummy);
  return pe.impose(new SelectThePropagator(OZ_in(0),OZ_in(1)));
}
OZ_BI_end

OZ_Return SelectThePropagator::propagate(void)
{
  OZ_FSetVar s(_s);
  OZ_FDIntVar i(_i);

  if (!s->putCard(1,1)) goto failure;

  {
    OZ_FiniteDomain not_in(s->getNotInSet());
    if (!(*i -= not_in)) goto failure;
  }

  {
    OZ_FSetValue is(*i);
    if (!(*s <= is)) goto failure;
  }

  {
    OZ_Boolean has_vars = s.leave();
    has_vars |= i.leave();
    return (has_vars)?OZ_SLEEP:OZ_ENTAILED;
  }

 failure:
  s.fail();
  i.fail();
  return OZ_FAILED;
}

OZ_BI_proto(select_the);

OZ_C_proc_interface *oz_init_module(void)
{
  static OZ_C_proc_interface table[] = {
    {"the",2,0,select_the},
    {0,0,0,0}
  };
  return table;
}

char oz_module_name[] = "SelectThe";

