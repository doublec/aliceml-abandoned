#include "mozart_cpi_extra.hh"

//static int seq_counter = 0;

class SeqUnionPropagator : public OZ_Propagator {
private:
  static OZ_PropagatorProfile profile;
  OZ_Term * _tuple;
  OZ_Term   _value;
  int size;
  //  int _cnt;
public:
  SeqUnionPropagator(OZ_Term tuple,OZ_Term value)
    : _value(value),size(OZ_vectorSize(tuple))//,_cnt(-1)
  {
    _tuple = OZ_hallocOzTerms(size);
    OZ_getOzTermVector(tuple,_tuple);
  }
  virtual OZ_Return propagate(void);
  virtual size_t sizeOf() { return sizeof(SeqUnionPropagator); }
  virtual OZ_PropagatorProfile * getProfile(void) const { return &profile; }
  virtual OZ_Term getParameters(void) const;
  virtual void sClone(void);
  virtual void gCollect(void);
};

OZ_PropagatorProfile SeqUnionPropagator::profile = "Select.sequnion";

OZ_Term SeqUnionPropagator::getParameters(void) const
{
  OZ_Term lst = OZ_nil();
  for (int i=size; i--;) {
    OZ_Term t = _tuple[i];
    if (t==0 || t==1) t=OZ_unit();
    lst=OZ_cons(t,lst);
  }
  return OZ_cons(_value,lst);
}

void SeqUnionPropagator::sClone(void) {
  _tuple = OZ_sCloneAllocBlock(size,_tuple);
  OZ_sCloneTerm(_value);
  //_cnt += 10;
}

void SeqUnionPropagator::gCollect(void) {
  _tuple = OZ_gCollectAllocBlock(size,_tuple);
  OZ_gCollectTerm(_value);
}

OZ_BI_define(set_sequnion,2,0)
{
  OZ_EXPECTED_TYPE(OZ_EM_VECT OZ_EM_FSET "," OZ_EM_FSET);
  SelectExpect pe;
  int dummy;
  OZ_EXPECT_SUSPEND(pe,0,expectFSVector,dummy);
  OZ_EXPECT_SUSPEND(pe,1,expectFS,      dummy);
  return pe.impose(new SeqUnionPropagator(OZ_in(0),OZ_in(1)));
}
OZ_BI_end

// ===================================================================
// S = <U[S1 ... Sn]
//
// ==<	\subsetq
// >==	\supseteq
// >>	\prec
// <<	\succ
// =<	\leq
// >=	\geq
//
// 1. S ==< U{ hi(Sk) | 1=<k=<n }
// 2. S >== U{ lo(Sk) | 1=<k=<n }
// 3. Si ==< hi(S) \ U{ lo(Sk) | k!=i }
// 4. Si >== lo(S) \ U{ hi(Sk) | k!=i }
// 5. Si >> lo(S) \ U{ hi(Sk) | k>=i }
// 6. Si << lo(S) \ U{ hi(Sk) | k=<i }
// 7. |S| =< +{ hi(|Sk|) | 1=<k=<n }
// 8. |S| >= +{ lo(|Sk|) } 1=<k=<n }
// 9. |Si| =< hi(|S|) - +{ lo(|Sk|) | k!=i }
// 10. |Si| >= lo(|S|) - +{ hi(|Sk|) | k!=i }
// ===================================================================
//#include <unistd.h>

static inline int FSetGetLowerBoundOfMax(OZ_FSetConstraint& c)
{
  int i = c.getCardMin();
  if (i==0) return c.getGlbMaxElem();
  int n = c.getLubMinElem();
  while (--i) n=c.getLubNextLargerElem(n);
  return max(n,c.getGlbMaxElem());
}

// Similarly for the min element

static inline int FSetGetUpperBoundOfMin(OZ_FSetConstraint& c)
{
  int i = c.getCardMin();
  if (i==0) return c.getGlbMinElem();
  int n = c.getLubMaxElem();
  while (--i) n=c.getLubNextSmallerElem(n);
  int m = c.getGlbMinElem();
  return (m>=0)?min(n,m):n;
}

OZ_Return SeqUnionPropagator::propagate(void)
{
  //if (_cnt<0) _cnt = seq_counter++;
  //fprintf(stderr,"[%d] ENTER: %s\n",_cnt,OZ_toC(getParameters(),100,100));
  OZ_FSetVar value(_value);
  OZ_FSetConstraint& S = *value;

  if (size==0) {
    if (!(S <= fs_empty)) {
      value.fail();
      return OZ_FAILED;
    } else {
      value.leave();
      return OZ_ENTAILED;
    }
  }

  OZ_FSetVar tuple[size];
  for(int i=size;i--;) tuple[i].read(_tuple[i]);

  OZ_FSetValue elts_lo[size];
  OZ_FSetValue elts_hi[size];
  OZ_FSetValue elts_above_lo[size];
  OZ_FSetValue elts_above_hi[size];
  OZ_FSetValue elts_below_lo[size];
  OZ_FSetValue elts_below_hi[size];
  OZ_FSetValue elts_eqabove_hi[size];
  OZ_FSetValue elts_eqbelow_hi[size];
  int card_below_lo[size];
  int card_below_hi[size];
  int card_above_lo[size];
  int card_above_hi[size];

  OZ_FSetVar_Snapshot snap_value;
  OZ_FSetVar_Snapshot snap_tuple[size];

  // the following still does not implement the improved stuff of FS.int.seq

 loop:
  {
    // take snapshots
    snap_value = value;
    for(int i=size;i--;) snap_tuple[i] = tuple[i];
    
    OZ_FSetValue ELTS_ABOVE_LO(fs_empty);
    OZ_FSetValue ELTS_ABOVE_HI(fs_empty);
    int CARD_ABOVE_LO = 0;
    int CARD_ABOVE_HI = 0;
    int SUP = OZ_getFSetSup()+1;
    int UB_MIN = SUP;

    for (int i=size; i--;) {
      OZ_FSetConstraint& Si = *tuple[i];
      elts_above_lo[i] = ELTS_ABOVE_LO;
      elts_above_hi[i] = ELTS_ABOVE_HI;
      elts_lo[i] = Si.getGlbSet();
      elts_hi[i] = Si.getLubSet();
      ELTS_ABOVE_LO |= elts_lo[i];
      ELTS_ABOVE_HI |= elts_hi[i];
      elts_eqabove_hi[i] = ELTS_ABOVE_HI;
      card_above_lo[i] = CARD_ABOVE_LO;
      card_above_hi[i] = CARD_ABOVE_HI;
      CARD_ABOVE_LO += Si.getCardMin();
      CARD_ABOVE_HI += Si.getCardMax();
      if (i<(size-1) && UB_MIN!=SUP && !(Si.le(UB_MIN-1))) goto failed;
      int ub_min = FSetGetUpperBoundOfMin(Si);
      if (ub_min>=0) UB_MIN=min(UB_MIN,ub_min);
    }

    if (!(S <= ELTS_ABOVE_HI) ||
	!(S >= ELTS_ABOVE_LO) ||
	!S.putCard(CARD_ABOVE_LO,CARD_ABOVE_HI))
      goto failed;

    OZ_FSetValue ELTS_BELOW_LO(fs_empty);
    OZ_FSetValue ELTS_BELOW_HI(fs_empty);
    int CARD_BELOW_LO = 0;
    int CARD_BELOW_HI = 0;
    int LB_MAX = -1;

    for (int i=0;i<size;i++) {
      elts_below_lo[i] = ELTS_BELOW_LO;
      elts_below_hi[i] = ELTS_BELOW_HI;
      ELTS_BELOW_LO |= elts_lo[i];
      ELTS_BELOW_HI |= elts_hi[i];
      elts_eqbelow_hi[i] = ELTS_BELOW_HI;
      card_below_lo[i] = CARD_BELOW_LO;
      card_below_hi[i] = CARD_BELOW_HI;
      OZ_FSetConstraint& Si = *tuple[i];
      CARD_BELOW_LO += Si.getCardMin();
      CARD_BELOW_HI += Si.getCardMax();
      if (i>0 && LB_MAX!=-1 && !(Si.ge(LB_MAX+1))) goto failed;
      int lb_max = FSetGetLowerBoundOfMax(Si);
      LB_MAX=max(LB_MAX,lb_max);
    }

    OZ_FSetValue S_LO = S.getGlbSet();
    OZ_FSetValue S_HI = S.getLubSet();
    int S_CARD_LO = S.getCardMin();
    int S_CARD_HI = S.getCardMax();

    for (int i=size;i--;) {
      OZ_FSetConstraint& Si = *tuple[i];
      if (!(Si <= (S_HI - (elts_below_lo[i] | elts_above_lo[i]))) ||
	  !(Si >= (S_LO - (elts_below_hi[i] | elts_above_hi[i]))) ||
	  !Si.putCard(max(0,S_CARD_LO - card_below_hi[i] - card_above_hi[i]),
		      max(0,S_CARD_HI - card_below_lo[i] - card_above_lo[i])))
	goto failed;
      int smaller = (S_LO - elts_eqabove_hi[i]).getMaxElem();
      if (smaller>=0 && !Si.ge(smaller+1)) goto failed;
      int greater = (S_LO - elts_eqbelow_hi[i]).getMinElem();
      if (greater>0 && !Si.le(greater-1)) goto failed;
    }

    if (snap_value!=value) goto loop;
    for(int i=size;i--;) if (snap_tuple[i]!=tuple[i]) goto loop;
  }

  {
    OZ_Boolean hasVars;
    hasVars = value.leave();
    for(int i=size;i--;) if (tuple[i].leave()) hasVars=OZ_TRUE;
    //fprintf(stderr,"[%d] %s: %s\n",_cnt,(hasVars)?"SLEEP":"ENTAILED",OZ_toC(getParameters(),100,100));
    return (hasVars)?OZ_SLEEP:OZ_ENTAILED;
  }

 failed:
  value.fail();
  for(int i=size;i--;) tuple[i].fail();
  //fprintf(stderr,"[%d] FAIL: %s\n",_cnt,OZ_toC(getParameters(),100,100));
  return OZ_FAILED;
}

OZ_BI_proto(set_sequnion);

//OZ_BI_define(set_bugreset,0,0)
//{
//  seq_counter=0;
//  return PROCEED;
//}
//OZ_BI_end

OZ_C_proc_interface *oz_init_module(void)
{
  static OZ_C_proc_interface table[] = {
    {"sequnion",2,0,set_sequnion},
    //{"bugreset",0,0,set_bugreset},
    {0,0,0,0}
  };
  return table;
}

char oz_module_name[] = "SeqUnion";
