package de.uni_sb.ps.dml.runtime;

public class Tuple3 implements DMLTuple {

    protected DMLValue fst = null;
    protected DMLValue snd = null;
    protected DMLValue thr = null;

    public Tuple3(DMLValue eins,
		  DMLValue zwei,
		  DMLValue drei) {
	fst=eins;
	snd=zwei;
	thr=drei;
    }

    public DMLValue get0() { return fst; }
    public DMLValue get1() { return snd; }
    public DMLValue get2() { return thr; }
    public DMLValue get3() { throw new ArrayIndexOutOfBoundsException(); }
    public DMLValue get4() { throw new ArrayIndexOutOfBoundsException(); }

    final public boolean equals(Object val) {
	return (val instanceof Tuple3) &&
	    fst.equals(((Tuple3) val).fst) &&
	    snd.equals(((Tuple3) val).snd) &&
	    thr.equals(((Tuple3) val).thr);
    }

    public String toString() {
	return "("+fst+", "+snd+", "+thr+")";
    }

    /** gibt den i-ten Eintrag des Tuples oder Records*/
    final public DMLValue getByIndex(int i){
	switch (i) {
	case 0: return fst;
	case 1: return snd;
	case 2: return thr;
	default: throw new ArrayIndexOutOfBoundsException();
	}
    }

    /** gibt die Stelligkeit des Tuples oder Records an */
    final public int getArity() {
	return 3;
    }

    final public DMLValue[] getVals() {
	DMLValue[] vals = {fst,snd,thr};
	return vals;
    }

    final public DMLValue getValue() {
	return this;
    }

    final public DMLValue request() {
	return this;
    }

    final public DMLValue apply(DMLValue val) {
	return DMLConstants.runtimeError.apply(new DMLString("cannot apply "+this+" to "+val)).raise();
    }

    final public DMLValue raise() {
	throw new DMLExceptionWrapper(this);
    }
}
