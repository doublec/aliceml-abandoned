/*
 * Author: 
 *      Daniel Simon, <dansim@ps.uni-sb.de>
 * 
 * Copyright:
 *      Daniel Simon, 1999
 *
 * Last change:
 *    $Date$ by $Author$
 * $Revision$
 * 
 */
package de.uni_sb.ps.dml.runtime;

/** Special class for tuples with arity 4.
 */
final public class Tuple4 implements DMLTuple {

    protected DMLValue fst;
    protected DMLValue snd;
    protected DMLValue thr;
    protected DMLValue fur;

    public Tuple4() {}

    public Tuple4(DMLValue eins,
		  DMLValue zwei,
		  DMLValue drei,
		  DMLValue vier) {
	fst=eins;
	snd=zwei;
	thr=drei;
	fur=vier;
    }

    public DMLValue get0() { return fst; }
    public DMLValue get1() { return snd; }
    public DMLValue get2() { return thr; }
    public DMLValue get3() { return fur; }
    public DMLValue get4() { throw new ArrayIndexOutOfBoundsException(); }

    final public void setContent(DMLValue eins,
			DMLValue zwei,
			DMLValue drei,
			DMLValue vier) {
	if (fst == null) {
	    fst=eins;
	    snd=zwei;
	    thr=drei;
	    fur=vier;
	} else {
	    try {
		_RAISE(runtimeError, new STRING ("cannot set content twice"));
	    } catch (java.rmi.RemoteException r) {
		System.err.println(r);
		r.printStackTrace();
	    }
	}
    }

    /** Gleichheit der  und Inhalte */
    final public boolean equals(java.lang.Object val) {
	if (val instanceof DMLTuple) {
	    if (val instanceof Record) {
		return false;
	    } else if (val instanceof Tuple4) {
		Tuple4 v = (Tuple4) val;
		return
		    fst.equals(v.fst) &&
		    snd.equals(v.snd) &&
		    thr.equals(v.thr) &&
		    fur.equals(v.fur);
	    } else {
		DMLTuple t = (DMLTuple) val;
		if (t.getArity()!=4) {
		    return false;
		} else {
		    return
			t.get0().equals(fst) &&
			t.get1().equals(snd) &&
			t.get2().equals(thr) &&
			t.get3().equals(fur);
		}
	    }
	} else {
	    return false;
	}
    }

    final public java.lang.String toString() {
	return "("+fst+", "+snd+", "+thr+", "+fur+")/4";
    }

    /** gibt den i-ten Eintrag des Tuples oder Records*/
    final public DMLValue get(int i){
	switch (i) {
	case 0: return fst;
	case 1: return snd;
	case 2: return thr;
	case 3: return fur;
	default: throw new ArrayIndexOutOfBoundsException();
	}
    }

    final public DMLValue get(java.lang.String i) {
	try {
	    _RAISE(runtimeError,new STRING ("no such label in tuple: "+i));
	} catch (java.rmi.RemoteException r) {
	    System.err.println(r);
	    r.printStackTrace();
	    return null;
	}
    }

    final public DMLValue get(Label i) {
	try {
	    _RAISE(runtimeError,new STRING ("no such label in tuple: "+i));
	} catch (java.rmi.RemoteException r) {
	    System.err.println(r);
	    r.printStackTrace();
	    return null;
	}
    }

    /** gibt die Stelligkeit des Tuples oder Records an */
    final public int getArity() {
	return 4;
    }

    final public DMLValue[] getVals() {
	DMLValue[] r = {fst,snd,thr,fur};
	return r;
    }

    _apply_fails ;
}
