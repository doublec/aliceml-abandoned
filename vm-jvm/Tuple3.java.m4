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

/** Special class for tuples with arity 3.
 */
final public class Tuple3 implements DMLTuple {

    public DMLValue fst;
    public DMLValue snd;
    public DMLValue thr;

    public Tuple3() {}

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

    /** Gleichheit der  und Inhalte */
    final public boolean equals(Object val) {
	if (val instanceof DMLTuple) {
	    if (val instanceof Record) {
		return false;
	    } else if (val instanceof Tuple3) {
		Tuple3 v = (Tuple3) val;
		return
		    fst.equals(v.fst) &&
		    snd.equals(v.snd) &&
		    thr.equals(v.thr);
	    } else {
		DMLTuple t = (DMLTuple) val;
		if (t.getArity()!=3) {
		    return false;
		} else {
		    return
			t.get0().equals(fst) &&
			t.get1().equals(snd) &&
			t.get2().equals(thr);
		}
	    }
	} else if (val instanceof DMLTransient) {
	    return val.equals(this);
	} else {
	    return false;
	}
    }

    public java.lang.String toString() {
	return "("+fst+", "+snd+", "+thr+")/3";
    }

    public java.lang.String toString(int level) throws java.rmi.RemoteException {
	if (level<1) {
	    return "...";
	} else {
	return "("+fst.toString(level-1)+", "+snd.toString(level-1)+", "+thr.toString(level-1)+")/3";
    }
    }
    /** gibt den i-ten Eintrag des Tuples oder Records*/
    final public DMLValue get(int i){
	switch (i) {
	case 0: return fst;
	case 1: return snd;
	case 2: return thr;
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

    /** gibt die Stelligkeit des Tuples oder Records an */
    final public int getArity() {
	return 3;
    }

    final public DMLValue[] getVals() {
	DMLValue[] r = {fst,snd,thr};
	return r;
    }

    _apply_fails ;
}
