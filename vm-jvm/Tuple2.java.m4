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

/** Special class for tuples with arity 2.
 */
final public class Tuple2 implements DMLTuple {

    protected DMLValue fst;
    protected DMLValue snd;

    public Tuple2() {}

    public Tuple2(DMLValue eins,
		  DMLValue zwei) {
	fst=eins;
	snd=zwei;
    }

    final public DMLValue get0() { return fst; }
    final public DMLValue get1() { return snd; }
    final public DMLValue get2() { throw new ArrayIndexOutOfBoundsException(); }
    final public DMLValue get3() { throw new ArrayIndexOutOfBoundsException(); }
    final public DMLValue get4() { throw new ArrayIndexOutOfBoundsException(); }

    final public void setContent(DMLValue eins,
			DMLValue zwei) {
	if (fst == null) {
	    fst=eins;
	    snd=zwei;
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
	    } else if (val instanceof Tuple2) {
		Tuple2 v = (Tuple2) val;
		return
		    fst.equals(v.fst) &&
		    snd.equals(v.snd);
	    } else {
		DMLTuple t = (DMLTuple) val;
		if (t.getArity()!=2) {
		    return false;
		} else {
		    return
			t.get0().equals(fst) &&
			t.get1().equals(snd);
		}
	    }
	} else {
	    return false;
	}
    }

    final public java.lang.String toString() {
	return "("+fst+", "+snd+")/2";
    }

    /** gibt den i-ten Eintrag des Tuples oder Records*/
    final public DMLValue get(int i) {
	switch (i) {
	case 0: return fst;
	case 1: return snd;
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
	return 2;
    }

    final public DMLValue[] getVals() {
	DMLValue[] r = {fst,snd};
	return r;
    }

    _apply_fails ;
}
