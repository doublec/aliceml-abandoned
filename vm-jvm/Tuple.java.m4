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

/** Tuple-Darstellung in DML.
 *  @see Record
 */
public class Tuple implements DMLTuple {

    final protected DMLValue vals[];

    public Tuple(DMLValue[] v) {
	vals=v;
    }

    public DMLValue get0() { return vals[0]; }
    public DMLValue get1() { return vals[1]; }
    public DMLValue get2() { return vals[2]; }
    public DMLValue get3() { return vals[3]; }
    public DMLValue get4() { return vals[4]; }

    public boolean equals(java.lang.Object val) {
	if (val instanceof DMLTuple) {
	    if (val instanceof Record) {
		return false;
	    } else if (val instanceof Tuple) {
		Tuple r = (Tuple) val;
		if (r.vals.length != this.vals.length)
		    return false;
		int length=vals.length;
		for(int i=0; i<length; i++) {
		    if (!vals[i].equals(r.vals[i])) {
			return false;
		    }
		}
		return true;
	    } else { // kann nur noch Tuple<i> sein
		switch (vals.length) {
		case 1:
		    if (val instanceof Tuple1) {
			return vals[0].equals(((Tuple1) val).fst);
		    } else {
			return false;
		    }
		case 2:
		    if (val instanceof Tuple2) {
			return
			    vals[0].equals(((Tuple2) val).fst) &&
			    vals[1].equals(((Tuple2) val).snd);
		    } else {
			return false;
		    }
		case 3:
		    if (val instanceof Tuple3) {
			return
			    vals[0].equals(((Tuple3) val).fst) &&
			    vals[1].equals(((Tuple3) val).snd) &&
			    vals[2].equals(((Tuple3) val).thr);
		    } else {
			return false;
		    }
		case 4:
		    if (val instanceof Tuple4) {
			return
			    vals[0].equals(((Tuple4) val).fst) &&
			    vals[1].equals(((Tuple4) val).snd) &&
			    vals[2].equals(((Tuple4) val).thr) &&
			    vals[3].equals(((Tuple4) val).fur);
		    } else {
			return false;
		    }
		case 5:
		    if (val instanceof Tuple5) {
			return
			    vals[0].equals(((Tuple5) val).fst) &&
			    vals[1].equals(((Tuple5) val).snd) &&
			    vals[2].equals(((Tuple5) val).thr) &&
			    vals[3].equals(((Tuple5) val).fur) &&
			    vals[4].equals(((Tuple5) val).fiv);
		    } else {
			return false;
		    }
		default:
		    return false;
		}
	    }
	} else {
	    return false;
	}
    }

    public java.lang.String toString() {
	java.lang.String s="(";
	int i;
	for (i=0; i<vals.length;i++) {
	    if (i>0) s+=", ";
	    s+=vals[i];
	}
	return s+")/"+getArity();
    }

    /** gibt den i-ten Eintrag des Tuples oder Records*/
    public DMLValue get(int i){
	return vals[i];
    }

    public DMLValue get(java.lang.String i) {
	try {
	    _RAISE(runtimeError,new STRING ("no such label in tuple: "+i));
	} catch (java.rmi.RemoteException r) {
	    System.err.println(r);
	    r.printStackTrace();
	    return null;
	}
    }

    public DMLValue get(Label i) {
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
	return vals.length;
    }

    final public DMLValue[] getVals() {
	return vals;
    }
    _apply_fails ;
}
