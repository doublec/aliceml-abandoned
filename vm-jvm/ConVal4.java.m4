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

import java.rmi.RemoteException;

final public class ConVal4 implements DMLConVal {

    public DMLValue fst = null;
    public DMLValue snd = null;
    public DMLValue thr = null;
    public DMLValue fur = null;

    final public Constructor constructor;

    public ConVal4(Constructor con) {
	constructor = con;
    }

    public ConVal4(Constructor con,
			DMLValue eins,
			DMLValue zwei,
			DMLValue drei,
			DMLValue vier) {
	constructor=con;
	fst=eins;
	snd=zwei;
	thr=drei;
	fur=vier;
    }

    final public DMLValue get0() { return fst; }
    final public DMLValue get1() { return snd; }
    final public DMLValue get2() { return thr; }
    final public DMLValue get3() { return fur; }

    final public DMLValue getContent() {
	return new Tuple4(fst,snd,thr,fur);
    }

    /** Gleichheit der  und Inhalte */
    final public boolean equals(Object val) {
	try {
	    if (val instanceof ConVal4) {
		ConVal4 v = (ConVal4) val;
		return
		    v.constructor == constructor &&
		    fst.equals(v.fst) &&
		    snd.equals(v.snd) &&
		    thr.equals(v.thr) &&
		    fur.equals(v.fur);
	    } else if (val instanceof DMLConVal) {
		DMLConVal cv = (DMLConVal) val;
		if (cv.getConstructor() == constructor) {
		    DMLTuple t = (DMLTuple) cv.getContent();
		    if (t.getArity()!=4) {
			return false;
		    } else {
			return
			    t.get0().equals(fst) &&
			    t.get1().equals(snd) &&
			    t.get2().equals(thr) &&
			    t.get3().equals(fur);
		    }
		} else {
		    return false;
		}
	    } else if (val instanceof DMLTransient) {
		return val.equals(this);
	    } else {
		return false;
	    }
	} catch (RemoteException r) {
	    System.err.println(r);
	    r.printStackTrace();
	    return false;
	}
    }

    final public java.lang.String toString() {
	return constructor+"("+fst+", "+snd+", "+thr+", "+fur+") : constructed value";
    }

    final public java.lang.String toString(int level) throws java.rmi.RemoteException {
	if (level<1) {
	    return "...";
	} else {
	    return constructor.toString(level-1)+"("+fst.toString(level-1)+", "+snd.toString(level-1)+", "+thr.toString(level-1)+", "+fur.toString(level-1)+") : constructed value";
	}
    }

    final public void set(DMLValue v0) throws RemoteException {
	if (v0 instanceof Tuple4) {
	    Tuple4 t = (Tuple4) v0;
	    fst = t.fst;
	    snd = t.snd;
	    thr = t.thr;
	    fur = t.fur;
	} else {
	    // this should never happen
	    _RAISE(runtimeError,new STRING ("INTERNAL COMPILER ERROR"));
	}
    }

    final public void set(DMLValue v0,DMLValue v1) throws RemoteException {
	// this should never happen
	_RAISE(runtimeError,new STRING ("INTERNAL COMPILER ERROR"));
    }

    final public void set(DMLValue v0,DMLValue v1,DMLValue v2) throws RemoteException {
	// this should never happen
	_RAISE(runtimeError,new STRING ("INTERNAL COMPILER ERROR"));
    }

    final public void set(DMLValue v0,DMLValue v1,DMLValue v2,DMLValue v3) throws RemoteException {
	fst = v0;
	snd = v1;
	thr = v2;
	fur = v3;
    }
    _getConstructor ;
    _apply_fails ;
}
