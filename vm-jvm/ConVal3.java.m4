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

final public class ConVal3 implements DMLConVal {

    public DMLValue fst = null;
    public DMLValue snd = null;
    public DMLValue thr = null;

    final public Constructor constructor;

    public ConVal3(Constructor con) {
	constructor = con;
    }

    public ConVal3(Constructor con,
			DMLValue eins,
			DMLValue zwei,
			DMLValue drei) {
	constructor=con;
	fst=eins;
	snd=zwei;
	thr=drei;
    }

    final public DMLValue get0() { return fst; }
    final public DMLValue get1() { return snd; }
    final public DMLValue get2() { return thr; }
    final public DMLValue get3() { throw new ArrayIndexOutOfBoundsException(); }

    final public DMLValue getContent() {
	return new Tuple3(fst,snd,thr);
    }

    /** Gleichheit der  und Inhalte */
    final public boolean equals(Object val) {
	try {
	    if (val instanceof ConVal3) {
		ConVal3 v = (ConVal3) val;
		return
		    v.constructor == constructor &&
		    fst.equals(v.fst) &&
		    snd.equals(v.snd) &&
		    thr.equals(v.thr);
	    } else if (val instanceof DMLConVal) {
		DMLConVal cv = (DMLConVal) val;
		if (cv.getConstructor() == constructor) {
		    DMLTuple t = (DMLTuple) cv.getContent();
		    if (t.getArity()!=3) {
			return false;
		    } else {
			return
			    t.get0().equals(fst) &&
			    t.get1().equals(snd) &&
			    t.get2().equals(thr);
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
	return constructor+"("+fst+", "+snd+", "+thr+") : constructed value";
    }

    final public java.lang.String toString(int level) throws java.rmi.RemoteException {
	if (level<1) {
	    return "...";
	} else {
	    return constructor.toString(level-1)+
		"("+fst.toString(level-1)+", "+
		snd.toString(level-1)+", "+
		thr.toString(level-1) +
		")";
	}
    }

    final public void set(DMLValue v0) throws RemoteException {
	if (v0 instanceof Tuple3) {
	    Tuple3 t = (Tuple3) v0;
	    fst = t.fst;
	    snd = t.snd;
	    thr = t.thr;
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
	fst = v0;
	snd = v1;
	thr = v2;
    }

    final public void set(DMLValue v0,DMLValue v1,DMLValue v2,DMLValue v3) throws RemoteException {
	// this should never happen
	_RAISE(runtimeError,new STRING ("INTERNAL COMPILER ERROR"));
    }

    _getConstructor ;
    _apply_fails ;
}
