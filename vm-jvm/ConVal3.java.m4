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
    final public DMLValue get4() { throw new ArrayIndexOutOfBoundsException(); }

    final public void setContent(DMLValue eins,
			DMLValue zwei,
			DMLValue drei) {
	if (fst == null) {
	    fst=eins;
	    snd=zwei;
	    thr=drei;
	} else {
	    try {
		_RAISE(runtimeError, new STRING ("cannot set content twice"));
	    } catch (java.rmi.RemoteException r) {
		System.err.println(r);
		r.printStackTrace();
	    }
	}
    }

    final public DMLValue getContent() {
	return new Tuple3(fst,snd,thr);
    }

    /** Gleichheit der  und Inhalte */
    final public boolean equals(java.lang.Object val) {
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
	    } else {
		return false;
	    }
	} catch (java.rmi.RemoteException r) {
	    System.err.println(r);
	    r.printStackTrace();
	    return false;
	}
    }

    final public java.lang.String toString() {
	return constructor+"("+fst+", "+snd+", "+thr+") : constructed value";
    }

    _getConstructor ;
    _apply_fails ;
}
