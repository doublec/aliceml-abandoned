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
    final public DMLValue get4() { throw new ArrayIndexOutOfBoundsException(); }

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
	} catch (java.rmi.RemoteException r) {
	    System.err.println(r);
	    r.printStackTrace();
	    return false;
	}
    }

    final public java.lang.String toString() {
	return constructor+"("+fst+", "+snd+", "+thr+", "+fur+") : constructed value";
    }

    _getConstructor ;
    _apply_fails ;
}
