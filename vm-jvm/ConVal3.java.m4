/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

final public class ConValTuple3 implements DMLConVal {

    private DMLValue fst = null;
    private DMLValue snd = null;
    private DMLValue thr = null;

    final public Constructor constructor;

    public ConValTuple3(Constructor con) {
	constructor = con;
    }

    public ConValTuple3(Constructor con,
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
	    if (val instanceof ConValTuple3) {
	    ConValTuple3 v = (ConValTuple3) val;
	    return
		fst.equals(v.fst) &&
		snd.equals(v.snd) &&
		thr.equals(v.thr);
	} else if (val instanceof DMLConVal) {
	    DMLTuple t = (DMLTuple) ((DMLConVal) val).getContent();
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
	} catch (java.rmi.RemoteException r) {
	    System.err.println(r);
	    r.printStackTrace();
	    return false;
	}
    }

    final public DMLValue assign(DMLValue val) throws java.rmi.RemoteException {
	_RAISE(runtimeError,new STRING ("cannot assign "+val+" to "+this));
    }

    final public java.lang.String toString() {
	return constructor+"("+fst+", "+snd+", "+thr+") : constructed value";
    }

    _getConstructor ;
    _apply_fails ;
}
