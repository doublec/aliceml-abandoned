/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

final public class ConValTuple4 implements DMLConVal {

    protected DMLValue fst = null;
    protected DMLValue snd = null;
    protected DMLValue thr = null;
    protected DMLValue fur = null;

    protected Constructor constructor=null;

    public ConValTuple4(Constructor con) {
	constructor = con;
    }

    public ConValTuple4(Constructor con,
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

    final public void setContent(DMLValue eins,
			DMLValue zwei,
			DMLValue drei,
			DMLValue vier) {
	fst=eins;
	snd=zwei;
	thr=drei;
	fur=vier;
    }

    final public DMLValue getContent() {
	return new Tuple4(fst,snd,thr,fur);
    }

    /** Gleichheit der  und Inhalte */
    final public boolean equals(java.lang.Object val) {
	try {
	    if (val instanceof ConValTuple4) {
	    ConValTuple4 v = (ConValTuple4) val;
	    return
		fst.equals(v.fst) &&
		snd.equals(v.snd) &&
		thr.equals(v.thr) &&
		fur.equals(v.fur);
	} else if (val instanceof DMLConVal) {
	    DMLTuple t = (DMLTuple) ((DMLConVal) val).getContent();
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
	return constructor+"("+fst+", "+snd+", "+thr+", "+fur+") : constructed value";
    }

    _getConstructor ;
    _apply_fails ;
    _request_id ;
    _getValue_id ;
    _raise ;
}
