/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

final public class ConValTuple2 implements DMLConVal {

    private DMLValue fst = null;
    private DMLValue snd = null;

    private Constructor constructor=null;

    public ConValTuple2(Constructor con) {
	constructor = con;
    }

    public ConValTuple2(Constructor con,
			DMLValue eins,
			DMLValue zwei) {
	constructor=con;
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
	fst=eins;
	snd=zwei;
    }

    final public DMLValue getContent() {
	return new Tuple2(fst,snd);
    }

    /** Gleichheit der  und Inhalte */
    final public boolean equals(java.lang.Object val) {
	try {
	    if (val instanceof ConValTuple2) {
	    ConValTuple2 v = (ConValTuple2) val;
	    return
		fst.equals(v.fst) &&
		snd.equals(v.snd);
	} else if (val instanceof DMLConVal) {
	    DMLTuple t = (DMLTuple) ((DMLConVal) val).getContent();
	    if (t.getArity()!=2) {
		return false;
	    } else {
		return
		    t.get0().equals(fst) &&
		    t.get1().equals(snd);
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

    /** setzt Wert auf val und gibt alten Wert zurueck */
    final public DMLValue assign(DMLValue val) throws java.rmi.RemoteException {
	_RAISE(runtimeError,new STRING ("cannot assign "+val+" to "+this));
    }

    final public java.lang.String toString() {
	return constructor+"("+fst+", "+snd+") : constructed value";
    }

    _getConstructor ;
    _apply_fails ;
    _request_id ;
    _getValue_id ;
    _raise ;
}
