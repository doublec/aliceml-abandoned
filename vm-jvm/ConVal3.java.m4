/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

final public class ConValTuple3 implements DMLConVal {

    DMLValue fst = null;
    DMLValue snd = null;
    DMLValue thr = null;

    Constructor constructor=null;

    public ConValTuple3(Constructor con,
			DMLValue eins,
			DMLValue zwei,
			DMLValue drei) {
	constructor=con;
	fst=eins;
	snd=zwei;
	thr=drei;
    }

    public DMLValue get0() { return fst; }
    public DMLValue get1() { return snd; }
    public DMLValue get2() { return thr; }
    public DMLValue get3() { throw new ArrayIndexOutOfBoundsException(); }
    public DMLValue get4() { throw new ArrayIndexOutOfBoundsException(); }

    /** Gleichheit der  und Inhalte */
    final public boolean equals(java.lang.Object val) {
	return (val instanceof ConValTuple3) &&
	    fst.equals(((ConValTuple3) val).fst) &&
	    snd.equals(((ConValTuple3) val).snd) &&
	    thr.equals(((ConValTuple3) val).thr);
    }

    final public DMLValue getContent() {
	return new Tuple3(fst,snd,thr);
    }

    final public DMLValue assign(DMLValue val) {
	try {
	    return Constants.runtimeError.apply(new de.uni_sb.ps.dml.runtime.String("cannot assign "+val+" to "+this)).raise();
	} catch (java.rmi.RemoteException r) {
	    System.out.println(r);
	    return null;
	}
    }

    final public java.lang.String toString() {
	return constructor+"("+fst+", "+snd+", "+thr+") : constructed value";
    }

    _getConstructor ;
    _apply_fails ;
    _request_id ;
    _getValue_id ;
    _raise ;
}
