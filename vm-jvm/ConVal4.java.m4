/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

final public class ConValTuple4 implements DMLConVal {

    private DMLValue fst = null;
    private DMLValue snd = null;
    private DMLValue thr = null;
    private DMLValue fur = null;

    private Constructor constructor=null;

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
	return (val instanceof ConValTuple4) &&
	    fst.equals(((ConValTuple4) val).fst) &&
	    snd.equals(((ConValTuple4) val).snd) &&
	    thr.equals(((ConValTuple4) val).thr) &&
	    fur.equals(((ConValTuple4) val).fur);
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
	return constructor+"("+fst+", "+snd+", "+thr+", "+fur+") : constructed value";
    }

    _getConstructor ;
    _apply_fails ;
    _request_id ;
    _getValue_id ;
    _raise ;
}
