package de.uni_sb.ps.dml.runtime;

final public class ConValTuple4 implements DMLConVal {

    DMLValue fst = null;
    DMLValue snd = null;
    DMLValue thr = null;
    DMLValue fur = null;

    Constructor constructor=null;

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

    public DMLValue get0() { return fst; }
    public DMLValue get1() { return snd; }
    public DMLValue get2() { return thr; }
    public DMLValue get3() { return fur; }
    public DMLValue get4() { throw new ArrayIndexOutOfBoundsException(); }

    /** Gleichheit der  und Inhalte */
    final public boolean equals(java.lang.Object val) {
	return (val instanceof ConValTuple5) &&
	    fst.equals(((ConValTuple5) val).fst) &&
	    snd.equals(((ConValTuple5) val).snd) &&
	    thr.equals(((ConValTuple5) val).thr) &&
	    fur.equals(((ConValTuple5) val).fur);
    }

    final public DMLValue getContent() {
	return new Tuple4(fst,snd,thr,fur);
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
