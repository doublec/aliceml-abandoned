package de.uni_sb.ps.dml.runtime;

final public class ConValTuple2 implements DMLConVal {

    DMLValue fst = null;
    DMLValue snd = null;

    Constructor constructor=null;

    public ConValTuple2(Constructor con,
			DMLValue eins,
			DMLValue zwei) {
	constructor=con;
	fst=eins;
	snd=zwei;
    }

    public DMLValue get0() { return fst; }
    public DMLValue get1() { return snd; }
    public DMLValue get2() { throw new ArrayIndexOutOfBoundsException(); }
    public DMLValue get3() { throw new ArrayIndexOutOfBoundsException(); }
    public DMLValue get4() { throw new ArrayIndexOutOfBoundsException(); }

    /** Gleichheit der  und Inhalte */
    final public boolean equals(java.lang.Object val) {
	return (val instanceof ConValTuple2) &&
	    fst.equals(((ConValTuple2) val).fst) &&
	    snd.equals(((ConValTuple2) val).snd);
    }

    final public DMLValue getContent() {
	return new Tuple2(fst,snd);
    }

    /** setzt Wert auf val und gibt alten Wert zurueck */
    final public DMLValue assign(DMLValue val) {
	try {
	    return Constants.runtimeError.apply(new de.uni_sb.ps.dml.runtime.String("cannot assign "+val+" to "+this)).raise();
	} catch (java.rmi.RemoteException r) {
	    System.out.println(r);
	    return null;
	}
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
