package de.uni_sb.ps.dml.runtime;

final public class ConValTuple3 implements DMLConVal {

    DMLValue fst = null;
    DMLValue snd = null;
    DMLValue thr = null;

    DMLConstructor constructor=null;

    public ConValTuple3(DMLConstructor con,
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
    final public boolean equals(Object val) {
	return (val instanceof ConValTuple5) &&
	    fst.equals(((ConValTuple5) val).fst) &&
	    snd.equals(((ConValTuple5) val).snd) &&
	    thr.equals(((ConValTuple5) val).thr);
    }

    final public DMLValue getContent() {
	return new Tuple3(fst,snd,thr);
    }

    final public DMLValue assign(DMLValue val) {
	try {
	    return DMLConstants.runtimeError.apply(new DMLString("cannot assign "+val+" to "+this)).raise();
	} catch (java.rmi.RemoteException r) {
	    System.out.println(r);
	    return null;
	}
    }

    final public String toString() {
	return constructor+"("+fst+", "+snd+", "+thr+") : constructed value";
    }

    final public DMLValue getValue() {
	return this;
    }

    final public DMLValue request() {
	return this;
    }

    final public DMLValue apply(DMLValue v) {
	try {
	    return DMLConstants.runtimeError.apply(new DMLString("cannot apply "+this+" to "+v)).raise();
	} catch (java.rmi.RemoteException r) {
	    System.out.println(r);
	    return null;
	}
    }

    final public DMLValue raise() {
	throw new DMLExceptionWrapper(this);
    }

    final public DMLConstructor getConstructor() {
	return constructor;
    }
}
