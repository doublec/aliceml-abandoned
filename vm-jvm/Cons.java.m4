package de.uni_sb.ps.dml.builtin;

import de.uni_sb.ps.dml.runtime.*;

final public class Cons implements DMLConVal, DMLList {

    protected DMLValue car=null;
    protected DMLValue cdr=null;

    public Cons(DMLValue fst, DMLValue snd) {
	car=fst;
	cdr=snd;
    }

    final public DMLValue getCar() {
	return car;
    }

    final public DMLValue getCdr() {
	return cdr;
    }

    public DMLValue get0() { return car; }
    public DMLValue get1() { return cdr; }
    public DMLValue get2() { throw new ArrayIndexOutOfBoundsException(); }
    public DMLValue get3() { throw new ArrayIndexOutOfBoundsException(); }
    public DMLValue get4() { throw new ArrayIndexOutOfBoundsException(); }

    /** Gleichheit der  und Inhalte */
    final public boolean equals(Object val) {
	return (val instanceof Cons) &&
	    car.equals(((Cons) val).car) &&
	    cdr.equals(((Cons) val).cdr);
    }

    final public DMLValue getContent() {
	return new Tuple2(car,cdr);
    }

    /** wirft Fehler */
    final public DMLValue assign(DMLValue val) {
	try {
	    return DMLConstants.runtimeError.apply(new DMLString("cannot assign "+val+" to "+this)).raise();
	} catch (java.rmi.RemoteException r) {
	    System.err.println(r);
	    return null;
	}
    }

    final public java.lang.String toString() {
	return "("+car+", "+cdr+")";
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
	    System.err.println(r);
	    return null;
	}
    }

    final public DMLValue raise() {
	throw new DMLExceptionWrapper(this);
    }

    final public DMLConstructor getConstructor() {
	return List.cons;
    }
}
