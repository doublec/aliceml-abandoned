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

import java.rmi.RemoteException;

final public class Cons implements DMLConVal {

    public DMLValue car=null;
    public DMLValue cdr=null;

    public Cons(DMLValue fst, DMLValue snd) {
	car=fst;
	cdr=snd;
    }

    public DMLValue get0() { return car; }
    public DMLValue get1() { return cdr; }
    public DMLValue get2() { throw new ArrayIndexOutOfBoundsException(); }
    public DMLValue get3() { throw new ArrayIndexOutOfBoundsException(); }

    final public DMLValue getContent() {
	return new Tuple2(car,cdr);
    }

    final public Constructor getConstructor() {
	return List.cons;
    }

    final public java.lang.String toString() {
	return "("+car+". "+cdr+")";
    }

    final public java.lang.String toString(int level) throws java.rmi.RemoteException {
	if (level<1) {
	    return "...";
	} else {
	    return "("+car.toString(level-1)+". "+cdr.toString(level-1)+")";
	}
    }

    /** Gleichheit der  und Inhalte */
    final public boolean equals(Object val) {
	if (val instanceof Cons) {
	    return car.equals(((Cons) val).car) &&
		cdr.equals(((Cons) val).cdr);
	} else if (val instanceof DMLTransient) {
	    return val.equals(this);
	} else {
	    return false;
	}
    }

    final public void set(DMLValue v0) throws RemoteException {
	if (v0 instanceof Tuple2) {
	    Tuple2 t = (Tuple2) v0;
	    car = t.fst;
	    cdr = t.snd;
	} else {
	    // this should never happen
	    _RAISE(runtimeError,new STRING ("INTERNAL COMPILER ERROR"));
	}
    }

    final public void set(DMLValue v0,DMLValue v1) throws RemoteException {
	car = v0;
	cdr = v1;
    }

    final public void set(DMLValue v0,DMLValue v1,DMLValue v2) throws RemoteException {
	// this should never happen
	_RAISE(runtimeError,new STRING ("INTERNAL COMPILER ERROR"));
    }

    final public void set(DMLValue v0,DMLValue v1,DMLValue v2,DMLValue v3) throws RemoteException {
	// this should never happen
	_RAISE(runtimeError,new STRING ("INTERNAL COMPILER ERROR"));
    }
    _apply_fails ;
}
