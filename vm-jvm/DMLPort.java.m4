package de.uni_sb.ps.dml.builtin;

import de.uni_sb.ps.dml.runtime.*;

final public class DMLPort implements DMLValue, DMLRemoteValue, RemotePort {

    DMLValue first = null;
    DMLLVar last = null;

    public DMLPort() {
	last = new DMLLVar();
	first = last;
    }

    final public DMLValue send(DMLValue msg) {
	DMLLVar newLast = new DMLLVar();
	synchronized (last) {
	    last.bind(new Cons(msg,newLast));
	    last=newLast;
	}
	return DMLConstants.dmlunit;
    }

    final public DMLValue recieve() {
	DMLValue ret = null;
	synchronized (first) {
	    ret = ((Cons) first.request()).car;
	    first = ((Cons) first.request()).cdr;
	}
	return ret;
    }

    /** Liefert das Array selbst. */
    final public DMLValue getValue() {
	return this;
    }

    /** Liefert das Array selbst. */
    final public DMLValue request() {
	return this;
    }

    final public DMLValue apply(DMLValue val) {
	return DMLConstants.runtimeError.apply( new DMLString("cannot apply "+this+" to "+val)).raise();
    }

    final public DMLValue raise() {
	throw new DMLExceptionWrapper(this);
    }

    final public java.lang.String toString() {
	return first.toString()+": port";
    }
}
