package de.uni_sb.ps.dml.builtin;

import de.uni_sb.ps.dml.runtime.*;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

final public class DMLPort extends UnicastRemoteObject
    implements DMLRemoteValue, RemotePort {

    DMLValue first = null;
    LVar last = null;

    public DMLPort() throws RemoteException {
	last = new LVar();
	first = last;
    }

    final public DMLValue send(DMLValue msg) throws RemoteException {
	LVar newLast = new LVar();
	synchronized (last) {
	    last.bind(new Cons(msg,newLast));
	    last=newLast;
	}
	return Constants.dmlunit;
    }

    final public DMLValue recieve() throws RemoteException {
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

    final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException {
	return Constants.runtimeError.apply( new de.uni_sb.ps.dml.runtime.String("cannot apply "+this+" to "+val)).raise();
    }

    final public DMLValue raise() {
	throw new ExceptionWrapper(this);
    }

    final public java.lang.String toString() {
	return first.toString()+": port";
    }
}
