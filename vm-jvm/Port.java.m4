/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

final public class Port extends UnicastRemoteObject
    implements DMLPort {

    DMLValue first = null;
    LVar last = null;

    public Port() throws RemoteException {
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

    final public static class NewPort extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args=fromTuple(val,1,"Port.newPort");
	    try {
		return new Port();
	    } catch (RemoteException r) {
		System.err.println(r);
		return null;
	    }
	}
    }
    final public static NewPort newPort = new NewPort();

    final public static class Send extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args=fromTuple(val,2,"Port.send");
	    DMLValue p = args[0].request();
	    if (!(p instanceof DMLPort))
		return error("argument #1 not DMLPort",val);
	    DMLPort port = (DMLPort) p;
	    try {
		return port.send(args[1]);
	    } catch (RemoteException r) {
		System.err.println(r);
		return null;
	    }
	}
    }
    final public static Send send = new Send();

    final public static class Recieve extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException{
	    DMLValue[] args=fromTuple(val,1,"Port.recieve");
	    DMLValue p = args[0].request();
	    if (!(p instanceof DMLPort))
		return error("argument #1 not DMLPort",val);
	    DMLPort port = (DMLPort) p;
	    try {
		return port.recieve();
	    } catch (RemoteException r) {
		System.err.println(r);
		return null;
	    }
	}
    }
    final public static Recieve recieve = new Recieve();

    // Hilfsfunktionen
    final public static DMLValue[] fromTuple
	(DMLValue v, /** <code>value-Tuple</code>*/
	 int ea,     // erwartete Anzahl Argumente
	 java.lang.String errMsg) throws java.rmi.RemoteException {
	v=v.request();
	if (v instanceof DMLTuple) {
	    DMLTuple t=(DMLTuple) v;
	    if (t.getArity()==ea) {
		DMLValue[] vals = new DMLValue[ea];
		for(int i=0; i<ea; i++)
		    vals[i]=t.getByIndex(i);
		return vals;
	    }
	    else
		error("wrong number of arguments in "+errMsg, v);
	}
	else
	    error("wrong argument type for "+errMsg,v);
	return null;
    }

    final protected static DMLValue error
	(java.lang.String msg, DMLValue v) throws java.rmi.RemoteException {
	// sonst: Fehler
	DMLValue[] err = {
	    new de.uni_sb.ps.dml.runtime.String(msg),
	    v};
	return Constants.
	    runtimeError.apply(new Tuple(err)).raise();
    }
}
