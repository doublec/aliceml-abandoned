/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

import java.rmi.server.UnicastRemoteObject;

public class LVar extends UnicastRemoteObject
    implements DMLLVar {

    protected DMLValue ref=null;

    public LVar() throws java.rmi.RemoteException { }

    final synchronized public DMLValue getValue() throws java.rmi.RemoteException { // gibt Wert zurück ohne blockieren
	if (ref == null) {
	    return this;
	} else {
	    if (ref instanceof DMLLVar) {
		ref = ((DMLLVar) ref).getValue();
	    }
	}
	return ref;
    }

    synchronized public DMLValue request() throws java.rmi.RemoteException { // gibt Wert zurück wenn verfügbar
	if (ref==null) {
	    try {
		this.wait();
	    } catch (java.lang.InterruptedException e) {
		System.err.println(e);
		e.printStackTrace();
	    }
	}
	if (ref instanceof DMLLVar) {
	    ref = ((DMLLVar) ref).request();
	}
	return ref;
    }

    synchronized public DMLValue bind(DMLValue v) throws java.rmi.RemoteException { // bindet Variable und startet java.lang.Threads aus suspendVector-Liste
	ref=v;
	this.notifyAll();
	return Constants.dmlunit;
    }

    /** Gleichheit der referenzierten Werte, blockiert auf beiden Werten */
    final public boolean equals(java.lang.Object val) {
	try {
	    return (val instanceof LVar) && this.request().equals(((LVar) val).request());
	} catch (java.rmi.RemoteException r) {
	    System.err.println(r);
	    return false;
	}
    }

    public java.lang.String toString() {
	DMLValue val;
	try {
	    val=this.getValue();
	} catch (java.rmi.RemoteException r) {
	    System.err.println(r);
	    return null;
	}
	if (val instanceof LVar) return "<unresolved>: lvar";
	return val.toString();
    }


    public DMLValue apply(DMLValue v)  throws java.rmi.RemoteException {
	return this.request().apply(v);
    }

    /** LVar und Future werden beim pickeln ersetzt, falls sie gebunden sind.
	Nicht gebunde logische Variablen dürfen nicht gepickelt werden. */
    final private void writeObject(java.io.ObjectOutputStream out)
	throws java.io.IOException {
	_RAISE(runtimeError,new STRING ("cannot pickle LVar"));
    }

    _raise ;
}
