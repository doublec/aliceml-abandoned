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

import java.rmi.server.UnicastRemoteObject;

final public class LVar extends UnicastRemoteObject
    implements DMLLVar {

    private DMLValue ref=null;

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

    final synchronized public DMLValue request() throws java.rmi.RemoteException { // gibt Wert zurück wenn verfügbar
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

    // bindet Variable und startet Threads aus
    // suspendVector-Liste
    final synchronized public DMLValue bind(DMLValue v)
	throws java.rmi.RemoteException {
	// here I must check if the value v is admissible
	// avoid cycles
	// path compression is performed during checking
	boolean hasSelfRef = false;
	while (v instanceof DMLLVar) {
	    if (v == this) { // we detect a self-cycle
		hasSelfRef = true;
		break;
	    }
	    DMLValue vv = ((DMLLVar) v).getValue();
	    if (v == vv) { // we run into an unbound variable
		// hasSelfRef = false;
		break;
	    } else {
		v = vv;
	    }
	}
	if (hasSelfRef) {
	    _RAISENAME(LVar.Fulfill);
	}

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

    final public java.lang.String toString() {
	DMLValue val;
	try {
	    val=this.getValue();
	} catch (java.rmi.RemoteException r) {
	    System.err.println(r);
	    return null;
	}
	if (val instanceof LVar) {
	    return "<unresolved>: lvar";
	} else {
	    return val.toString();
	}
    }


    final public DMLValue apply(DMLValue v)  throws java.rmi.RemoteException {
	return this.request().apply(v);
    }
    final public DMLValue apply0() throws java.rmi.RemoteException {
	return this.request().apply0();
    }
    final public DMLValue apply2(DMLValue v1, DMLValue v2)
	throws java.rmi.RemoteException {
	return this.request().apply2(v1,v2);
    }
    final public DMLValue apply3(DMLValue v1, DMLValue v2, DMLValue v3)
	throws java.rmi.RemoteException {
	return this.request().apply3(v1,v2,v3);
    }
    final public DMLValue apply4(DMLValue v1, DMLValue v2, DMLValue v3, DMLValue v4)
	throws java.rmi.RemoteException {
	return this.request().apply4(v1,v2,v3,v4);
    }

    /** LVar und Future werden beim pickeln ersetzt, falls sie gebunden sind.
	Nicht gebunde logische Variablen dürfen nicht gepickelt werden. */
    final private void writeObject(java.io.ObjectOutputStream out)
	throws java.io.IOException {
	_RAISE(runtimeError,new STRING ("cannot pickle LVar"));
    }

    /** <code> exception Fulfill </code>*/
    final public static Name Fulfill = new UniqueName("LVar.Fullfil");
}
