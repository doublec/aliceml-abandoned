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
    implements DMLTransient {

    private DMLValue ref = null;

    public LVar() throws java.rmi.RemoteException { }

    final synchronized public DMLValue getValue() throws java.rmi.RemoteException { // gibt Wert zurück ohne blockieren
	if (ref == null) {
	    return this;
	} else {
	    if (ref instanceof DMLTransient) {
		ref = ((DMLTransient) ref).getValue();
	    }
	}
	return ref;
    }

    final synchronized public DMLValue request() throws java.rmi.RemoteException { // gibt Wert zurück wenn verfügbar
	while (ref==null) {
	    try {
		this.wait();
	    } catch (java.lang.InterruptedException e) {
		// This should never happen!
		System.err.println(e);
		e.printStackTrace();
	    }
	}
	if (ref instanceof DMLTransient) {
	    ref = ((DMLTransient) ref).request();
	}
	return ref;
    }

    // binds the variable and starts threads in
    // the suspend list
    final synchronized public DMLValue bind(DMLValue v)
	throws java.rmi.RemoteException {
	if (ref != null) { // then this a second attempt to bind
	    _RAISENAME(LVar.Rebind);
	} else {
	    // here I must check if the value v is admissible
	    // avoid cycles
	    // path compression is performed during checking
	    while (v instanceof DMLTransient) {
		if (v == this) { // we detect a self-cycle
		    _RAISENAME(LVar.Fulfill);
		}
		DMLValue vv = ((DMLTransient) v).getValue();
		if (v == vv) { // we run into an unbound variable
		    break;
		} else {
		    v = vv;
		}
	    }
	    ref = v;
	    this.notifyAll();
	    return Constants.dmlunit;
	}
    }

    /** Gleichheit der referenzierten Werte, blockiert auf beiden Werten */
    final public boolean equals(Object val) {
	try {
	    return (val instanceof LVar) &&
		val.equals(request());
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
    final public static Name Rebind = new UniqueName("LVar.Rebind");
}
