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

final public class Future extends UnicastRemoteObject
    implements DMLTransient {

    /** The logic value of which this is the future. */
    private DMLValue ref;

    //      public Future() throws java.rmi.RemoteException {
    //      }

    /** Dieser Konstruktor wird nur mit LVar als Argument aufgerufen.
     *  @param v LVar
     */
    public Future(DMLValue v) throws java.rmi.RemoteException {
	if (v instanceof LVar) {
	    ref = v;
	} else {
	    ref = null;
	    _RAISE(runtimeError, new STRING ("cannot create future of "+v));
	}
    }

    /** Gleichheit der referenzierten Werte, blockiert auf beiden Werten */
    final public boolean equals(Object val) {
	try {
	    return (val instanceof Future) &&
		val.equals(request());
	} catch (java.rmi.RemoteException r) {
	    System.err.println(r);
	    return false;
	}
    }

    final synchronized public DMLValue getValue()
	throws java.rmi.RemoteException { // gibt Wert zurück ohne blockieren
	if (ref instanceof DMLTransient) {
	    ref = ((DMLTransient) ref).getValue();
	}
	return ref;
    }

    final synchronized public DMLValue request()
	throws java.rmi.RemoteException { // gibt Wert zurück wenn verfügbar
	if (ref instanceof DMLTransient) {
	    ref = ((DMLTransient) ref).request();
	}
	return ref;
    }

    /** bind ist nicht erlaubt und wirft RuntimeError */
    final public DMLValue bind(DMLValue v)
	throws java.rmi.RemoteException {
	_RAISENAME(Future.Rebind);
    }

    final public java.lang.String toString() {
	DMLValue val = null;
	try {
	    this.getValue();
	} catch (java.rmi.RemoteException r) {
	    System.err.println(r);
	}
	if (val instanceof LVar)
	    return "<unresolved>: future";
	else
	    return val.toString();
    }

    /** die Referenz der Future wird appliziert */
    final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException {
	return ref.apply(val); // ref ist LVar !
    }
    final public DMLValue apply0() throws java.rmi.RemoteException {
	return ref.apply0();
    }
    final public DMLValue apply2(DMLValue v1, DMLValue v2)
	throws java.rmi.RemoteException {
	return ref.apply2(v1,v2);
    }
    final public DMLValue apply3(DMLValue v1, DMLValue v2, DMLValue v3)
	throws java.rmi.RemoteException {
	return ref.apply3(v1,v2,v3);
    }
    final public DMLValue apply4(DMLValue v1, DMLValue v2, DMLValue v3, DMLValue v4)
	throws java.rmi.RemoteException {
	return ref.apply4(v1,v2,v3,v4);
    }

    final public static Name Fulfill = new UniqueName("LVar.Fullfil");
    final public static Name Rebind = new UniqueName("LVar.Rebind");
}
