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
import java.rmi.server.UnicastRemoteObject;

final public class Future extends UnicastRemoteObject
    implements DMLTransient {

    /** The logic value of which this is the future. */
    private DMLValue ref;

    //      public Future() throws RemoteException {
    //      }

    /** Dieser Konstruktor wird nur mit LVar als Argument aufgerufen.
     *  @param v LVar
     */
    public Future(DMLValue v) throws RemoteException {
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
	    return (val instanceof DMLTransient) &&
		val.equals(request());
	} catch (RemoteException r) {
	    System.err.println(r);
	    return false;
	}
    }

    final synchronized public DMLValue getValue()
	throws RemoteException { // gibt Wert zurück ohne blockieren
	if (ref instanceof DMLTransient) {
	    ref = ((DMLTransient) ref).getValue();
	}
	return ref;
    }

    final synchronized public DMLValue request()
	throws RemoteException { // gibt Wert zurück wenn verfügbar
	if (ref instanceof DMLTransient) {
	    ref = ((DMLTransient) ref).request();
	}
	return ref;
    }

    /** bind ist nicht erlaubt und wirft RuntimeError */
    final public DMLValue bind(DMLValue v)
	throws RemoteException {
	_RAISENAME(Future.Rebind);
    }

    final public java.lang.String toString() {
	DMLValue val = null;
	try {
	    this.getValue();
	} catch (RemoteException r) {
	    System.err.println(r);
	}
	if (val instanceof LVar)
	    return "<unresolved>: future";
	else
	    return val.toString();
    }

    final public java.lang.String toString(int level) throws java.rmi.RemoteException {
	if (level<1) {
	    return "...";
	} else {
	    DMLValue val = null;
	    try {
		this.getValue();
	    } catch (RemoteException r) {
		System.err.println(r);
	    }
	    if (val instanceof LVar)
		return "<unresolved>: future";
	    else
		return val.toString(level-1);
	}
    }

    /** die Referenz der Future wird appliziert */
    final public DMLValue apply(DMLValue val) throws RemoteException {
	return ref.apply(val); // ref ist LVar !
    }
    final public DMLValue apply0() throws RemoteException {
	return ref.apply0();
    }
    final public DMLValue apply2(DMLValue v1, DMLValue v2)
	throws RemoteException {
	return ref.apply2(v1,v2);
    }
    final public DMLValue apply3(DMLValue v1, DMLValue v2, DMLValue v3)
	throws RemoteException {
	return ref.apply3(v1,v2,v3);
    }
    final public DMLValue apply4(DMLValue v1, DMLValue v2, DMLValue v3, DMLValue v4)
	throws RemoteException {
	return ref.apply4(v1,v2,v3,v4);
    }

    UNAME(Fulfill,LVar.Fullfil);
    UNAME(Rebind,LVar.Rebind);

    /** LVar und Future werden beim pickeln ersetzt, falls sie gebunden sind.
	Nicht gebunde logische Variablen dürfen nicht gepickelt werden. */
    final private void writeObject(java.io.ObjectOutputStream out)
	throws java.io.IOException {
	_RAISE(runtimeError,new STRING ("cannot pickle Future"));
    }
}
