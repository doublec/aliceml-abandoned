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

final public class ByNeedFuture extends UnicastRemoteObject
    implements DMLTransient {
    // von Future: DMLValue ref = null;
    // ref kann hier nur Function : unit -> 'a  sein.
    // diese Bedingung wird nicht geprüft

    /** The closure that is evaluated once when it is requested.
     */
    private DMLValue closure = null;

    //    private LVar ref = null;

    private int state = 0; // 0 - unbound, 1 ByNeed error, 2 = bound

    public ByNeedFuture(DMLValue v) throws RemoteException {
	closure = v;
	//	ref = new LVar();
    }

    final synchronized public DMLValue getValue()
	throws RemoteException { // gibt Wert zurück ohne blockieren
	if (state == 0) {
	    return this;
	} else {
	    return closure;
	}
    }

    final synchronized public DMLValue request()
	throws RemoteException {
	if (state == 2) {
	    return closure;
	}
	else if (state == 0) {
	    DMLValue temp = closure;
	    DMLValue v = null;
	    closure = null;
	    boolean hasSelfRef = false;
	    try {
		v = temp.apply0();

		while (v instanceof DMLTransient) {
		    if (v == this) { // we detect a self-cycle
			_RAISENAME(LVar.Fulfill);
		    }
		    DMLValue vv = ((DMLTransient) v).getValue();
		    if (v == vv) { // we run into an unbound variable
			// hasSelfRef = false;
			break;
		    } else {
			v = vv;
		    }
		}
		closure = v;
		state = 2; // now the variable is bound
		return closure;
	    } catch (ExceptionWrapper t) {
		state = 1; // error
		closure = t.value;
		throw new ExceptionWrapper(ByNeedFuture.ByNeed.apply(closure));
	    }
	} else { // state == 1
	    throw new ExceptionWrapper(ByNeedFuture.ByNeed.apply(closure));
	}
    }

    /** bind ist nicht erlaubt und wirft RuntimeError */
    final public DMLValue bind(DMLValue v)
	throws RemoteException {
	_RAISENAME(ByNeedFuture.Rebind);
    }

    final public java.lang.String toString() {
	DMLValue val=null;
	try{
	    val=this.getValue();
	} catch (RemoteException r) {
	    System.out.println(r);
	}
	if (val == this) {
	    return "<unresolved>: byneed-future";
	} else {
	    return val.toString();
	}
    }

    final public java.lang.String toString(int level) throws java.rmi.RemoteException {
	if (level<1) {
	    return "...";
	} else {
	    DMLValue val=null;
	    try{
		val=this.getValue();
	    } catch (RemoteException r) {
		System.out.println(r);
	    }
	    if (val == this) {
		return "<unresolved>: byneed-future";
	    } else {
		return "[BN"+val.toString(level-1)+"BN]";
	    }
	}
    }

    final public DMLValue apply(DMLValue val) throws RemoteException {
	return request().apply(val); // request() ist LVar !
    }
    final public DMLValue apply0() throws RemoteException {
	return request().apply0();
    }
    final public DMLValue apply2(DMLValue v1, DMLValue v2)
	throws RemoteException {
	return request().apply2(v1,v2);
    }
    final public DMLValue apply3(DMLValue v1, DMLValue v2, DMLValue v3)
	throws RemoteException {
	return request().apply3(v1,v2,v3);
    }
    final public DMLValue apply4(DMLValue v1, DMLValue v2, DMLValue v3, DMLValue v4)
	throws RemoteException {
	return request().apply4(v1,v2,v3,v4);
    }

    UCONS(ByNeed,ByNeed);
    UCONS(Rebind,ByNeedFuture.Rebind);

    /** LVar und Future werden beim pickeln ersetzt, falls sie gebunden sind.
	Nicht gebunde logische Variablen dürfen nicht gepickelt werden. */
    final private void writeObject(java.io.ObjectOutputStream out)
	throws java.io.IOException {
	_RAISE(runtimeError,new STRING ("cannot pickle ByNeed"));
    }
}
