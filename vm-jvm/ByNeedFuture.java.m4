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

final public class ByNeedFuture extends UnicastRemoteObject
    implements DMLLVar {
    // von Future: DMLValue ref = null;
    // ref kann hier nur Function : unit -> 'a  sein.
    // diese Bedingung wird nicht gepr�ft

    /** The closure that is evaluated once when it is requested.
     */
    private DMLValue closure = null;

    private LVar ref = null;

    private int state = 0; // 0 - unbound, 1 ByNeed error, 2 = bound

    public ByNeedFuture(DMLValue v) throws java.rmi.RemoteException {
	closure = v;
	ref = new LVar();
    }

    final synchronized public DMLValue getValue() throws java.rmi.RemoteException { // gibt Wert zur�ck ohne blockieren
	return ref.getValue();
    }

    final synchronized public DMLValue request() throws java.rmi.RemoteException {
	if (state == 2) {
	    return ref.request();
	}
	else if (state == 0) {
	    DMLValue temp = closure;
	    DMLValue v = null;
	    closure = null;
	    boolean hasSelfRef = false;
	    try {
		v = temp.apply0();

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
		if (!hasSelfRef) {
		    ref.bind(v);
		}
	    } catch (ExceptionWrapper t) {
		if (t.value == LVar.Fulfill ||
		    hasSelfRef) {
		    throw new ExceptionWrapper(ByNeedFuture.ByNeed.apply(LVar.Fulfill));
		} else {
		    throw t;
		}
	    }

	    if (hasSelfRef) {
		state = 1; // error
		throw new ExceptionWrapper(ByNeedFuture.ByNeed.apply(LVar.Fulfill));
	    } else {
		return ref.request();
	    }
	} else { // state == 1
		    throw new ExceptionWrapper(ByNeedFuture.ByNeed.apply(LVar.Fulfill));
	}
    }

    /** bind ist nicht erlaubt und wirft RuntimeError */
    final public DMLValue bind(DMLValue v)  throws java.rmi.RemoteException {
	_RAISE(runtimeError,new STRING ("cannot bind byNeedFuture to "+v));
    }

    final public java.lang.String toString() {
	DMLValue val=null;
	try{
	    val=this.getValue();
	} catch (java.rmi.RemoteException r) {
	    System.out.println(r);
	}
	if (val instanceof LVar) {
	    return "<unresolved>: byneed-future";
	} else {
	    return val.toString();
	}
    }

    final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException {
	return request().apply(val); // request() ist LVar !
    }
    final public DMLValue apply0() throws java.rmi.RemoteException {
	return request().apply0();
    }
    final public DMLValue apply2(DMLValue v1, DMLValue v2)
	throws java.rmi.RemoteException {
	return request().apply2(v1,v2);
    }
    final public DMLValue apply3(DMLValue v1, DMLValue v2, DMLValue v3)
	throws java.rmi.RemoteException {
	return request().apply3(v1,v2,v3);
    }
    final public DMLValue apply4(DMLValue v1, DMLValue v2, DMLValue v3, DMLValue v4)
	throws java.rmi.RemoteException {
	return request().apply4(v1,v2,v3,v4);
    }

    final public static Constructor ByNeed = new UniqueConstructor("ByNeed");
}
