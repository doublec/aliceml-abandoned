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

public class Future extends LVar {

    // von LVar: DMLValue ref = null;
    // diese ref kann nur eine LVar sein!

    public Future() throws java.rmi.RemoteException {
    }

    /** Dieser Konstruktor wird nur mit LVar als Argument aufgerufen.
     *  @param v LVar
     */
    public Future(DMLValue v) throws java.rmi.RemoteException {
	ref=v;
    }

    /** bind ist nicht erlaubt und wirft RuntimeError */
    public DMLValue bind(DMLValue v)  throws java.rmi.RemoteException {
	_RAISE(runtimeError,new STRING ("cannot bind future to "+v));
    }

    public java.lang.String toString() {
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
    public DMLValue apply(DMLValue val) throws java.rmi.RemoteException {
	return ref.apply(val); // ref ist LVar !
    }
    public DMLValue apply0() throws java.rmi.RemoteException {
	return ref.apply0();
    }
    public DMLValue apply2(DMLValue v1, DMLValue v2)
	throws java.rmi.RemoteException {
	return ref.apply2(v1,v2);
    }
    public DMLValue apply3(DMLValue v1, DMLValue v2, DMLValue v3)
	throws java.rmi.RemoteException {
	return ref.apply3(v1,v2,v3);
    }
    public DMLValue apply4(DMLValue v1, DMLValue v2, DMLValue v3, DMLValue v4)
	throws java.rmi.RemoteException {
	return ref.apply4(v1,v2,v3,v4);
    }
}
