/*
 * $Date$
 * $Revision$
 * $Author$
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
	return Constants.runtimeError.apply(new de.uni_sb.ps.dml.runtime.String("cannot bind future to "+v)).raise();
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
}
