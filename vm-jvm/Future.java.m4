package de.uni_sb.ps.dml.runtime;

public class DMLFuture extends DMLLVar {

    // von DMLLVar: DMLValue ref = null;
    // diese ref kann nur eine DMLLVar sein!

    /** Dieser Konstruktor wird nur mit DMLLVar als Argument aufgerufen.
     *  @param v DMLLVar
     */
    public DMLFuture() throws java.rmi.RemoteException {
    }

    public DMLFuture(DMLValue v) throws java.rmi.RemoteException {
	super();
	ref=v;
    }

    /** bind ist nicht erlaubt und wirft RuntimeError */
    public DMLValue bind(DMLValue v)  throws java.rmi.RemoteException {
	return DMLConstants.runtimeError.apply(new DMLString("cannot bind future to "+v)).raise();
    }

    public String toString() {
	DMLValue val = null;
	try {
	    this.getValue();
	} catch (java.rmi.RemoteException r) {
	    System.err.println(r);
	}
	if (val instanceof DMLLVar)
	    return "<unresolved>: future";
	else
	    return val.toString();
    }

    /** die Referenz der DMLFuture wird appliziert */
    public DMLValue apply(DMLValue val) throws java.rmi.RemoteException {
	return ref.apply(val); // ref ist DMLLVar !
    }
}
