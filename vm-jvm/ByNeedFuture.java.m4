/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

public class ByNeedFuture extends Future {
    // von Future: DMLValue ref = null;
    // ref kann hier nur Function : unit -> 'a  sein.
    // diese Bedingung wird nicht geprüft

    private DMLValue closure = null;
    private LVar ref = null;

    public ByNeedFuture(DMLValue v) throws java.rmi.RemoteException {
	super();
	closure=v;
	ref = new LVar();
    }

    synchronized public DMLValue request() throws java.rmi.RemoteException {
	if (closure==null) {
	    if (ref instanceof DMLLVar) {
		return ((DMLLVar) ref).request();
	    } else {
		return ref;
	    }
	}
	else {
	    DMLValue temp = closure;
	    closure = null;
	    try {
		ref.bind(temp.apply(Constants.dmlunit));
	    } catch (Throwable t) {
		System.err.println(t);
	    }
	    if (ref instanceof DMLLVar) {
		return ((DMLLVar) ref).request();
	    } else {
		return ref;
	    }
	}
    }

    public java.lang.String toString() {
	DMLValue val=null;
	try{
	    val=this.getValue();
	} catch (java.rmi.RemoteException r) {
	    System.out.println(r);
	}
	if (val instanceof LVar)
	    return "<unresolved>: byneed-future";
	else
	    return val.toString();
    }
}
