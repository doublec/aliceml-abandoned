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
	if (closure == null) {
	    return ref.request();
	}
	else {
	    DMLValue temp = closure;
	    DMLValue v = null;
	    closure = null;
	    boolean hasSelfRef = false;
	    try {
		v = temp.apply(Constants.dmlunit);

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
		} else {
		    ref.bind(v);
		}
	    } catch (Throwable t) {
		System.err.println(t);
	    }

	    if (hasSelfRef) {
		closure = temp;
		return this;
	    } else {
		return ref.request();
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
	if (val instanceof LVar) {
	    return "<unresolved>: byneed-future";
	} else {
	    return val.toString();
	}
    }
}
