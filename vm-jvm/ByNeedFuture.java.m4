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
    private int state = 0; // 0 - unbound, 1 ByNeed error, 2 = bound
    public ByNeedFuture(DMLValue v) throws java.rmi.RemoteException {
	super();
	closure=v;
	ref = new LVar();
    }

    synchronized public DMLValue request() throws java.rmi.RemoteException {
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

    final public static Constructor ByNeed = new UniqueConstructor("ByNeed");
}
