package de.uni_sb.ps.dml.runtime;

public class DMLByNeedFuture extends DMLFuture {
    // von Future: DMLValue ref = null;
    // ref kann hier nur DMLFunction : unit -> 'a  sein.
    // diese Bedingung wird nicht geprüft

    private DMLValue closure = null;
    private DMLLVar ref = null;

    public DMLByNeedFuture(DMLValue v) {
	closure=v;
	ref = new DMLLVar();
    }

    synchronized public DMLValue request() throws java.rmi.RemoteException {
	if (closure==null)
	    return ref.request();
	else {
	    DMLValue temp = closure;
	    closure = null;
	    try {
		ref.bind(temp.apply(DMLConstants.dmlunit));
	    } catch (Throwable t) {
		System.err.println(t);
	    }
	    return ref.request();
	}
    }

    public String toString() {
	DMLValue val=null;
	try{
	    val=this.getValue();
	} catch (java.rmi.RemoteException r) {
	    System.out.println(r);
	}
	if (val instanceof DMLLVar)
	    return "<unresolved>: byneed-future";
	else
	    return val.toString();
    }
}
