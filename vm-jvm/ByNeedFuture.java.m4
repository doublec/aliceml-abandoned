package de.uni_sb.ps.DML.DMLRuntime;

public class DMLByNeedFuture extends DMLFuture {
    // von Future: DMLValue ref = null;
    // ref kann hier nur DMLFunction : unit -> 'a  sein.
    // diese Bedingung wird nicht geprüft

    private boolean applied = false;

    public DMLByNeedFuture(DMLValue v) {
	super(v); // einfach den Konstruktor von DMLFuture verwenden
    }

    synchronized public DMLValue request() {
	if (applied)
	    return ref;
	else {
	    ref=ref.apply(DMLConstants.dmlunit);
	    applied=true;
	    return ref;
	}
    }

    public String toString() {
	DMLValue val=this.getValue();
	if (val instanceof DMLLVar)
	    return "<unresolved>: byneed-future";
	else
	    return val.toString();
    }
}
