package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLFuture extends DMLLVar {

    public DMLFuture(DMLValue v) {
	super();
	ref=v;
    }

    /** bind ist nicht erlaubt und wirft RuntimeError */
    final public DMLValue bind(DMLValue v) {
	return DMLConstants.runtimeError.apply(new DMLString("cannot bind future to "+v)).raise();
    }

    final public String toString() {
	DMLValue val=this.getValue();
	if (val instanceof DMLLVar)
	    return "<unresolved>: future";
	else
	    return val.toString();
    }

    final public DMLValue apply(DMLValue val) {
	return ref.apply(val);
    }
}
