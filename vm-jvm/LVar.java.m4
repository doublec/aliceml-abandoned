package de.uni_sb.ps.DML.DMLRuntime;

public class DMLLVal extends DMLValue {

    public DMLLVal() {
	super();
    }

    final public DMLValue getValue() { // gibt Wert zurück ohne blockieren
	if (ref==null)
	    return this;
	else
	    if (ref instanceof DMLLVal)
		return ref.getValue();
	    else
		return ref;
    }

    final public DMLValue request() { // gibt Wert zurück wenn verfügbar
	if (ref==null)
	    this.wait();
	if (ref instanceof DMLLVal) return ref.request();
	else return ref.getValue();
    }

    /** Gleichheit der referenzierten Werte, blockiert auf beiden Werten */
    final public boolean equals(Object val) {
	return (val instanceof DMLLVal) && this.request().equals(((DMLLVal) val).request());
    }

    protected DMLValue ref=null;

    public DMLValue bind(DMLValue v) { // bindet Variable und startet Threads aus suspendVector-Liste
	ref=v;
	this.notifyAll();
	return DMLConstants.dmlunit;
    }

    public String toString() {
	DMLValue val=this.getValue();
	if (val instanceof DMLLVal) return "<unresolved>: lval";
	return val.toString();
    }

    public DMLValue apply(DMLValue val) {
	throw new DMLCoEx1(DMLConstants.runtimeError, new DMLString("logic value cannot be applied.\n\t"+this+" applied to "+val));
    }

}
