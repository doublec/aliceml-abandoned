package de.uni_sb.ps.DML.DMLRuntime;

public class DMLLVar implements DMLValue {

    protected DMLValue ref=null;

    public DMLLVar() {
	super();
    }

    final synchronized public DMLValue getValue() { // gibt Wert zurück ohne blockieren
	if (ref==null)
	    return this;
	else
	    ref=ref.getValue();
	return ref;
    }

    synchronized public DMLValue request() { // gibt Wert zurück wenn verfügbar
	if (ref==null)
	    try {
		this.wait();
	    } catch (java.lang.InterruptedException e) {}
	ref=ref.request();
	return ref;
    }

    synchronized public DMLValue bind(DMLValue v) { // bindet Variable und startet Threads aus suspendVector-Liste
	ref=v;
	this.notifyAll();
	return DMLConstants.dmlunit;
    }

    /** Gleichheit der referenzierten Werte, blockiert auf beiden Werten */
    final public boolean equals(Object val) {
	return (val instanceof DMLLVar) && this.request().equals(((DMLLVar) val).request());
    }

    public String toString() {
	DMLValue val=this.getValue();
	if (val instanceof DMLLVar) return "<unresolved>: lvar";
	return val.toString();
    }


    public DMLValue apply(DMLValue v) {
	return this.request().apply(v);
    }

    final public DMLValue raise() {
	throw new DMLExceptionWrapper(this);
    }

    /** DMLLVar und DMLFuture werden beim pickeln ersetzt, falls sie gebunden sind.
	Nicht gebunde logische Variablen dürfen nicht gepickelt werden. */
    final private void writeObject(java.io.ObjectOutputStream out) throws java.io.IOException {
	DMLConstants.runtimeError.apply(new DMLString("cannot pickle DMLLVar")).raise();
    }
}
