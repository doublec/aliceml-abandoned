package de.uni_sb.ps.dml.runtime;

public class DMLLVar extends java.rmi.server.UnicastRemoteObject implements DMLRemoteValue {

    protected DMLValue ref=null;

    public DMLLVar() throws java.rmi.RemoteException { }

    final synchronized public DMLValue getValue() throws java.rmi.RemoteException { // gibt Wert zurück ohne blockieren
	if (ref==null)
	    return this;
	else
	    ref=ref.getValue();
	return ref;
    }

    synchronized public DMLValue request() throws java.rmi.RemoteException { // gibt Wert zurück wenn verfügbar
	if (ref==null)
	    try {
		this.wait();
	    } catch (java.lang.InterruptedException e) {}
	ref=ref.request();
	return ref;
    }

    synchronized public DMLValue bind(DMLValue v) throws java.rmi.RemoteException { // bindet Variable und startet Threads aus suspendVector-Liste
	ref=v;
	this.notifyAll();
	return DMLConstants.dmlunit;
    }

    /** Gleichheit der referenzierten Werte, blockiert auf beiden Werten */
    final public boolean equals(Object val) {
	try {
	    return (val instanceof DMLLVar) && this.request().equals(((DMLLVar) val).request());
	} catch (java.rmi.RemoteException r) {
	    System.err.println(r);
	    return false;
	}
    }

    public String toString() {
	DMLValue val;
	try {
	    val=this.getValue();
	} catch (java.rmi.RemoteException r) {
	    System.err.println(r);
	    return null;
	}
	if (val instanceof DMLLVar) return "<unresolved>: lvar";
	return val.toString();
    }


    public DMLValue apply(DMLValue v)  throws java.rmi.RemoteException{
	return this.request().apply(v);
    }

    final public DMLValue raise()  throws java.rmi.RemoteException {
	throw new DMLExceptionWrapper(this);
    }

    /** DMLLVar und DMLFuture werden beim pickeln ersetzt, falls sie gebunden sind.
	Nicht gebunde logische Variablen dürfen nicht gepickelt werden. */
    final private void writeObject(java.io.ObjectOutputStream out) throws java.io.IOException {
	DMLConstants.runtimeError.apply(new DMLString("cannot pickle DMLLVar")).raise();
    }
}
