package de.uni_sb.ps.dml.runtime;

final public class JObject implements DMLValue {

    java.lang.Object javaObject = null;

    public JObject(java.lang.Object o) {
	super();
	javaObject=o;
    }

    final public DMLValue getValue() {
	return this;
    }

    final public DMLValue request() {
	return this;
    }

    final public DMLValue apply(DMLValue v) throws java.rmi.RemoteException {
	return Constants.runtimeError.apply( new de.uni_sb.ps.dml.runtime.String("cannot apply "+this+" to "+v)).raise();
    }

    final public DMLValue raise() {
	throw new ExceptionWrapper(this);
    }

    final public java.lang.Object getObject() {
	return javaObject;
    }

    final public java.lang.String toString() {
	return javaObject.getClass().getName()+": "+javaObject+" : APIObject";
    }
}
