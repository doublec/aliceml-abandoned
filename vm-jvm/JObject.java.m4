package de.uni_sb.ps.dml.runtime;

final public class DMLObject implements DMLValue {

    Object javaObject = null;

    public DMLObject(Object o) {
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
	return DMLConstants.runtimeError.apply( new DMLString("cannot apply "+this+" to "+v)).raise();
    }

    final public DMLValue raise() {
	throw new DMLExceptionWrapper(this);
    }

    final public Object getObject() {
	return javaObject;
    }

    final public String toString() {
	return javaObject.getClass().getName()+": "+javaObject+" : APIObject";
    }
}
