package de.uni_sb.ps.DML.DMLRuntime;

abstract public class DMLFunction implements DMLValue {

    public DMLFunction() {
	super();
    }

    /** Gleicheit der FQ-Klassennamen */
    final public boolean equals(Object val) {
	return this.getClass().equals(val.getClass());
    }

    final public String toString() {
	return "compiled function: "+this.getClass();
    }

    final public DMLValue getValue() {
	return this;
    }

    final public DMLValue request() {
	return this;
    }

    final public DMLValue raise() {
	throw new DMLExceptionWrapper(this);
    }
}
