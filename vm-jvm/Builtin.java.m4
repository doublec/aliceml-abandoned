package de.uni_sb.ps.dml.runtime;

abstract public class Builtin implements DMLValue {
    public Builtin() {
	super();
    }

    /** Gleicheit der FQ-Klassennamen */
    final public boolean equals(java.lang.Object val) {
	return this.getClass().equals(val.getClass());
    }

    final public java.lang.String toString() {
	return "builtin function: "+this.getClass();
    }

    _request_id ;
    _getValue_id ;
    _raise ;
}
