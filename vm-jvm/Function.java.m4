/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

abstract public class Function implements DMLValue {

    static public boolean DEBUG = false;

    _BUILTIN(Debug) {
	_APPLY(val) {
	    DEBUG=true;
	    return null;
	}
    }
    _FIELD(Function,debug);

    public Function() {
	super();
    }

    /** Gleicheit der FQ-Klassennamen */
    final public boolean equals(java.lang.Object val) {
	return this.getClass().equals(val.getClass());
    }

    final public java.lang.String toString() {
	return "compiled function: "+this.getClass();
    }
}
