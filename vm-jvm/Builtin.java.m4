/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

abstract public class Builtin implements DMLValue {

    public static java.util.Hashtable builtins = new java.util.Hashtable();

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

    final public static DMLValue getBuiltin(java.lang.String name) {
	return (DMLValue) builtins.get(name);
    }
}
