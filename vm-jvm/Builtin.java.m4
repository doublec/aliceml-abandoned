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
	DMLValue b = (DMLValue) builtins.get(name);
	if (b!=null) {
	    return b;
	} else {
	    java.lang.String lib = name.substring(0,name.indexOf('.'));
	    try {
		Class.forName("de.uni_sb.ps.dml.runtime.General");
		Class.forName("de.uni_sb.ps.dml.runtime."+lib);
	    } catch (ClassNotFoundException c) {
		System.err.println("Unknown Library: "+lib);
		c.printStackTrace();
	    }
	    b = (DMLValue) builtins.get(name);
	    if (b==null) {
		System.err.println("WARNING: could not find builtin function: "+name);
	    }
	    return b;
	}
    }

    final public static DMLValue getBuiltin(STRING name) {
	return getBuiltin(name.getString());
    }
}
