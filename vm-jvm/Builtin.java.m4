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
	    int dot = name.indexOf('.');
	    java.lang.String lib = (dot > 0 ? name.substring(0,dot) : "General");
	    try {
		System.out.println("Trying to create class "+lib);
		if (lib.equals("String")) {
		    new STRING ("");
		} else if (lib.equals("Char")) {
		    new Char(' ');
		} else if (lib.equals("Int")) {
		    new Int(0);
		} else if (lib.equals("Real")) {
		    new Real(0.0f);
		} else if (lib.equals("Real")) {
		    new Word(0);
		} else {
		    Class.forName("de.uni_sb.ps.dml.runtime."+lib).newInstance();
		}
	    } catch (ClassNotFoundException c) {
		System.err.println("Unknown Library: "+lib);
		c.printStackTrace();
	    } catch (Exception e) {
		System.err.println(e);
		e.printStackTrace();
	    }
	    b = (DMLValue) builtins.get(name);
	    if (b==null) {
		System.err.println("WARNING: could not find builtin function: "+name);
	    }
	    return b;
	}
    }

    final public static DMLValue getBuiltin(STRING name) {
	return getBuiltin(name.value);
    }
}
