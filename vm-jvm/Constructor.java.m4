package de.uni_sb.ps.dml.runtime;

final public class Constructor implements DMLValue {

    static java.util.Hashtable gNames = new java.util.Hashtable();
    java.lang.String name = null;
    GName gName = null;

    public Constructor() {
	super();
	this.name="unnamed";
	this.gName = null;
    }

    public Constructor(GName g) {
	super();
	this.name="unnamed";
	this.gName=g;
    }

    public Constructor(java.lang.String name) {
	super();
	this.name=name;
	this.gName = null;
    }

    /** Pointergleichheit */
    final public boolean equals(java.lang.Object val) {
	return (val == this);
    }

    final public java.lang.String toString() {
	return this.name+" : constructor";
    }

    final public DMLValue apply(DMLValue val) {
	return new ConVal(this,val);
    }

    final public DMLValue getValue() {
	return this;
    }

    final public DMLValue request() {
	return this;
    }

    final public DMLValue raise() {
	throw new ExceptionWrapper(this);
    }

    final public GName globalize() {
	if (gName==null) {
	    gName = new GName(1);
	    gNames.put(gName,this);
	    return gName;
	}
	else
	    return gName;
    }
}
