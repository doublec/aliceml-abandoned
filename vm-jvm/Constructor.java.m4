package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLConstructor implements DMLValue {

    static java.util.Hashtable gNames = new java.util.Hashtable();
    String name = null;
    GName gName = null;

    public DMLConstructor() {
	super();
	this.name="unnamed";
	this.gName = null;
    }

    public DMLConstructor(GName g) {
	super();
	this.name="unnamed";
	this.gName=g;
    }

    public DMLConstructor(String name) {
	super();
	this.name=name;
	this.gName = null;
    }

    /** Pointergleichheit */
    final public boolean equals(Object val) {
	return (val == this);
    }

    final public String toString() {
	return this.name+" : constructor";
    }

    final public DMLValue apply(DMLValue val) {
	return new DMLConVal(this,val);
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
