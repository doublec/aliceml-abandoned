package de.uni_sb.ps.dml.runtime;

public class Name implements DMLValue {

    java.lang.String name = null;
    GName gName = null;

    public Name(java.lang.String name) {
	super();
	this.name=name;
	this.gName=null;
    }

    public Name() {
	super();
	this.name = "unnamed";
	this.gName=null;
    }

    public Name(GName g) {
	super();
	this.name = "unnamed";
	this.gName= g;
    }

    final public java.lang.String toString() {
	return name+" : name";
    }

    final public boolean equals(java.lang.Object o) {
	return (this == o);
    }

    final public DMLValue getValue() {
	return this;
    }

    final public DMLValue request() {
	return this;
    }

    final public DMLValue apply(DMLValue v) throws java.rmi.RemoteException {
	return Constants.runtimeError.apply(new de.uni_sb.ps.dml.runtime.String("cannot apply "+this+" to "+v)).raise();
    }

    final public DMLValue raise() {
	throw new ExceptionWrapper(this);
    }
    final public GName globalize() {
	if (gName==null) {
	    gName = new GName(0);
	    Constructor.gNames.put(gName,this);
	    return gName;
	}
	else
	    return gName;
    }
}
