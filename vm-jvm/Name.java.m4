package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLName implements DMLValue {

    String name = null;

    public DMLName(String name) {
	super();
	this.name=name;
    }

    public DMLName() {
	super();
	this.name = "unnamed";
    }

    final public String toString() {
	return name+" : name";
    }

    final public boolean equals(Object o) {
	return (this == o);
    }

    final public DMLValue getValue() {
	return this;
    }

    final public DMLValue request() {
	return this;
    }

    final public DMLValue apply(DMLValue v) {
	return DMLConstants.runtimeError.apply(new DMLString("cannot apply "+this+" to "+v)).raise();
    }

    final public DMLValue raise() {
	throw new DMLExceptionWrapper(this);
    }
}
