package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLConstructor implements DMLValue {

    String name = null;

    public DMLConstructor() {
	super();
	this.name="unnamed";
    }

    public DMLConstructor(String name) {
	super();
	this.name=name;
    }

    /** Gleichheit des Namens und der Stelligkeit */
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
}
