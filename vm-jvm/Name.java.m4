package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLName extends DMLValue {

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

    final public DMLValue apply(DMLValue v) {
	throw DMLConstants.runtimeerror.apply(new DMLString("cannot apply "+this+" to "+v));
    }
}
