package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLCoEx0 extends DMLCoEx {

    public DMLCoEx0(String name) {
	super();
	this.name=new DMLCoExName(name,0);
    }

    public DMLCoEx0(DMLCoExName en) {
	super();
	this.name = en;
    }

    final public String toString() {
	return name+"(-) : exn";
    }
}
