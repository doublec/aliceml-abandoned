package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLException0 extends DMLException {

    public DMLException0(String name) {
	super();
	this.name=name;
    }

    public DMLException0(DMLExName en) {
	super();
	this.name = en.name;
    }

    /** Gleichheit der Namen */
    final public boolean equals(Object val) {
	return (val instanceof DMLException1) &&
	    this.name.equals(((DMLException1) val).name);
    }

    final public String toString() {
	return name+"(-) : exn";
    }
}
