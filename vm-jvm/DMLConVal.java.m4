package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLException1 extends DMLException {

  DMLValue content=null;

    public DMLException1(String name, DMLValue content) {
	super();
	this.name = name;
	this.content = content;
    }

    public DMLException1(DMLExName exname, DMLValue content) {
	super();
	if (exname.arity!=1) throw new DMLRuntimeError("operator exception/0 is not a function.");
	this.name=exname.name;
	this.content=content;
    }

  /** Gleichheit der Namen und Inhalte */
  final public boolean equals(Object val) {
    return (val instanceof DMLException1) &&
      this.name.equals(((DMLException1)val).name) &&
      this.content.equals(((DMLException1)val).content);
  }

  final public DMLValue getContent() {
    return content;
  }

  final public String toString() {
    return name+"("+content+") : exn";
  }
}
