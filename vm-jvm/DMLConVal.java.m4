package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLCoEx1 extends DMLCoEx {

  DMLValue content=null;

    public DMLCoEx1(String name, DMLValue content) {
	super();
	this.name = new DMLCoExName(name,1);
	this.content = content;
    }

    public DMLCoEx1(DMLCoExName exname, DMLValue content) {
	super();
	if (exname.arity!=1) throw new DMLCoEx1(DMLConstants.runtimeError,new DMLString("operator exception/0 is not a function."));
	this.name=exname;
	this.content=content;
    }

    /** Gleichheit der Namen und Inhalte */
    final public boolean equals(Object val) {
	return (val instanceof DMLCoEx1) &&
	    (this.name == ((DMLCoEx1)val).name) &&
	    this.content.equals(((DMLCoEx1)val).content);
    }
    
    final public DMLValue getContent() {
	return content;
    }
    
  final public String toString() {
    return name+"("+content+") : exn";
  }
}
