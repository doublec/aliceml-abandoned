package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLCoExName extends DMLValue {

  public DMLCoExName(String name, int arity) {
    super();
    this.name=name;
    this.arity=arity;
  }

    final public DMLValue getValue() {
	return this;
    }
    
    final public DMLValue request() {
	return this;
    }
    
    /** Gleichheit des Namens und der Stelligkeit */
    final public boolean equals(Object val) {
	return (val instanceof DMLCoExName) &&
	    ((DMLCoExName)val).name.equals(this.name) &&
	    this.arity==((DMLCoExName)val).arity;
    }
    
    final public String toString() {
	return this.name+"/"+this.arity+" : constructor/exceptionname";
    }
    
    final public DMLValue apply(DMLValue val) {
	if (arity == 0)
	    throw new DMLCoEx1(DMLConstants.runtimeError,new DMLString("nullary exceptionname cannot be applied.\n\t"+this+" applied to "+val));
	else
	    return new DMLCoEx1(name,val);
    }
    
    String name=null;
    int arity=0;
}
