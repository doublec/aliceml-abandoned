package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLConVal implements DMLValue {

    DMLValue content=null;
    DMLConstructor constructor=null;

    public DMLConVal(DMLConstructor constructor, DMLValue content) {
	super();
	this.constructor = constructor;
	this.content = content;
    }

    /** Gleichheit der  und Inhalte */
    final public boolean equals(Object val) {
	return (val instanceof DMLConVal) &&
	    (this.constructor == ((DMLConVal)val).constructor) &&
	    this.content.equals(((DMLConVal)val).content);
    }

    final public DMLValue getContent() {
	return content;
    }

    final public String toString() {
	return constructor+"("+content+") : constructed value";
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
