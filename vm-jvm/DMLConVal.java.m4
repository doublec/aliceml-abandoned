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

    /** setzt Wert auf val und gibt alten Wert zurueck */
    final public DMLValue assign(DMLValue val) {
	if (this.constructor == DMLConstants.reference) {
	    DMLValue v=this.content;
	    this.content=val;
	    return DMLConstants.dmlunit;
	}
	else
	    return DMLConstants.runtimeError.apply(new DMLString("cannot assign "+val+" to "+this)).raise();
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

    final private void writeObject(java.io.ObjectOutputStream out) throws java.io.IOException {
	if (this.constructor == DMLConstants.reference)
	    DMLConstants.runtimeError.apply(new DMLString("cannot pickle referencev")).raise();
	else
	    out.defaultWriteObject();
    }

    final public DMLConstructor getConstructor() {
	return constructor;
    }
}
