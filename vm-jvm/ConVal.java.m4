package de.uni_sb.ps.dml.runtime;

final public class ConVal implements DMLConVal {

    DMLValue content=null;

    DMLConstructor constructor=null;

    public ConVal(DMLConstructor constructor, DMLValue content) {
	this.constructor = constructor;
	this.content = content;
    }

    public DMLValue get0() {
	if (content instanceof DMLTuple)
	    return ((DMLTuple) content).get0();
	else
	    throw new ArrayIndexOutOfBoundsException();
    }
    public DMLValue get1() {
	if (content instanceof DMLTuple)
	    return ((DMLTuple) content).get1();
	else
	    throw new ArrayIndexOutOfBoundsException();
    }
    public DMLValue get2() {
	if (content instanceof DMLTuple)
	    return ((DMLTuple) content).get2();
	else
	    throw new ArrayIndexOutOfBoundsException();
    }
    public DMLValue get3() {
	if (content instanceof DMLTuple)
	    return ((DMLTuple) content).get3();
	else
	    throw new ArrayIndexOutOfBoundsException();
    }
    public DMLValue get4() {
	if (content instanceof DMLTuple)
	    return ((DMLTuple) content).get4();
	else
	    throw new ArrayIndexOutOfBoundsException();
    }

    /** Gleichheit der  und Inhalte */
    final public boolean equals(Object val) {
	return (val instanceof ConVal) &&
	    (this.constructor == ((ConVal)val).constructor) &&
	    this.content.equals(((ConVal)val).content);
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
	    try {
		return DMLConstants.runtimeError.apply(new DMLString("cannot assign "+val+" to "+this)).raise();
	    } catch (java.rmi.RemoteException r) {
		System.err.println(r);
		return null;
	    }
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

    final public DMLValue apply(DMLValue v) throws java.rmi.RemoteException {
	try {
	    return DMLConstants.runtimeError.apply(new DMLString("cannot apply "+this+" to "+v)).raise();
	} catch (java.rmi.RemoteException r) {
	    System.out.println(r);
	    return null;
	}
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
