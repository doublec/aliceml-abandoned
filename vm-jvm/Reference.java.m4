package de.uni_sb.ps.dml.runtime;

final public class Reference implements DMLConVal, DMLReference {

    DMLValue content=null;

    public Reference(DMLValue content) {
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
	return (val instanceof Reference) &&
	    this.content.equals(((Reference)val).content);
    }

    final public DMLValue getContent() {
	return content;
    }

    /** setzt Wert auf val und gibt alten Wert zurueck */
    final public DMLValue assign(DMLValue val) {
	DMLValue v=this.content;
	this.content=val;
	return DMLConstants.dmlunit;
    }

    final public String toString() {
	return content+" : ref";
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

    final public DMLConstructor getConstructor() {
	return DMLConstants.reference;
    }
}
