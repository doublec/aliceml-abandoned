/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

final public class ConVal implements DMLConVal {

    DMLValue content=null;

    Constructor constructor=null;

    public ConVal(Constructor con) {
	constructor = con;
    }

    public ConVal(Constructor constructor, DMLValue content) {
	this.constructor = constructor;
	this.content = content;
    }

    final public DMLValue get0() {
	if (content instanceof DMLTuple)
	    return ((DMLTuple) content).get0();
	else
	    throw new ArrayIndexOutOfBoundsException();
    }
    final public DMLValue get1() {
	if (content instanceof DMLTuple)
	    return ((DMLTuple) content).get1();
	else
	    throw new ArrayIndexOutOfBoundsException();
    }
    final public DMLValue get2() {
	if (content instanceof DMLTuple)
	    return ((DMLTuple) content).get2();
	else
	    throw new ArrayIndexOutOfBoundsException();
    }
    final public DMLValue get3() {
	if (content instanceof DMLTuple)
	    return ((DMLTuple) content).get3();
	else
	    throw new ArrayIndexOutOfBoundsException();
    }
    final public DMLValue get4() {
	if (content instanceof DMLTuple)
	    return ((DMLTuple) content).get4();
	else
	    throw new ArrayIndexOutOfBoundsException();
    }

    /** Gleichheit der  und Inhalte */
    final public boolean equals(java.lang.Object val) {
	return (val instanceof ConVal) &&
	    (this.constructor == ((ConVal)val).constructor) &&
	    this.content.equals(((ConVal)val).content);
    }

    final public void setContent(DMLValue eins) {
	content=eins;
    }

    final public DMLValue getContent() {
	return content;
    }

    /** setzt Wert auf val und gibt alten Wert zurueck */
    final public DMLValue assign(DMLValue val) {
	if (this.constructor == Constants.reference) {
	    DMLValue v=this.content;
	    this.content=val;
	    return Constants.dmlunit;
	}
	else
	    try {
		return Constants.runtimeError.apply(new de.uni_sb.ps.dml.runtime.String("cannot assign "+val+" to "+this)).raise();
	    } catch (java.rmi.RemoteException r) {
		System.err.println(r);
		return null;
	    }
    }

    final public java.lang.String toString() {
	return constructor+"("+content+") : constructed value";
    }

    final private void writeObject(java.io.ObjectOutputStream out) throws java.io.IOException {
	if (this.constructor == Constants.reference)
	    Constants.runtimeError.apply(new de.uni_sb.ps.dml.runtime.String("cannot pickle referencev")).raise();
	else
	    out.defaultWriteObject();
    }

    _getConstructor ;
    _apply_fails ;
    _request_id ;
    _getValue_id ;
    _raise ;
}
