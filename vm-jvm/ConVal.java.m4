/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

final public class ConVal implements DMLConVal {

    private DMLValue content = null;

    final public Constructor constructor;

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
	try {
	return (val instanceof DMLConVal) &&
	    (constructor == ((DMLConVal)val).getConstructor()) &&
	    content.equals(((DMLConVal)val).getContent());
	} catch (java.rmi.RemoteException r) {
	    System.err.println(r);
	    r.printStackTrace();
	    return false;
	}
    }

    final public void setContent(DMLValue val) {
	if (content == null) {
	    content=val;
	} else {
	    try {
		_RAISE(runtimeError, new STRING ("cannot set content twice"));
	    } catch (java.rmi.RemoteException r) {
		System.err.println(r);
		r.printStackTrace();
	    }
	}
    }

    final public DMLValue getContent() {
	return content;
    }

    final public java.lang.String toString() {
	return constructor+"("+content+")";
    }

    final private void writeObject(java.io.ObjectOutputStream out) throws java.io.IOException {
	if (this.constructor == Constants.reference) {
	    _RAISE(runtimeError,new STRING ("cannot pickle reference"+this.toString()));
	}
	else {
	    out.defaultWriteObject();
	}
    }

    _getConstructor ;
    _apply_fails ;
}
