/*
 * Author:
 *      Daniel Simon, <dansim@ps.uni-sb.de>
 *
 * Copyright:
 *      Daniel Simon, 1999
 *
 * Last change:
 *    $Date$ by $Author$
 * $Revision$
 *
 */
package de.uni_sb.ps.dml.runtime;

import java.rmi.RemoteException;

final public class ConVal implements DMLConVal {

    public DMLValue content = null;

    final public Constructor constructor;

    public ConVal(Constructor con) {
	constructor = con;
    }

    public ConVal(Constructor constructor, DMLValue content) {
	this.constructor = constructor;
	this.content = content;
    }

    final public DMLValue get0() throws RemoteException {
	_REQUEST(content,content);
	if (content instanceof DMLTuple)
	    return ((DMLTuple) content).get0();
	else
	    throw new ArrayIndexOutOfBoundsException();
    }

    final public DMLValue get1() throws RemoteException {
	_REQUEST(content,content);
	if (content instanceof DMLTuple)
	    return ((DMLTuple) content).get1();
	else
	    throw new ArrayIndexOutOfBoundsException();
    }

    final public DMLValue get2() throws RemoteException {
	_REQUEST(content,content);
	if (content instanceof DMLTuple)
	    return ((DMLTuple) content).get2();
	else
	    throw new ArrayIndexOutOfBoundsException();
    }

    final public DMLValue get3() throws RemoteException {
	_REQUEST(content,content);
	if (content instanceof DMLTuple)
	    return ((DMLTuple) content).get3();
	else
	    throw new ArrayIndexOutOfBoundsException();
    }

    /** Gleichheit der  und Inhalte */
    final public boolean equals(Object val) {
	try {
	    if (val instanceof DMLConVal) {
		return (constructor == ((DMLConVal) val).getConstructor()) &&
		    content.equals(((DMLConVal) val).getContent());
	    } else if (val instanceof DMLTransient) {
		return val.equals(this);
	    } else {
		return false;
	    }
	} catch (RemoteException r) {
	    System.err.println(r);
	    r.printStackTrace();
	    return false;
	}
    }

    final public DMLValue getContent() {
	return content;
    }

    final public java.lang.String toString() {
	return constructor+"("+content+")";
    }

    final private void writeObject(java.io.ObjectOutputStream out)
	throws java.io.IOException {
	if (this.constructor == Constants.reference) {
	    _RAISE(runtimeError,
		   new STRING ("cannot pickle reference"+this.toString()));
	}
	else {
	    out.defaultWriteObject();
	}
    }

    _getConstructor ;
    _apply_fails ;
}
