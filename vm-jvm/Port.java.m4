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
import java.rmi.server.UnicastRemoteObject;

final public class Port extends UnicastRemoteObject
    implements DMLPort {

    LVar last = null;

    public Port() throws RemoteException { }

    public Port(LVar f) throws RemoteException {
	last = f;
    }

    final public DMLValue send(DMLValue msg) throws RemoteException {
	LVar newLast = new LVar();
	synchronized (last) {
	    last.bind(new Cons(msg, new Future(newLast)));
	    last = newLast;
	}
	return Constants.dmlunit;
    }

    _apply_fails ;

    final public java.lang.String toString() {
	return "port"+hashCode();
    }

    final public java.lang.String toString(int level) {
	return "port"+hashCode();
    }

    _BUILTIN(NewPort) {
	_APPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    return sapply0();
	}
	_SAPPLY0 {
	    try {
		LVar np = new LVar();
		Future captain = new Future(np);
		Port p = new Port(np);
		return new Tuple2(p,captain);
	    } catch (RemoteException r) {
		System.err.println(r);
		return null;
	    }
	}
    }
    /** val newPort: _ -> port */
    _FIELD(Port,newPort);

    _BUILTIN(Send) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _fromTuple(args,val,2,"Port.send");
	}
	_SAPPLY2(v) {
	    _REQUESTDEC(DMLValue p,v1);
	    if (!(p instanceof DMLPort)) {
		_RAISENAME(General.Match);
	    }
	    DMLPort port = (DMLPort) p;
	    try {
		return port.send(v2);
	    } catch (RemoteException r) {
		System.err.println(r);
		return null;
	    }
	}
    }
    /** val send : port * value -> unit */
    _FIELD(Port,send);
}
