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

    DMLValue first = null;
    LVar last = null;

    public Port() throws RemoteException {
	last = new LVar();
	first = last;
    }

    final public DMLValue send(DMLValue msg) throws RemoteException {
	LVar newLast = new LVar();
	synchronized (last) {
	    last.bind(new Cons(msg,newLast));
	    last=newLast;
	}
	return Constants.dmlunit;
    }

    final public DMLValue recieve() throws RemoteException {
	DMLValue ret = null;
	synchronized (first) {
	    _REQUESTDEC(DMLValue h, first);
	    ret = ((Cons) h).car;
	    first = ((Cons) h).cdr;
	}
	return ret;
    }

    _apply_fails ;

    final public java.lang.String toString() {
	return first.toString()+": port";
    }

    _BUILTIN(NewPort) {
	_APPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    return sapply0();
	}
	_SAPPLY0 {
	    try {
		return new Port();
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

    _BUILTIN(Recieve) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Port.recieve");
	    if (!(val instanceof DMLPort)) {
		_RAISENAME(General.Match);
	    }
	    DMLPort port = (DMLPort) val;
	    try {
	    return port.recieve();
	    } catch (RemoteException r) {
		System.err.println(r);
		return null;
	    }
	}
    }
    /** val recieve : port -> value */
    _FIELD(Port,recieve);
    _nomatch;
}
