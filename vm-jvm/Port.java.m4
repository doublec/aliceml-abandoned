/*
 * $Date$
 * $Revision$
 * $Author$
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
    _request_id ;
    _getValue_id ;
    _raise ;

    final public java.lang.String toString() {
	return first.toString()+": port";
    }

    _BUILTIN(NewPort) {
	_APPLY(val) {
	    // _FROMSINGLE(val,"Port.newPort");
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
	_APPLY(val) {
	    _fromTuple(args,val,2,"Port.send");
	    _REQUESTDEC(DMLValue p,args[0]);
	    if (!(p instanceof DMLPort)) {
		_error("argument 1 not DMLPort",val);
	    }
	    DMLPort port = (DMLPort) p;
	    try {
		return port.send(args[1]);
	    } catch (RemoteException r) {
		System.err.println(r);
		return null;
	    }
	}
    }
    /** val send : port * value -> unit */
    _FIELD(Port,send);

    _BUILTIN(Recieve) {
	_APPLY(val) {
	    // _FROMSINGLE(val,"Port.recieve");
	    if (!(val instanceof DMLPort)) {
		_error("argument 1 not DMLPort",val);
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
}
