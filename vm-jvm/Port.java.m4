package de.uni_sb.ps.dml.builtin;

import de.uni_sb.ps.dml.runtime.*;
import java.rmi.RemoteException;

final public class Port {
    final public static class NewPort extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    DMLValue[] args=fromTuple(val,1,"Port.newPort");
	    return new DMLPort();
	}
    }
    final public static NewPort newPort = new NewPort();

    final public static class Send extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    DMLValue[] args=fromTuple(val,2,"Port.send");
	    DMLValue p = args[0].request();
	    if (!(p instanceof DMLPort))
		return error("argument #1 not DMLPort",val);
	    DMLPort port = (DMLPort) p;
	    try {
		return port.send(args[1]);
	    } catch (RemoteException r) {
		System.err.println(r);
		return null;
	    }
	}
    }
    final public static Send send = new Send();

    final public static class Recieve extends DMLBuiltin {
	final public DMLValue apply(DMLValue val) {
	    DMLValue[] args=fromTuple(val,1,"Port.recieve");
	    DMLValue p = args[0].request();
	    if (!(p instanceof DMLPort))
		return error("argument #1 not DMLPort",val);
	    DMLPort port = (DMLPort) p;
	    try {
		return port.recieve();
	    } catch (RemoteException r) {
		System.err.println(r);
		return null;
	    }
	}
    }
    final public static Recieve recieve = new Recieve();

    // Hilfsfunktionen
    final public static DMLValue[] fromTuple
	(DMLValue v, /** <code>value-Tuple</code>*/
	 int ea,     // erwartete Anzahl Argumente
	 java.lang.String errMsg) {
	v=v.request();
	if (v instanceof DMLTuple) {
	    DMLTuple t=(DMLTuple) v;
	    if (t.getArity()==ea) {
		DMLValue[] vals = new DMLValue[ea];
		for(int i=0; i<ea; i++)
		    vals[i]=t.getByIndex(i);
		return vals;
	    }
	    else
		error("wrong number of arguments in "+errMsg, v);
	}
	else
	    error("wrong argument type for "+errMsg,v);
	return null;
    }

    final protected static DMLValue error
	(java.lang.String msg, DMLValue v) {
	// sonst: Fehler
	DMLValue[] err = {
	    new DMLString(msg),
	    v};
	return DMLConstants.
	    runtimeError.apply(new Tuple(err)).raise();
    }
}
