package de.uni_sb.ps.dml.builtin;

import de.uni_sb.ps.dml.runtime.*;
import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.RMISecurityManager;
import java.rmi.server.*;

interface Export extends java.rmi.Remote {
    public DMLValue get(java.lang.String what) throws java.rmi.RemoteException;
}

final class Exporter extends java.rmi.server.UnicastRemoteObject implements Export {

    java.util.Hashtable hash = null;

    public Exporter() throws java.rmi.RemoteException {}

    public Exporter(java.util.Hashtable h) throws java.rmi.RemoteException {
	hash=h;
    }

    public DMLValue get(java.lang.String what) throws java.rmi.RemoteException {
	return (DMLValue) hash.get(what);
    }
}
final public class Connection {
    static java.util.Hashtable export = null;
    static Exporter exp = null;

    final public static class Offer extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException {
	    DMLValue[] args=fromTuple(val,1,"Connection.offer");
	    java.net.InetAddress i=null;
	    try {
		i=java.net.InetAddress.getLocalHost();
	    } catch (java.net.UnknownHostException u) {
		System.err.println(u);
		u.printStackTrace();
		return null;
	    }

	    if (export==null) { // starten des Servers
		if (System.getSecurityManager() == null) {
		    System.setSecurityManager(new RMISecurityManager());
		}
		export = new java.util.Hashtable();
		exp = new Exporter(export);
		//hier noch properties setzen, evtl. Klassen in die codebase schreiben

		java.util.Properties prop = System.getProperties();
		java.lang.String home = (java.lang.String) prop.get("user.home");
		java.lang.String name = (java.lang.String) prop.get("user.name");
		prop.put("java.rmi.server.codebase",
			 "http://"+i.getHostName()+"/~"+name+"/codebase/"); // der letzte / ist wichtig
		prop.put("java.security.policy",
			 "http://"+i.getHostName()+"/~"+name+"/codebase/policy");
		PickleClassLoader.loader.writeCodebase(home+"/public_html/codebase");
		java.rmi.registry.LocateRegistry.createRegistry(1099); // am Standardport
		try {
		    Naming.rebind("//localhost/exporter",exp);
		} catch (java.net.MalformedURLException m) {
		    System.err.println(m);
		    m.printStackTrace();
		}
	    }
	    java.lang.String ticket = Double.toString(java.lang.Math.random());
	    export.put(ticket,args[0]);
	    return new de.uni_sb.ps.dml.runtime.String(i.getHostAddress()+"\\"+ticket);
	}
    }

    final public static Offer offer = new Offer();

    final public static class Take extends Builtin {
	final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException {
	    DMLValue[] args=fromTuple(val,1,"Connection.take");
	    DMLValue t = args[0].request();
	    if (!(t instanceof de.uni_sb.ps.dml.runtime.String)) {
		return error("argumetn not String",val);
	    }
	    java.lang.String ti = ((de.uni_sb.ps.dml.runtime.String) t).getString();
	    java.lang.String ip = ti.substring(0,ti.indexOf('\\'));
	    java.lang.String ticket = ti.substring(ti.indexOf('\\')+1);
	    Exporter exp = null;
	    try {
		exp = (Exporter) Naming.lookup("//"+ip+"/exporter");
	    } catch (java.rmi.NotBoundException n) {
		return error("ticket not bound",val);
	    } catch (java.net.MalformedURLException m) {
		System.err.println(m);
		m.printStackTrace();
		return null;
	    }
	    return (DMLValue) exp.get(ticket);
	}
    }

    final public static Take take = new Take();


	    // Hilfsfunktionen
    final public static DMLValue[] fromTuple
	(DMLValue v, // Value-Tuple
	 int ea,     // erwartete Anzahl Argumente
	 java.lang.String errMsg) throws java.rmi.RemoteException {
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

    final public static DMLValue error
	(java.lang.String msg, DMLValue v) throws java.rmi.RemoteException {
	// sonst: Fehler
	DMLValue[] err = {
	    new de.uni_sb.ps.dml.runtime.String(msg),
	    v};
	return Constants.
	    runtimeError.apply(new Tuple(err)).raise();
    }
}
