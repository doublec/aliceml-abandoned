/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.RMISecurityManager;
import java.rmi.server.*;

final public class Connection {
    static java.util.Hashtable export = null;
    static Exporter exp = null;
    static java.util.Random rand = null;

    _BUILTIN(Offer) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"Connection.offer");
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
		rand = new java.util.Random(42);
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
	    java.lang.String ticket = Long.toString(rand.nextLong());
	    export.put(ticket,args[0]);
	    return new STRING (i.getHostAddress()+"\\"+ticket);
	}
    }
    /** val offer : value -> ticket */
    _FIELD(Connection,offer);

    _BUILTIN(Take) {
	_APPLY(val) {
	    _fromSingle(val,"Connection.take");
	    if (!(val instanceof STRING)) {
		_error("argument not String",val);
	    }
	    java.lang.String ti = ((STRING) val).value;
	    java.lang.String ip = ti.substring(0,ti.indexOf('\\'));
	    java.lang.String ticket = ti.substring(ti.indexOf('\\')+1);
	    Exporter exp = null;
	    try {
		exp = (Exporter) Naming.lookup("//"+ip+"/exporter");
	    } catch (java.rmi.NotBoundException n) {
		_error("ticket not bound",val);
	    } catch (java.net.MalformedURLException m) {
		System.err.println(m);
		m.printStackTrace();
		return null;
	    }
	    return (DMLValue) exp.get(ticket);
	}
    }
    /** val take : ticket -> value */
    _FIELD(Connection,take);
}
