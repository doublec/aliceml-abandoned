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

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.RMISecurityManager;
import java.rmi.server.*;

final public class Connection {
    static java.util.Hashtable export = null;
    static Exporter exp = null;
    static java.util.Random rand = null;

    _BUILTIN(Offer) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // FROMSINGLE(args,val,1,"Connection.offer");
	    java.net.InetAddress i=null;
	    try {
		i=java.net.InetAddress.getLocalHost();
	    } catch (java.net.UnknownHostException u) {
		System.err.println(u);
		u.printStackTrace();
		return null;
	    }

	    if (export==null) { // starten des Servers
		java.util.Properties prop = System.getProperties();
		java.lang.String home = (java.lang.String) prop.get("user.home");
		java.lang.String name = (java.lang.String) prop.get("user.name");
		// System.out.println("setze properties");
		// prop.put("java.rmi.server.codebase",
		// "http://"+i.getHostName()+"/~"+name+"/codebase/"); // der letzte / ist wichtig
		prop.put("java.security.policy",
			 "http://"+i.getHostName()+"/~"+name+"/codebase/policy");
		// System.out.println("schreibe klassen");
		// PickleClassLoader.loader.writeCodebase(home+"/public_html/codebase");

		if (System.getSecurityManager() == null) {
		    System.out.println("starte security manager");
		    try {
			System.setSecurityManager(new RMISecurityManager());
		    } catch (Exception e) {
			System.err.println("could not install security manager");
			System.err.println("Policy-file used: "+prop.get("java.security.policy"));
			return new STRING ("invalid ticket");
		    }
		}
		export = new java.util.Hashtable();
		exp = new Exporter(export);
		rand = new java.util.Random(42);

		// System.out.println("starte registry");
		java.rmi.registry.LocateRegistry.createRegistry(1099); // am Standardport
		try {
		    // System.out.println("binde exporter in registry");
		    Naming.rebind("//localhost/exporter",exp);
		} catch (java.net.MalformedURLException m) {
		    System.err.println(m);
		    m.printStackTrace();
		}
	    }
	    java.lang.String ticket = Long.toString(rand.nextLong());
	    export.put(ticket,val);
	    return new STRING (i.getHostAddress()+"\\"+ticket);
	}
    }
    /** val offer : value -> ticket */
    _FIELD(Connection,offer);

    _BUILTIN(Take) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Connection.take");
	    if (!(val instanceof STRING)) {
		_error("argument not String",val);
	    }
	    java.lang.String ti = ((STRING) val).value;
	    // System.out.println("ti = "+ti);
	    int indexofbackslash = ti.indexOf('\\');
	    if (indexofbackslash < 0) {
		_error("ticket invalid",val);
	    }
	    java.lang.String ip = ti.substring(0,indexofbackslash);
	    // System.out.println("ip = "+ip);
	    java.lang.String ticket = ti.substring(indexofbackslash+1);
	    // System.out.println("ticket = "+ticket);
	    Export exp = null;
	    try {
		// System.out.println("looking for "+ticket+" on "+ip);
		exp = (Export) Naming.lookup("//"+ip+"/exporter");
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
