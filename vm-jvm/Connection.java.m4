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

    final static java.net.InetAddress thisHost;
    static {
	java.net.InetAddress i=null;
	try {
	    i = java.net.InetAddress.getLocalHost();
	} catch (java.net.UnknownHostException u) {
	    System.err.println(u);
	    u.printStackTrace();
	}
	thisHost = i;
    }

    final private static void startServer() throws java.rmi.RemoteException {
	java.util.Properties prop = System.getProperties();
	Object o = prop.get("java.security.policy");
	if (o == null) {
	    java.lang.String name = (java.lang.String) prop.get("user.name");
	    prop.put("java.security.policy",
		     "http://"+thisHost.getHostName()+"/~"+name+"/codebase/policy");
	}
	if (System.getSecurityManager() == null) {
	    System.out.println("starte security manager");
	    try {
		System.setSecurityManager(new RMISecurityManager());
	    } catch (Exception e) {
		System.err.println("could not install security manager");
		System.err.println("Policy-file used: "+prop.get("java.security.policy"));
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

    _BUILTIN(Offer) {
	_BUILTTUP;
	_APPLY(val) {
	    // FROMSINGLE(args,val,1,"Connection.offer");
	    if (export == null) {
		try {
		    startServer();
		} catch (java.rmi.RemoteException n) {
		    System.err.println("Could not start server!");
		    return new STRING ("invalid ticket");
		}
	    }
	    java.lang.String ticket = Long.toString(rand.nextLong());
	    export.put(ticket,val);
	    return new STRING (thisHost.getHostAddress()+"\\"+ticket);
	}
    }
    /** val offer : value -> ticket */
    _FIELD(Connection,offer);

    _BUILTIN(Take) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    if (export == null) {
		try {
		    startServer();
		} catch (java.rmi.RemoteException n) {
		    System.err.println("Could not start server!");
		    return new STRING ("invalid ticket");
		}
	    }
	    try {
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
	    } catch (ClassCastException e) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** val take : ticket -> value */
    _FIELD(Connection,take);
}
