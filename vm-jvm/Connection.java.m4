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

import java.net.*;
import java.rmi.*;
import java.rmi.server.*;
import java.util.*;

final public class Connection {
    static Hashtable export = null;
    static Exporter exp = null;
    static Random rand = null;
    static int exporterNumber = 0;
    static InetAddress thisHost;
    static {
	InetAddress i=null;
	try {
	    i = InetAddress.getLocalHost();
	} catch (java.net.UnknownHostException u) {
	    System.err.println(u);
	    u.printStackTrace();
	}
	if (i == null) {
	    thisHost = null;
	} else {
	    thisHost = i;
	}
    }

    final protected static void startServer() throws RemoteException {
	Properties prop = System.getProperties();
	Object o = prop.get("java.security.policy");
	if (o == null) {
	    java.lang.String name = (java.lang.String) prop.get("user.name");
	    prop.put("java.security.policy",
		     "http://"+thisHost.getHostName()+"/~"+name+"/codebase/policy");
	}
	if (System.getSecurityManager() == null) {
	    // System.out.println("starte security manager");
	    try {
		System.setSecurityManager(new RMISecurityManager());
	    } catch (Exception e) {
		System.err.println("could not install security manager");
		System.err.println("Policy-file used: "+prop.get("java.security.policy"));
	    }
	}
	export = new Hashtable();
	exp = new Exporter(export);
	//	rand = new Random(42);
	rand = new Random((new Date()).getTime());

	// System.out.println("starte registry");
	try {
	    java.rmi.registry.Registry r = java.rmi.registry.LocateRegistry.getRegistry(1099);
	    exporterNumber = r.list().length;
	} catch (RemoteException _) {
	    java.rmi.registry.LocateRegistry.createRegistry(1099); // am Standardport
	    // exporterNumber = 0;
	}
	try {
	    // System.out.println("binde exporter in registry");
	    Naming.rebind("//localhost/exporter"+exporterNumber,exp);
	} catch (MalformedURLException m) {
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
		} catch (RemoteException n) {
		    System.err.println("Could not start server!");
		    return new STRING ("invalid ticket");
		}
	    }
	    java.lang.String ticket = Long.toString(rand.nextLong());
	    export.put(ticket,val);
	    return new STRING (exporterNumber+"#"+
			       thisHost.getHostAddress()+":"+ticket);
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
		} catch (RemoteException n) {
		    System.err.println("Could not start server!");
		    return new STRING ("invalid ticket");
		}
	    }
	    try {
	    java.lang.String ti = ((STRING) val).value;
	    // System.out.println("ti = "+ti);
	    int indexofnumber = ti.indexOf('#');
	    if (indexofnumber < 0) {
		_error("ticket invalid",val);
	    }
	    java.lang.String number = ti.substring(0,indexofnumber);
	    ti = ti.substring(indexofnumber+1);
	    int indexofcolon = ti.indexOf(':');
	    if (indexofcolon < 0) {
		_error("ticket invalid",val);
	    }
	    java.lang.String ip = ti.substring(0,indexofcolon);
	    // System.out.println("ip = "+ip);
	    java.lang.String ticket = ti.substring(indexofcolon+1);
	    // System.out.println("ticket = "+ticket);
	    Export exp = null;
	    try {
		// System.out.println("looking for "+ticket+" on "+ip);
		exp = (Export) Naming.lookup("//"+ip+"/exporter"+number);
	    } catch (NotBoundException n) {
		_error("ticket not bound",val);
	    } catch (MalformedURLException m) {
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
