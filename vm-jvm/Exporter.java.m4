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

final public class Exporter extends java.rmi.server.UnicastRemoteObject implements Export {

    public static final java.util.Hashtable classfields = new java.util.Hashtable();
    
    private java.util.Hashtable hash;

    public Exporter() throws java.rmi.RemoteException {}

    public Exporter(java.util.Hashtable h) throws java.rmi.RemoteException {
	hash=h;
    }

    final public DMLValue get(java.lang.String what) throws java.rmi.RemoteException {
	return (DMLValue) hash.get(what);
    }

    final public byte[] getClass(java.lang.String className) throws java.rmi.RemoteException {
	return PickleClassLoader.loader.getBytes(className);
    }

    // This method returns the static fields of a function class
    // corresponds to the pickling process of storing static fields
    final public Object getField(java.lang.String fieldName) throws java.rmi.RemoteException {
	return classfields.get(fieldName);
    }

    // This method can only be invoked on the server site; it is not visible
    // on client sites
    // Use this method to enter the static fields of a function class
    final static public Object putField(java.lang.String fieldName,
					Object content) {
	return classfields.put(fieldName,content);
    }
}
