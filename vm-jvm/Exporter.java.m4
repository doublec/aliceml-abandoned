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
import java.util.Hashtable;

final public class Exporter extends UnicastRemoteObject implements Export {

    public static final Hashtable classfields = new Hashtable();

    private Hashtable hash;

    public Exporter() throws RemoteException {}

    public Exporter(Hashtable h) throws RemoteException {
	hash=h;
    }

    final public DMLValue get(java.lang.String what) throws RemoteException {
	return (DMLValue) hash.get(what);
    }

    final public byte[] getClass(java.lang.String className) throws RemoteException {
	return PickleClassLoader.loader.getBytes(className);
    }

    // This method returns the static fields of a function class
    // corresponds to the pickling process of storing static fields
    final public Object getField(java.lang.String fieldName) throws RemoteException {
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
