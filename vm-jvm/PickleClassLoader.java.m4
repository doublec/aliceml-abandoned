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

import java.io.*;
import java.util.*;
import java.rmi.RemoteException;

/** This is the classloader used in conjunction with the pickle in/out streams
 *  to load the classes of pickled objects.
 */
final public class PickleClassLoader extends ClassLoader {

    static final public PickleClassLoader loader = new PickleClassLoader();
    static java.lang.String path = null;
    static Hashtable hash = new Hashtable();
    public Class findClass(java.lang.String name) throws ClassNotFoundException {
	byte[] b = (byte[]) hash.get(name);
	// System.out.print("Trying to define "+name);
	if (b != null) {
	    // System.out.println(". Class has "+b.length+" bytes");
	    return defineClass(name, b, 0, b.length);
	} else {
	    return super.findClass(name);
	}
    }

    public static void enter(java.lang.String cl, byte[] b) throws RemoteException {
	hash.put(cl,b);
    }

    public static byte[] getBytes(java.lang.String cl) {
	return (byte[]) hash.get(cl);
    }

}
