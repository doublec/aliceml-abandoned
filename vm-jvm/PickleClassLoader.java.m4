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
    static Hashtable classHash = new Hashtable();

    final public Class findClass(java.lang.String name)
	throws ClassNotFoundException {
	Class cl = null;
	try {
	    cl = super.findClass(name);
	    return cl;
	} catch (ClassNotFoundException c) {
	    cl = (Class) classHash.get(name);
	    if (cl == null) {
		byte[] b = (byte[]) hash.get(name);
		// System.out.println("Trying to define "+name+".");
		if (b != null) {
		    // System.out.println(". Class has "+b.length+" bytes");
		    cl = defineClass(name, b, 0, b.length);
		    classHash.put(name,cl);
		    return cl;
		} else {
		    throw new ClassNotFoundException("PickleClassLoader: class not available.");
		}
	    } else {
		return cl;
	    }
	}
    }

    final public static void enter(java.lang.String cl, byte[] b)
	throws RemoteException {
	hash.put(cl,b);
    }

    final public static byte[] getBytes(java.lang.String cl) {
	return (byte[]) hash.get(cl);
    }

}
