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

/** This is the classloader used in conjunction with the pickle in/out streams
 *  to load the classes of pickled objects.
 */
final public class PickleClassLoader extends ClassLoader {

    static final public PickleClassLoader loader = new PickleClassLoader();
    static java.lang.String path = null;
    static boolean write = false;
    java.util.Hashtable hash = new java.util.Hashtable();

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

    public void enter(java.lang.String cl, byte[] b) throws java.rmi.RemoteException {
	hash.put(cl,b);
	if (write) {
	    writeClass(cl,b);
	}
    }

    public byte[] getBytes(java.lang.String cl) {
	return (byte[]) hash.get(cl);
    }

    public static void writeCodebase(java.lang.String p) throws java.rmi.RemoteException {
	path = p;
	write=true;
	Enumeration e = loader.hash.keys();
	while (e.hasMoreElements()) {
	    java.lang.Object name = e.nextElement();
	    byte[] b = (byte[]) loader.hash.get(name);
	    writeClass((java.lang.String) name,b);
	}
    }

    private static void writeClass(java.lang.String name, byte[] b) throws java.rmi.RemoteException {
	try {
	    FileOutputStream fo = new FileOutputStream(path+name);
	    DataOutputStream dout = new DataOutputStream(fo);
	    dout.write(b,0,b.length);
	    dout.flush();
	    fo.close();
	} catch (Exception e) {
	    System.err.println(e);
	    e.printStackTrace();
	    _RAISE(runtimeError,new STRING ("panik"));
	}
    }
}
