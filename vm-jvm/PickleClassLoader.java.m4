package de.uni_sb.ps.dml.runtime;

import java.io.*;
import java.util.*;

public class PickleClassLoader extends ClassLoader {

    static public PickleClassLoader loader = new PickleClassLoader();
    static java.lang.String path = null;
    static boolean write = false;
    java.util.Hashtable hash = new java.util.Hashtable();

    public Class findClass(java.lang.String name) {
	byte[] b = (byte[]) hash.get(name);
	return defineClass(name, b, 0, b.length);
    }

    public void enter(java.lang.String cl, byte[] b) throws java.rmi.RemoteException {
	hash.put(cl,b);
	if (write)
	    writeClass(cl,b);
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
	    Constants.runtimeError.apply(new de.uni_sb.ps.dml.runtime.String("panik")).raise();
	}
    }
}
