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

import java.rmi.server.*;
import java.lang.reflect.*;

/** This is the OutputStream used to create pickles.
 */
final public class PickleOutputStream extends java.io.ObjectOutputStream {

    final static Class fcn; // class of Function
    final static Class ccn; // class of Constructor
    final static Class ucn; // class of UniqueConstructor
    final static Class ncn; // class of Name
    final static Class uncn; // class of UniqueName
    static {
	Class cl = null;
	try{
	    cl = Class.forName("de.uni_sb.ps.dml.runtime.Function");
	    // System.err.println("Class zum Vergleichen: "+fcn);
	} catch (ClassNotFoundException e) {
	    System.err.println("Function must be accessable by the same ClassLoader as java.lang.ObjectOutputStream.");
	    e.printStackTrace();
	}
	fcn = cl;

	cl = null;
	try{
	    cl = Class.forName("de.uni_sb.ps.dml.runtime.Constructor");
	    // System.err.println("Class zum Vergleichen: "+fcn);
	} catch (ClassNotFoundException e) {
	    System.err.println("Constructor must be accessable by the same ClassLoader as java.lang.ObjectOutputStream.");
	    e.printStackTrace();
	}
	ccn = cl;

	cl = null;
	try{
	    cl = Class.forName("de.uni_sb.ps.dml.runtime.UniqueConstructor");
	    // System.err.println("Class zum Vergleichen: "+fcn);
	} catch (ClassNotFoundException e) {
	    System.err.println("Constructor must be accessable by the same ClassLoader as java.lang.ObjectOutputStream.");
	    e.printStackTrace();
	}
	ucn = cl;

	cl = null;
	try{
	    cl = Class.forName("de.uni_sb.ps.dml.runtime.Name");
	    // System.err.println("Class zum Vergleichen: "+fcn);
	} catch (ClassNotFoundException e) {
	    System.err.println("Name must be accessable by the same ClassLoader as java.lang.ObjectOutputStream.");
	    e.printStackTrace();
	}
	ncn = cl;

	cl = null;
	try{
	    cl = Class.forName("de.uni_sb.ps.dml.runtime.UniqueName");
	    // System.err.println("Class zum Vergleichen: "+fcn);
	} catch (ClassNotFoundException e) {
	    System.err.println("UniqueName must be accessable by the same ClassLoader as java.lang.ObjectOutputStream.");
	    e.printStackTrace();
	}
	uncn = cl;
    }

    boolean waitforbind = false;
    int objectcounter = 0;

    public PickleOutputStream() throws java.io.IOException {
	super();
	enableReplaceObject(true);
    }

    public PickleOutputStream(java.io.OutputStream out) throws java.io.IOException {
	super(out);
	enableReplaceObject(true);
    }

    final public void waitForBind(boolean b) {
	waitforbind=b;
    }

    final protected void annotateClass(Class cls) throws java.io.IOException {
	objectcounter++;
	// System.out.println("annotateClass "+cls);
	Class superClass = cls.getSuperclass();
	if (fcn.isAssignableFrom(superClass) // cls instanceof fUNcTIOn
	    || (ccn.isAssignableFrom(superClass) &&
		!ucn.isAssignableFrom(cls) &&
		!ucn.equals(cls)) // cls is a constructor group
	    || (ncn.isAssignableFrom(superClass) &&
		!uncn.isAssignableFrom(cls)) // cls is a name group
	    ) {
	    // System.out.println("POS: annotateClass "+cls+" must be annotated");
	    byte[] bytes = null;
	    java.lang.String name = cls.getName();
	    ClassLoader cl = cls.getClassLoader();
	    if (cl == PickleClassLoader.loader ||
		PickleClassLoader.loader.getBytes(name) != null) {
		// System.out.println("POS: annotateClass "+cls+" came from a pickle");
		bytes = PickleClassLoader.loader.getBytes(name);
	    } else if (cl == null || cl==ClassLoader.getSystemClassLoader()) {
		//System.out.println("POS: annotateClass "+cls+" came from somewhere else");
		java.io.InputStream in = null;
		java.io.DataInputStream din = null;
		if (cl == null) {
		    cl = ClassLoader.getSystemClassLoader();
		}
//              java.io.OutputStream out = null;
//              java.io.DataOutputStream refout = null;
		try {
		    in = cl.getResourceAsStream(name+".class");
		    din = new java.io.DataInputStream(in);
		    bytes = new byte[din.available()];
		    din.readFully(bytes); // NICHT: read
//                  out = new java.io.FileOutputStream("ref"+name);
//                  refout = new java.io.DataOutputStream(out);
//                  refout.write(bytes,0,bytes.length);
//                  refout.close();
		}
		catch (Exception e) {
		    System.err.println("This should never happen.");
		    e.printStackTrace();
		}
		finally {
		    try {
			if (in!=null)
			    in.close();
			if (din!=null)
			    din.close();
		    } catch (java.io.IOException e) {
			e.printStackTrace();
		    }
		}
	    } else if (cl instanceof java.net.URLClassLoader) { // Klasse wurde über Netz geladen
		// System.out.println("POS: annotateClass "+cls+" has been loaded via Network");
		java.net.URL[] urls = ((java.net.URLClassLoader) cl).getURLs();
		for(int i=0; i<urls.length; i++) {
		    try {
			// System.out.println("Trying: "+urls[i]);
			java.io.DataInputStream in =new java.io.DataInputStream(urls[i].openStream());
			bytes=new byte[in.available()];
			in.readFully(bytes);
			break;  // bei Erfolg for verlassen
		    } catch (java.io.IOException io) {
			System.err.println(urls[i]+" IOException");
			io.printStackTrace();
		    }
		}
	    }
	    writeBoolean(true);
	    if (bytes == null) {
		System.out.println("CRASH: class annotated was "+cls);
		System.out.println("Class loaded by "+cl);
	    }
	    writeInt(bytes.length);
	    write(bytes,0,bytes.length);
	    // now class has been written. output the static fields
	    Field[] fields = cls.getDeclaredFields();
	    int fc = fields.length;
	    for (int i=0; i<fc; i++) {
		int modifier = fields[i].getModifiers();
		if (Modifier.isStatic(modifier)) {
		    Object content = null;
		    try {
			content = fields[i].get(null);
		    } catch (IllegalArgumentException I) {
			System.err.println(I);
			I.printStackTrace();
		    } catch (IllegalAccessException I) {
			System.err.println(I);
			I.printStackTrace();
		    }
		    writeObject(content);
		}
	    }
	} else {
	    writeBoolean(false);
	}
    }

    /** LVar/Future werden durch ihren Inhalt ersetzt. Falls @field waitforbind true ist,
     *  wird gewartet, bis die logische Variable gebunden ist. Falls @field waitforbind false ist
     *  und die logische Variable ungebunden, wirft LVar.writeObject eine Exception.
     *  Array und Thread sollen immer Exception werfen, Reference ebenfalls.
    */

    final protected java.lang.Object replaceObject(java.lang.Object obj) {
	// System.out.println("POS: replaceObject "+obj);
	try {
	    if (obj instanceof DMLLVar) {
		if (waitforbind)
		    obj = ((DMLLVar) obj).request();
		else
		    obj = ((DMLLVar) obj).getValue();
		return obj;
	    } else if (obj instanceof Array) {
		_RAISE(runtimeError,new STRING ("cannot pickle DMLArray"));
	    } else if (obj instanceof Thread) {
		_RAISE(runtimeError,new STRING ("cannot pickle Thread"));
	    } else if (obj instanceof Reference) {
		_RAISE(runtimeError,new STRING ("cannot pickle Reference"));
	    } else
		return obj;
	} catch (java.rmi.RemoteException r) {
	    System.err.println(r);
	    r.printStackTrace();
	    return null;
	}
    }
}
