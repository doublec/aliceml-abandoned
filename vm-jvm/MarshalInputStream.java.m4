/*
 * @(#)MarshalInputStream.java	1.19 98/07/15
 *
 * Copyright 1996-1998 by Sun Microsystems, Inc.,
 * 901 San Antonio Road, Palo Alto, California, 94303, U.S.A.
 * All rights reserved.
 *
 * This software is the confidential and proprietary information
 * of Sun Microsystems, Inc. ("Confidential Information").  You
 * shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement
 * you entered into with Sun.
 */

package sun.rmi.server;

import java.io.*;
import java.net.*;
import java.util.*;
import java.rmi.server.RMIClassLoader;

import sun.security.action.GetBooleanAction;

/**
 * MarshalInputStream is an extension of ObjectInputStream.  When resolving
 * a class, it reads an object from the stream written by a corresponding
 * MarshalOutputStream.  If the class to be resolved is not available
 * locally, from the first class loader on the execution stack, or from the
 * context class loader of the current thread, it will attempt to load the
 * class from the location annotated by the sending MarshalOutputStream.
 * This location object must be a string representing a path of URLs.
 * <p>
 * A new MarshalInputStream should be created to deserialize remote objects or 
 * graphs containing remote objects.  Objects are created from the stream
 * using the ObjectInputStream.readObject method.
 */
public class MarshalInputStream extends ObjectInputStream {

    /** if true, don't try superclass first in resolveClass() */
    private boolean skipDefaultResolveClass = false;

    /**
     * value of "java.rmi.server.useCodebaseOnly" property,
     * as cached at class initialization time.
     */
    private final static boolean useCodebaseOnly =
	((Boolean) java.security.AccessController.doPrivileged(
            new GetBooleanAction("java.rmi.server.codebase"))).booleanValue();

    /**
     * Create a new MarshalInputStream object.
     */
    public MarshalInputStream(InputStream in)
	throws IOException, StreamCorruptedException
    {	      
	super(in);
    }

    /**
     * resolveClass is extended to acquire (if present) the location
     * from which to load the specified class.
     * It will find, load, and return the class.
     */
    protected Class resolveClass(ObjectStreamClass classDesc)
	throws IOException, ClassNotFoundException
    {
	/*
	 * Always read annotation written by MarshalOutputStream
	 * describing where to load class from.
	 */
	Object annotation = readLocation();

	/*
	 * Unless we were told to skip this step, first try resolving the
	 * class using default ObjectInputStream mechanism (using first
	 * non-null class loader on the execution stack) to maximize
	 * likelihood of type compatibility with calling code.  (This step
	 * is skipped during server parameter unmarshalling using the 1.2
	 * stub protocol, because there would never be a non-null class
	 * loader on the stack in that situation anyway.)
	 */
	if (!skipDefaultResolveClass) {
	    try {
		return super.resolveClass(classDesc);
	    } catch (ClassNotFoundException e) {
	    }
	}

	String className = classDesc.getName();

	/*
	 * Finally, try loading class from an RMIClassLoader instance.
	 * If either the "java.rmi.server.useCodebaseOnly" property was
	 * set or the annotation is not a String, load from the local
	 * loader using the "java.rmi.server.codebase" URL.  Otherwise,
	 * load from a loader using the codebase URL in the annotation.
	 */
	try {
	    if (!useCodebaseOnly &&
		annotation != null && (annotation instanceof String))
	    {
		String location = (String) annotation;
			System.out.println("Location: "+location);
			// hier machen wir eine tolle Analyse des annotation Strings
			// und laden gegebenenfalls die Klasse selbst via unseren PickleClassLoader
			// case location of !IP!className =>
			//   schnapp den byte code von IP mit className;
			//   lade Klasse mit PickleClassLoader.enter und gib sie zurück
			if (location.startsWith("!")) {
			    String ip = location.substring(1);
			    Export exp = null;
			    try {
				exp = (Export) Naming.lookup("//"+ip+"/exporter");
			    } catch (Exception e) {
				System.err.println(e);
				e.printStackTrace();
				return null;
			    }
			    PickleClassLoader.loader.enter(className, exp.getClass(className));
			    return PickleClassLoader.loader.findClass(className);
			} else {
			    return LoaderHandler.loadClass(location, className);
			}
	    } else {
		return LoaderHandler.loadClass(className);
	    }
	} catch (MalformedURLException ex) {
	    /*
	     * REMIND: Do we really want to propagate MalformURLException
	     * like this?  (And it doesn't need to be wrapped; it is a
	     * subclass of IOException anyway.)
	     */
	    throw new IOException("Malformed URL: " + annotation);
	}
    }

    /**
     * Return the location for the class in the stream.  This method can
     * be overridden by subclasses that store this annotation somewhere
     * else than as the next object in the stream, as is done by this class.
     */
    protected Object readLocation()
	throws IOException, ClassNotFoundException
    {
	return readObject();
    }

    /**
     * Set a flag to indicate that the superclass's default resolveClass()
     * implementation should not be invoked by our resolveClass().
     */
    void skipDefaultResolveClass() {
	skipDefaultResolveClass = true;
    }
}
