package sun.rmi.server;

import java.io.*;
import java.rmi.Remote;
import sun.rmi.transport.Utils;

import de.uni_sb.ps.dml.runtime.PickleClassLoader;
import de.uni_sb.ps.dml.runtime.Exporter;
import java.lang.reflect.*;
/**
 * A MarshalOutputStream extends ObjectOutputStream to add functions
 * specific to marshaling of remote object references. If it is
 * necessary to serialize remote objects or objects that contain
 * references to remote objects a MarshalOutputStream must be used
 * instead of ObjectOutputStream. <p>
 *
 * A new MarshalOutputStream is constructed to serialize remote
 * objects or graphs containing remote objects. Objects are written to
 * the stream using the ObjectOutputStream.writeObject method. <p>
 *
 * MarshalOutputStream maps remote objects to the corresponding remote
 * stub and embeds the location from which to load the stub
 * classes. The location may be ignored by the client but is supplied.
 */
public class MarshalOutputStream extends ObjectOutputStream
{
    final static String host;
    final static Class fcn;
    final static Class ccn; // class of Constructor
    final static Class ucn; // class of UniqueConstructor
    final static Class ncn; // class of Name
    final static Class uncn; // class of UniqueName
    static {
	Class cl = null;
	try{
	    cl=Class.forName("de.uni_sb.ps.dml.runtime.Function");
	    // System.err.println("Class zum Vergleichen: "+fcn);
	} catch (ClassNotFoundException e) {
	    System.err.println("Function must be accessable by the same ClassLoader as ObjectOutputStream.");
	    e.printStackTrace();
	}
	fcn = cl;
	java.net.InetAddress i = null;
	try {
	    i=java.net.InetAddress.getLocalHost();
	} catch (java.net.UnknownHostException u) {
	    System.err.println(u);
	    u.printStackTrace();
	}
	host = "!"+i.getHostAddress();

	cl = null;
	try{
	    cl = Class.forName("de.uni_sb.ps.dml.runtime.Constructor");
	    // System.err.println("Class zum Vergleichen: "+fcn);
	} catch (ClassNotFoundException e) {
	    System.err.println("Constructor must be accessable by the same ClassLoader as ObjectOutputStream.");
	    e.printStackTrace();
	}
	ccn = cl;

	cl = null;
	try{
	    cl = Class.forName("de.uni_sb.ps.dml.runtime.UniqueConstructor");
	    // System.err.println("Class zum Vergleichen: "+fcn);
	} catch (ClassNotFoundException e) {
	    System.err.println("UniqueConstructor must be accessable by the same ClassLoader as ObjectOutputStream.");
	    e.printStackTrace();
	}
	ucn = cl;

	cl = null;
	try{
	    cl = Class.forName("de.uni_sb.ps.dml.runtime.Name");
	    // System.err.println("Class zum Vergleichen: "+fcn);
	} catch (ClassNotFoundException e) {
	    System.err.println("Name must be accessable by the same ClassLoader as ObjectOutputStream.");
	    e.printStackTrace();
	}
	ncn = cl;

	cl = null;
	try{
	    cl = Class.forName("de.uni_sb.ps.dml.runtime.UniqueName");
	    // System.err.println("Class zum Vergleichen: "+fcn);
	} catch (ClassNotFoundException e) {
	    System.err.println("UniqueName must be accessable by the same ClassLoader as ObjectOutputStream.");
	    e.printStackTrace();
	}
	uncn = cl;
    }

    /** Create a marshaling stream to handle RMI marshaling
     */
    public MarshalOutputStream(OutputStream out) throws IOException {
	super(out);
	this.useProtocolVersion(ObjectStreamConstants.PROTOCOL_VERSION_1);
	java.security.AccessController.doPrivileged(
				    new java.security.PrivilegedAction() {
	    public Object run() {
		privilegedEnableReplaceObject();
		return null;
	    }
	});
    }

    private void privilegedEnableReplaceObject()
    {
	enableReplaceObject(true);
    }


    /** replaceObject is extended to check for instances of Remote
     * that need to be serialized as proxy objects.  RemoteProxy.getProxy
     * is called to check for and find the stub.
     */
    protected Object replaceObject(Object obj) throws IOException {
	if (obj instanceof Remote) {
	    obj = RemoteProxy.getProxy((Remote)obj);
	}
	return obj;
    }

    /**
     * annotateClass is extended to serialize a location from which
     * to load the the specified class.
     */
    protected void annotateClass(Class cl) throws IOException {
	Class superClass = cl.getSuperclass();
	//      System.out.println("Annotate: "+cl+" extends "+superClass);
	if (fcn.isAssignableFrom(superClass) // cl instanceof Function
	    || (ccn.isAssignableFrom(superClass) &&
		!ucn.isAssignableFrom(superClass) &&
		!ucn.equals(cl)) // cl is constructor group
	    || (ncn.isAssignableFrom(superClass) &&
		!uncn.isAssignableFrom(cl)) // cl is a name group
	    ) {      // we transfer class code by need, i.e. we annotate the class with the ip of
	    // the server that knows the byte code. the byte code is stored in the PickleClassLoader
	    //System.out.println("I RMI serialize: "+cl);
	    String className = cl.getName();
	    if (PickleClassLoader.loader.getBytes(className) == null) { // code not yet in loader
		// enter code into loader
		byte[] bytes = null;
		ClassLoader loader = cl.getClassLoader();
		if (loader == null
		    || loader==ClassLoader.getSystemClassLoader()) {
		    if (loader == null) {
			loader=ClassLoader.getSystemClassLoader();
		    }
		    java.io.InputStream in = null;
		    java.io.DataInputStream din = null;
		    try {
			in = loader.getResourceAsStream(className+".class");
			din = new java.io.DataInputStream(in);
			bytes = new byte[din.available()];
			din.readFully(bytes); // NICHT: read
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
		} else if (loader instanceof java.net.URLClassLoader) { // Klasse wurde über Netz geladen
		    java.net.URL[] urls = ((java.net.URLClassLoader) loader).getURLs();
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
		// what should I do about the static fields?
		// another hashtable with key = className+"field"+i?
		PickleClassLoader.loader.enter(className,bytes);

		// now make static fields accessable
		Field[] fields = cl.getDeclaredFields();
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
			Exporter.putField(className+"field"+i,content);
		    }
		}
	    }
	    // write the host; format: !134.96.186.121
	    writeLocation(host);
	} else {
	    // write the specified location (may be null).
	    writeLocation(java.rmi.server.RMIClassLoader.getClassAnnotation(cl));
	}
    }

    /**
     * Write the location for the class into the stream.  This method can
     * be overridden by subclasses that store this annotation somewhere
     * else than as the next object in the stream, as is done by this class.
     */
    protected void writeLocation(String location) throws IOException {
	writeObject(location);
    }
}
