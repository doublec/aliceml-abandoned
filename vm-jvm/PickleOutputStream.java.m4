package de.uni_sb.ps.dml.runtime;

final public class DMLObjectOutputStream extends java.io.ObjectOutputStream {

    static Class fcn = null;
    boolean waitforbind = false;

    public DMLObjectOutputStream() throws java.io.IOException {
	super();
	if (fcn==null)
	    try{
		fcn=Class.forName("de.uni_sb.ps.dml.runtime.DMLFunction");
		System.err.println("Class zum Vergleichen: "+fcn);
	    } catch (ClassNotFoundException e) {
		System.err.println("DMLFcnClosure must be accessable by the same ClassLoader as DMLObjectOutputStream.");
		e.printStackTrace();
	    }
	enableReplaceObject(true);
    }

    public DMLObjectOutputStream(java.io.OutputStream out) throws java.io.IOException {
	super(out);
	if (fcn==null)
	    try{
		fcn=Class.forName("de.uni_sb.ps.dml.runtime.DMLFunction");
		System.err.println("Class zum Vergleichen: "+fcn);
	    } catch (ClassNotFoundException e) {
		System.err.println("DMLFcnClosure must be accessable by the same ClassLoader as DMLObjectOutputStream.");
		e.printStackTrace();
	    }
	enableReplaceObject(true);
    }

    final public void waitForBind(boolean b) {
	waitforbind=b;
    }

    final protected void annotateClass(Class cls) throws java.io.IOException {
	if (fcn.isAssignableFrom(cls.getSuperclass())) {
	    byte[] bytes = null;
	    String name = cls.getName();
	    ClassLoader cl = cls.getClassLoader();
	    if (cl==DMLLoader.loader)
		bytes = ((DMLLoader) cl).getBytes(name);
	    else {
		java.io.InputStream in = null;
		java.io.DataInputStream din = null;
		try {
		    in = cl.getResourceAsStream(name+".class");
		    din = new java.io.DataInputStream(in);
		    bytes = new byte[din.available()];
		    din.readFully(bytes);
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
	    }
	    writeBoolean(true);
	    writeInt(bytes.length);
	    write(bytes,0,bytes.length);
	}
	else
	    writeBoolean(false);
    }

    /** DMLLVar/DMLFuture werden durch ihren Inhalt ersetzt. Falls @field waitforbind true ist,
	wird gewartet, bis die logische Variable gebunden ist. Falls @field waitforbind false ist
	und die logische Variable ungebunden, wirft DMLLVar.writeObject eine Exception. */

    final protected Object replaceObject(Object obj) {
	if (obj instanceof DMLLVar)
	    try {
		if (waitforbind)
		    obj = ((DMLLVar) obj).request();
		else
		    obj = ((DMLLVar) obj).getValue();
	    } catch (java.rmi.RemoteException r) {
		System.err.println(r);
	    }
	if (obj instanceof DMLConstructor)
	    return ((DMLConstructor) obj).globalize();
	else
	    if (obj instanceof DMLName)
		return ((DMLName) obj).globalize();
	    else
		return obj;
    }
}
