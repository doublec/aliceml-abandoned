package de.uni_sb.ps.DML.DMLRuntime;

final class DMLObjectOutputStream extends java.io.ObjectOutputStream {

    public DMLObjectOutputStream() throws java.io.IOException {
	super();
	if (fcn==null)
	    try{
		fcn=this.getClass().getClassLoader().loadClass("DMLFcnClosure");
	    } catch (ClassNotFoundException e) {
		System.err.println("DMLFcnClosure must be accessable by the same ClassLoader as DMLObjectOutputStream.");
		e.printStackTrace();
	    }
    }

    public DMLObjectOutputStream(java.io.OutputStream out) throws java.io.IOException {
	super(out);
    }

    static Class fcn = null;

    protected void annotateClass(Class cls) throws java.io.IOException {
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
}
