package de.uni_sb.ps.DML.DMLRuntime;

final class DMLObjectOutputStream extends java.io.ObjectOutputStream {

    public DMLObjectOutputStream() throws java.io.IOException {
	super();
	if (fcn==null)
	    fcn=this.getClass().getClassLoader().loadClass("DMLFcnClosure");
    }

    public DMLObjectOutputStream(java.io.OutputStream out) throws java.io.IOException {
	super(out);
    }

    static Class fcn = null;

    protected void annotateClass(Class cls) throws java.io.IOException {
	java.io.InputStream in=null;
	if (fcn.isAssignableFrom(cls.getSuperclass())) {
	    byte[] bytes = null;
	    String name = cls.getName();
	    ClassLoader cl = cls.getClassLoader();
	    if (cl==DMLLoader.loader)
		bytes = ((DMLLoader) cl).getBytes(name);
	    else {
		try {
		    java.io.InputStream in = cl.getResourceAsStream(name+".class");
		    java.io.DataInputStream din = new java.io.DataInputStream(in);
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
			e.printStacktrace();
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
