package de.uni_sb.ps.DML.DMLRuntime;

final class DMLObjectOutputStream extends java.io.ObjectOutputStream {

    public DMLObjectOutputStream() throws java.io.IOException {
	super();
    }

    public DMLObjectOutputStream(java.io.OutputStream out) throws java.io.IOException {
	super(out);
    }

    static Class fcn = (new DMLFcnClosure()).getClass();

    protected void annotateClass(Class cls) throws java.io.IOException {
	if (fcn.isAssignableFrom(cls.getSuperclass())) {
	    byte[] bytes = null;
	    String name = cls.getName();
	    ClassLoader cl = cls.getClassLoader();
	    try {
		if (cl==DMLLoader.loader)
		    bytes = ((DMLLoader) cl).getBytes(name);
		else {
		    java.io.InputStream in = cl.getResourceAsStream(name+".class");
		    java.io.DataInputStream din = new java.io.DataInputStream(in);
		    bytes = new byte[din.available()];
		    din.readFully(bytes);
		}
	    } catch (Exception e) {
		System.out.println("This should never happen.");
		e.printStackTrace();
	    }
	    writeBoolean(true);
	    writeInt(bytes.length);
	    write(bytes,0,bytes.length);
	}
	else
	    writeBoolean(false);
    }
}
