package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLObjectInputStream extends java.io.ObjectInputStream {

    public DMLObjectInputStream() throws java.io.IOException {
	super();
    }

    public DMLObjectInputStream(java.io.InputStream in)  throws java.io.IOException {
	super(in);
    }

    protected Class resolveClass(java.io.ObjectStreamClass osc) throws java.io.IOException, ClassNotFoundException {
	if (readBoolean()) {
	    int length = readInt();
	    byte[] bytes = new byte[length];
	    read(bytes,0,length);
	    DMLLoader.loader.enter(osc.getName(),bytes);
	    return DMLLoader.loader.loadClass(osc.getName());
	}
	else
	    return super.resolveClass(osc);
    }
}
