package de.uni_sb.ps.dml.runtime;

final public class PickleInputStream extends java.io.ObjectInputStream {

    public PickleInputStream() throws java.io.IOException {
	super();
	enableResolveObject(true);
    }

    public PickleInputStream(java.io.InputStream in)  throws java.io.IOException {
	super(in);
    }

    final protected Class resolveClass(java.io.ObjectStreamClass osc) throws java.io.IOException, ClassNotFoundException {
	if (readBoolean()) {
	    int length = readInt();
	    byte[] bytes = new byte[length];
	    read(bytes,0,length);
	    PickleClassLoader.loader.enter(osc.getName(),bytes);
	    return PickleClassLoader.loader.loadClass(osc.getName());
	}
	else
	    return super.resolveClass(osc);
    }
}
