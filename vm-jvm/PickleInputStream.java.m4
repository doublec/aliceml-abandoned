package de.uni_sb.ps.dml.runtime;

final public class DMLObjectInputStream extends java.io.ObjectInputStream {

    public DMLObjectInputStream() throws java.io.IOException {
	super();
	enableResolveObject(true);
    }

    public DMLObjectInputStream(java.io.InputStream in)  throws java.io.IOException {
	super(in);
    }

    final protected Class resolveClass(java.io.ObjectStreamClass osc) throws java.io.IOException, ClassNotFoundException {
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

    final protected Object resolveObject(Object obj) {
	if (obj instanceof GName) {
	    Object o = DMLConstructor.gNames.get(obj);
	    if (o==null) {
		DMLValue newc = null;
		if (((GName) obj).isName())
		    newc = new DMLName((GName) obj);
		else
		    newc = new DMLConstructor((GName) obj);
		DMLConstructor.gNames.put(obj,newc);
		return newc;
	    }
	    else
		return o;
	}
	else
	    return obj;
    }
}
