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

    final protected java.lang.Object resolveObject(java.lang.Object obj) {
	if (obj instanceof GName) {
	    java.lang.Object o = Constructor.gNames.get(obj);
	    if (o==null) {
		DMLValue newc = null;
		if (((GName) obj).isName())
		    newc = new Name((GName) obj);
		else
		    newc = new Constructor((GName) obj);
		Constructor.gNames.put(obj,newc);
		return newc;
	    }
	    else
		return o;
	}
	else
	    return obj;
    }
}
