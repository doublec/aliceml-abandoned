package de.uni_sb.ps.dml.runtime;

public class PickleClassLoader extends ClassLoader {

    static public PickleClassLoader loader = new PickleClassLoader();

    java.util.Hashtable hash = new java.util.Hashtable();

    public Class findClass(java.lang.String name) {
	byte[] b = (byte[]) hash.get(name);
	return defineClass(name, b, 0, b.length);
    }

    public void enter(java.lang.String cl, byte[] b) {
	hash.put(cl,b);
    }

    public byte[] getBytes(java.lang.String cl) {
	return (byte[]) hash.get(cl);
    }
}
