package de.uni_sb.ps.dml.runtime;

public class DMLLoader extends ClassLoader {

    static public DMLLoader loader = new DMLLoader();

    java.util.Hashtable hash = new java.util.Hashtable();

    public Class findClass(String name) {
	byte[] b = (byte[]) hash.get(name);
	return defineClass(name, b, 0, b.length);
    }

    public void enter(String cl, byte[] b) {
	hash.put(cl,b);
    }

    public byte[] getBytes(String cl) {
	return (byte[]) hash.get(cl);
    }
}
