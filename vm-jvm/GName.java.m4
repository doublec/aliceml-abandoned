package de.uni_sb.ps.dml.runtime;

final public class GName implements java.io.Serializable {

    private static long gidcount = 0l;
    private int arity = 0;
    private long gid = 0l;
    private java.lang.String osname = null;
    private long time = 0l;

    public GName(int ar) {
	super();
	gid=gidcount++;
	osname=System.getProperty("os.name");
	time=System.currentTimeMillis();
	arity=ar;
    }

    public boolean isName() {
	return (arity==0);
    }

    public int hashCode() {
	return osname.hashCode();
    }

    public boolean equals(java.lang.Object o) {
	return (o instanceof GName) && osname.equals(((GName) o).osname);
    }
}
