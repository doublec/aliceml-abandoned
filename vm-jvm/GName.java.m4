package de.uni_sb.ps.DML.DMLRuntime;

final public class GName implements java.io.Serializable {

    static long gidcount = 0l;
    
    int arity = 0;
    long gid = 0l;
    String osname = null;
    long time = 0l;

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

    public boolean equals(Object o) {
	return (o instanceof GName) && osname.equals(((GName) o).osname);
    }
}
