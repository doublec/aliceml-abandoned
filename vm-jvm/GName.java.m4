package de.uni_sb.ps.DML.DMLRuntime;

final public class GName implements java.io.Serializable {

    int arity = 0;
    String gid = null;

    public GName(int ar) {
	super();
	gid = "wuerfelwild";
	arity=ar;
    }

    public boolean isName() {
	return (arity==0);
    }

    public int hashCode() {
	return gid.hashCode();
    }

    public boolean equals(Object o) {
	return (o instanceof GName) && gid.equals(((GName) o).gid);
    }
}
