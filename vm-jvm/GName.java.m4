package de.uni_sb.ps.dml.runtime;

final public class GName implements java.io.Serializable {

    public static java.util.Hashtable gNames = new java.util.Hashtable();

    private java.rmi.dgc.VMID id = null;

    public GName() {
	id = new java.rmi.dgc.VMID();
    }

    public int hashCode() {
	return id.hashCode();
    }

    public boolean equals(java.lang.Object o) {
	return (o instanceof GName) &&
	    id.equals(((GName) o).id);
    }
}
