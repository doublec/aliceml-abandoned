/*
 * Author: 
 *      Daniel Simon, <dansim@ps.uni-sb.de>
 * 
 * Copyright:
 *      Daniel Simon, 1999
 *
 * Last change:
 *    $Date$ by $Author$
 * $Revision$
 * 
 */
package de.uni_sb.ps.dml.runtime;

final public class GName implements java.io.Serializable {

    final public static java.util.Hashtable gNames = new java.util.Hashtable();

    final private java.rmi.dgc.VMID id;

    public GName() {
	id = new java.rmi.dgc.VMID();
    }

    final public int hashCode() {
	return id.hashCode();
    }

    final public boolean equals(java.lang.Object o) {
	return (o instanceof GName) &&
	    id.equals(((GName) o).id);
    }
}
