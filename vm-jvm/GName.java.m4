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

import java.io.Serializable;
import java.rmi.dgc.VMID;
import java.util.Hashtable;

final public class GName implements Serializable {

    final public static Hashtable gNames = new Hashtable();

    final public VMID id;

    public GName() {
	id = new VMID();
//      System.out.println("created GName "+id);
    }

    final public int hashCode() {
	return id.hashCode();
    }

    final public boolean equals(Object o) {
	return (o instanceof GName) &&
	    id.equals(((GName) o).id);
    }
}
