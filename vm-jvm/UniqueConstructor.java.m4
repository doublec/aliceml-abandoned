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

/** The UniqueConstructor class represents constructors that are unique
 *  across machines, e.g., <code>::</code>, <code>ref</code> etc.
 */
public class UniqueConstructor extends Constructor {

    final public java.lang.String name;

    public UniqueConstructor(java.lang.String name) {
	this.name = name;
	GName.gNames.put(name,this);
    }

    /** @see UniqueName */
    final private void writeObject(java.io.ObjectOutputStream out)
	throws java.io.IOException {
	out.defaultWriteObject();
    }

    /** Such an object always exists on the machine.
     *  Still there could be no entry in the hashtable if the
     *  library has not yet been loaded.
     */
    final private Object readResolve()
	throws java.io.ObjectStreamException {
	return GName.gNames.get(name);
    }

    final public java.lang.String toString() {
	return this.name;
    }
}
