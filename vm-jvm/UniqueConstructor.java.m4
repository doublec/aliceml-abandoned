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
    final protected void writeObject(java.io.ObjectOutputStream out)
	throws java.io.IOException {
	out.defaultWriteObject();
    }

    /** Such an object always exists on the machine.
     *  Still there could be no entry in the hashtable if the
     *  library has not yet been loaded.
     */
    final protected Object readResolve()
	throws java.io.ObjectStreamException {
	Object o = GName.gNames.get(name);
	// System.out.println("UC: "+o);
	if (o==null) {
	    GName.gNames.put(name,this);
	    return this;
	} else {
	    return o;
	}
    }

    final public java.lang.String toString() {
	return this.name;
    }
}
