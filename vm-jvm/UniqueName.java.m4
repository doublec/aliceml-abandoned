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

/** The UniqueName class represents names that are unique across machines,
 *  e.g., <code>unit</code>, <code>nil</code>, <code>true</code> and
 *  <code>false</code> etc.
 */
public class UniqueName extends Name {

    final protected java.lang.String name;

    public UniqueName(java.lang.String name) {
	this.name = name;
	GName.gNames.put(name,this);
    }

    public java.lang.String toString() {
	return name+" : name";
    }

    /** Here we use the default mechanism to store the name, since the
     *  UniqueNames are put into the GName table by their constructor.
     */
    private void writeObject(java.io.ObjectOutputStream out)
	throws java.io.IOException {
	out.defaultWriteObject();
    }

    /** Beim Einlesen wird der UniqueName durch den der lokalen Maschine
     *  ersetzt.
     */
    final private Object readResolve()
	throws java.io.ObjectStreamException {
	Object o = GName.gNames.get(name);
	if (o==null) {
	    GName.gNames.put(name,this);
	    return this;
	} else {
	    return o;
	}
    }
}
