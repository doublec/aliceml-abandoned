/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

/** The UniqueName class represents names that are unique across machines,
 *  e.g., <code>unit</code>, <code>nil</code>, <code>true</code> and
 *  <code>false</code> etc.
 */
public class UniqueName extends Name {

    public UniqueName(java.lang.String name) {
	this.name=name;
	GName.gNames.put(name,this);
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
    private java.lang.Object readResolve()
	throws java.io.ObjectStreamException {
	    return GName.gNames.get(name);
    }
}
