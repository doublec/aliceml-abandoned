/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

/** The UniqueConstructor class represents constructors that are unique
 *  across machines, e.g., <code>::</code>, <code>ref</code> etc.
 */
final public class UniqueConstructor extends Constructor {

    public UniqueConstructor(java.lang.String name) {
	this.name=name;
	GName.gNames.put(name,this);
    }

    /** @see UniqueName */
    private void writeObject(java.io.ObjectOutputStream out)
	throws java.io.IOException {
	out.defaultWriteObject();
    }

    /** Such an object always exists on the machine.
     */
    private java.lang.Object readResolve()
	throws java.io.ObjectStreamException {
	return GName.gNames.get(name);
    }
}
