/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

public class Name implements DMLValue {

    final protected java.lang.String name;
    GName gName = null;

    public Name(java.lang.String name) {
	super();
	this.name=name;
	this.gName=null;
    }

    public Name() {
	super();
	this.name = "unnamed";
	this.gName=null;
    }

    final public java.lang.String toString() {
	return name+" : name";
    }

    /** Falls der Name noch keinen GName hat, wird jetzt ein
     *  neuer GName erzeugt und der Name wird unter dem GName in
     *  der globalen Hashtabelle eingetragen.
     */
    private void writeObject(java.io.ObjectOutputStream out)
	throws java.io.IOException {
	if (gName==null) {
	    gName=new GName();
	    GName.gNames.put(gName,this);
	}
	out.defaultWriteObject();
    }

    /** Beim Einlesen wird nachgeschaut, ob bereits ein Objekt mit
     *  diesem GName existiert. Falls nicht, wird das aktuelle
     *  Objekt mit einem neuen GName in die Hashtabelle
     *  eingetragen. Sonst wird das Objekt aus der Hashtabelle
     *  zurückgeliefert.
     */
    private java.lang.Object readResolve()
	throws java.io.ObjectStreamException {
	java.lang.Object o = GName.gNames.get(gName);
	if (o==null) {
	    gName=new GName();
	    GName.gNames.put(gName,this);
	    return this;
	} else {
	    return o;
	}
    }

    _apply_fails ;
    _request_id ;
    _getValue_id ;
    _raise ;
}
