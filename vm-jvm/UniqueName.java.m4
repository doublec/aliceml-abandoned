package de.uni_sb.ps.dml.runtime;

public class UniqueName extends Name {

    public UniqueName(java.lang.String name) {
	this.name=name;
	GName.gNames.put(name,this);
    }
    /** Falls der Name noch keinen GName hat, wird jetzt ein
     *  neuer GName erzeugt und der Name wird unter dem GName in
     *  der globalen Hashtabelle eingetragen.
     */
    private void writeObject(java.io.ObjectOutputStream out)
	throws java.io.IOException {
	out.defaultWriteObject();
    }

    /** Beim Einlesen wird der UniqueName durch den der lokalen Maschine ersetzt.
     */
    private java.lang.Object readResolve()
	throws java.io.ObjectStreamException {
	java.lang.Object o = GName.gNames.get(name);
	return o;
    }
}
