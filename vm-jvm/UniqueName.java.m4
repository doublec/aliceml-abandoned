package de.uni_sb.ps.dml.runtime;

public class UniqueName extends Name {
    // ererbt:
    //      java.lang.String name = null;
    //      GName gName = null;

    public UniqueName(java.lang.String name) {
	this.name=name;
	GName.gNames.put(name,this);
    }

    // im folgenden alles von Name geerbt:
    //      final public java.lang.String toString() {
    //  	return name+" : name";
    //      }

    //      final public boolean equals(java.lang.Object o) {
    //  	return (this == o);
    //      }

    //      final public DMLValue getValue() {
    //  	return this;
    //      }

    //      final public DMLValue request() {
    //  	return this;
    //      }

    //      final public DMLValue apply(DMLValue v) throws java.rmi.RemoteException {
    //  	return Constants.runtimeError.apply(new de.uni_sb.ps.dml.runtime.String("cannot apply "+this+" to "+v)).raise();
    //      }

    //      final public DMLValue raise() {
    //  	throw new ExceptionWrapper(this);
    //      }

    /** Falls der Name noch keinen GName hat, wird jetzt ein
     *  neuer GName erzeugt und der Name wird unter dem GName in
     *  der globalen Hashtabelle eingetragen.
     */
    private void writeObject(java.io.ObjectOutputStream out)
	throws java.io.IOException {
	// es wird _immer_ ein GName vergeben;
	//  	if (gName==null) {
	//  	    gName=new GName();
	//  	    GName.gNames.put(gName,this);
	//  	}
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
