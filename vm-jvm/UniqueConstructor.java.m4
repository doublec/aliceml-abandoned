package de.uni_sb.ps.dml.runtime;

final public class UniqueConstructor extends Constructor {

    //      java.lang.String name = null;
    //      GName gName = null;

    public UniqueConstructor(java.lang.String name) {
	this.name=name;
	GName.gNames.put(name,this);
    }
    //    ererbt
    //      /** Pointergleichheit */
    //      final public boolean equals(java.lang.Object val) {
    //  	return (val == this);
    //      }

    //      final public java.lang.String toString() {
    //  	return this.name+" : constructor";
    //      }

    //      final public DMLValue apply(DMLValue val) {
    //  	return new ConVal(this,val);
    //      }

    //      final public DMLValue getValue() {
    //  	return this;
    //      }

    //      final public DMLValue request() {
    //  	return this;
    //      }

    //      final public DMLValue raise() {
    //  	throw new ExceptionWrapper(this);
    //      }

    /** @see UniqueName */
    private void writeObject(java.io.ObjectOutputStream out)
	throws java.io.IOException {
	//  	if (gName==null) {
	//  	    gName=new GName();
	//  	    GName.gNames.put(gName,this);
	//  	}
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
	java.lang.Object o = GName.gNames.get(name);
	return o;
    }
}
