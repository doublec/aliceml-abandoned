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

public class Constructor implements DMLValue {

    GName gName = null;

    public Constructor() {
	this.gName = null;
    }

    public java.lang.String toString() {
	return "unnamed constructor";
    }

    public DMLValue apply(DMLValue val)
	throws java.rmi.RemoteException {
	return new ConVal(this,val);
    }

    /** Falls der Constructor noch keinen GName hat, wird jetzt ein
     *  neuer GName erzeugt und der Constructor wird unter dem GName in
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
     *  zur�ckgeliefert.
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
}
