package de.uni_sb.ps.dml.runtime;

final public class NoGood implements DMLValue {

    private GName gName = null;

    public NoGood(GName gn) {
	gName = gn;
    }

    final public DMLValue getValue() {
	return this;
    }

    final public DMLValue request() {
	return this;
    }

    final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException {
	return Constants.runtimeError.apply( new de.uni_sb.ps.dml.runtime.String("cannot apply "+this+" to "+val)).raise();
    }

    final public DMLValue raise() {
	throw new ExceptionWrapper(this);
    }

    /** Falls unter diesm GName ein Objekt eingetragen ist, wird
     *  dieses verwendet, aus einem NoGood wird wieder ein Good.
     */
    private java.lang.Object readResolve()
	throws java.io.ObjectStreamException {
	java.lang.Object o = GName.gNames.get(gName);
	if (o==null) {
	    return this;
	} else {
	    return o;
	}
    }
}

