package de.uni_sb.ps.dml.runtime;

final public class NoGood implements DMLValue {

    private GName gName = null;

    public NoGood(GName gn) {
	gName = gn;
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

    _apply_fails ;
    _request_id ;
    _getValue_id ;
    _raise ;
}

