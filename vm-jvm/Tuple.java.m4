/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

/** Tuple-Darstellung in DML.
 *  @see Record
 */
public class Tuple implements DMLTuple {

    protected DMLValue vals[]=null;

    public Tuple(DMLValue[] vals) {
	this.vals=vals;
    }

    public DMLValue get0() { return vals[0]; }
    public DMLValue get1() { return vals[1]; }
    public DMLValue get2() { return vals[2]; }
    public DMLValue get3() { return vals[3]; }
    public DMLValue get4() { return vals[4]; }

    public boolean equals(java.lang.Object val) {
	if (val instanceof Record) {
	    return false;
	} else if (val instanceof Tuple) {
	    Tuple r = (Tuple) val;
	    if (r.vals.length != this.vals.length)
		return false;
	    int length=vals.length;
	    for(int i=0; i<length; i++)
		if (!vals[i].equals(r.vals[i]))
		    return false;
	    return true;
	} else if (val instanceof DMLTuple) {
	    switch (vals.length) {
	    case 1:
		if (val instanceof Tuple1) {
		    return vals[0].equals(((Tuple1) val).getByIndex(0));
		} else {
		    return false;
		}
	    case 2:
		if (val instanceof Tuple2) {
		    return
			vals[0].equals(((Tuple2) val).getByIndex(0)) &&
			vals[1].equals(((Tuple2) val).getByIndex(1));
		} else {
		    return false;
		}
	    case 3:
		if (val instanceof Tuple3) {
		    return
			vals[0].equals(((Tuple3) val).getByIndex(0)) &&
			vals[1].equals(((Tuple3) val).getByIndex(1)) &&
			vals[2].equals(((Tuple3) val).getByIndex(2));
		} else {
		    return false;
		}
	    case 4:
		if (val instanceof Tuple4) {
		    return
			vals[0].equals(((Tuple4) val).getByIndex(0)) &&
			vals[1].equals(((Tuple4) val).getByIndex(1)) &&
			vals[2].equals(((Tuple4) val).getByIndex(2)) &&
			vals[3].equals(((Tuple4) val).getByIndex(3));
		} else {
		    return false;
		}
	    default:
		return false;
	    }
	} else {
	    return false;
	}
    }

    public java.lang.String toString() {
	java.lang.String s="(";
	int i;
	for (i=0; i<vals.length;i++) {
	    if (i>0) s+=", ";
	    s+=vals[i];
	}
	return s+")/"+getArity();
    }

    /** gibt den i-ten Eintrag des Tuples oder Records*/
    final public DMLValue getByIndex(int i){
	return vals[i];
    }

    /** gibt die Stelligkeit des Tuples oder Records an */
    final public int getArity() {
	return vals.length;
    }

    final public DMLValue[] getVals() {
	return vals;
    }

    _apply_fails ;
    _request_id ;
    _getValue_id ;
    _raise ;
}
