/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

/** Special class for tuples with arity 2.
 */
final public class Tuple2 implements DMLTuple {

    private DMLValue fst = null;
    private DMLValue snd = null;

    public Tuple2(DMLValue eins,
		  DMLValue zwei) {
	fst=eins;
	snd=zwei;
    }

    final public DMLValue get0() { return fst; }
    final public DMLValue get1() { return snd; }
    final public DMLValue get2() { throw new ArrayIndexOutOfBoundsException(); }
    final public DMLValue get3() { throw new ArrayIndexOutOfBoundsException(); }
    final public DMLValue get4() { throw new ArrayIndexOutOfBoundsException(); }

    /** Gleichheit der  und Inhalte */
    final public boolean equals(java.lang.Object val) {
	if (val instanceof Tuple2) {
	    Tuple2 v = (Tuple2) val;
	    return
		fst.equals(v.fst) &&
		snd.equals(v.snd);
	} else if (val instanceof DMLTuple) {
	    DMLTuple t = (DMLTuple) val;
	    if (t.getArity()!=2) {
		return false;
	    } else {
		return
		    t.getByIndex(0).equals(fst) &&
		    t.getByIndex(1).equals(snd);
	    }
	} else {
	    return false;
	}
    }

    final public java.lang.String toString() {
	return "("+fst+", "+snd+")/2";
    }

    /** gibt den i-ten Eintrag des Tuples oder Records*/
    final public DMLValue getByIndex(int i){
	switch (i) {
	case 0: return fst;
	case 1: return snd;
	default: throw new ArrayIndexOutOfBoundsException();
	}
    }

    /** gibt die Stelligkeit des Tuples oder Records an */
    final public int getArity() {
	return 2;
    }

    final public DMLValue[] getVals() {
	DMLValue[] vals = {fst,snd};
	return vals;
    }

    _apply_fails ;
    _request_id ;
    _getValue_id ;
    _raise ;
}
