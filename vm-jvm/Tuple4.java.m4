/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

/** Special class for tuples with arity 4.
 */
final public class Tuple4 implements DMLTuple {

    private DMLValue fst = null;
    private DMLValue snd = null;
    private DMLValue thr = null;
    private DMLValue fur = null;

    public Tuple4(DMLValue eins,
		  DMLValue zwei,
		  DMLValue drei,
		  DMLValue vier) {
	fst=eins;
	snd=zwei;
	thr=drei;
	fur=vier;
    }

    public DMLValue get0() { return fst; }
    public DMLValue get1() { return snd; }
    public DMLValue get2() { return thr; }
    public DMLValue get3() { return fur; }
    public DMLValue get4() { throw new ArrayIndexOutOfBoundsException(); }

    final public boolean equals(java.lang.Object val) {
	return (val instanceof Tuple4) &&
	    fst.equals(((Tuple4) val).fst) &&
	    snd.equals(((Tuple4) val).snd) &&
	    thr.equals(((Tuple4) val).thr) &&
	    fur.equals(((Tuple4) val).fur);
    }

    final public java.lang.String toString() {
	return "("+fst+", "+snd+", "+thr+", "+fur+")";
    }

    /** gibt den i-ten Eintrag des Tuples oder Records*/
    final public DMLValue getByIndex(int i){
	switch (i) {
	case 0: return fst;
	case 1: return snd;
	case 2: return thr;
	case 3: return fur;
	default: throw new ArrayIndexOutOfBoundsException();
	}
    }

    /** gibt die Stelligkeit des Tuples oder Records an */
    final public int getArity() {
	return 4;
    }

    final public DMLValue[] getVals() {
	DMLValue[] vals = {fst,snd,thr,fur};
	return vals;
    }

    _apply_fails ;
    _request_id ;
    _getValue_id ;
    _raise ;
}
