/* $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

/** Special class for tuples with arity 3.
 */
final public class Tuple3 implements DMLTuple {

    final protected DMLValue fst;
    final protected DMLValue snd;
    final protected DMLValue thr;

    public Tuple3(DMLValue eins,
		  DMLValue zwei,
		  DMLValue drei) {
	fst=eins;
	snd=zwei;
	thr=drei;
    }

    public DMLValue get0() { return fst; }
    public DMLValue get1() { return snd; }
    public DMLValue get2() { return thr; }
    public DMLValue get3() { throw new ArrayIndexOutOfBoundsException(); }
    public DMLValue get4() { throw new ArrayIndexOutOfBoundsException(); }

    /** Gleichheit der  und Inhalte */
    final public boolean equals(java.lang.Object val) {
	if (val instanceof Tuple3) {
	    Tuple3 v = (Tuple3) val;
	    return
		fst.equals(v.fst) &&
		snd.equals(v.snd) &&
		thr.equals(v.thr);
	} else if (val instanceof DMLTuple) {
	    DMLTuple t = (DMLTuple) val;
	    if (t.getArity()!=3) {
		return false;
	    } else {
		return
		    t.get0().equals(fst) &&
		    t.get1().equals(snd) &&
		    t.get2().equals(thr);
	    }
	} else {
	    return false;
	}
    }

    public java.lang.String toString() {
	return "("+fst+", "+snd+", "+thr+")/3";
    }

    /** gibt den i-ten Eintrag des Tuples oder Records*/
    final public DMLValue get(int i){
	switch (i) {
	case 0: return fst;
	case 1: return snd;
	case 2: return thr;
	default: throw new ArrayIndexOutOfBoundsException();
	}
    }

    final public DMLValue get(java.lang.String i) {
	_RAISE(runtimeError,new STRING ("no such label in tuple: "+i));
    }

    final public DMLValue get(Label i) {
	_RAISE(runtimeError,new STRING ("no such label in tuple: "+i));
    }

    /** gibt die Stelligkeit des Tuples oder Records an */
    final public int getArity() {
	return 3;
    }

    final public DMLValue[] getVals() {
	DMLValue[] vals = {fst,snd,thr};
	return vals;
    }

    _apply_fails ;
    _request_id ;
    _getValue_id ;
    _raise ;
}
