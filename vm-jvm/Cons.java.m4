/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

final public class Cons implements DMLConVal {

    protected DMLValue car=null;
    protected DMLValue cdr=null;

    public Cons(DMLValue fst, DMLValue snd) {
	car=fst;
	cdr=snd;
    }

    final public DMLValue getCar() {
	return car;
    }

    final public DMLValue getCdr() {
	return cdr;
    }

    final public void setCdr(DMLValue val) {
	cdr = val;
    }

    public DMLValue get0() { return car; }
    public DMLValue get1() { return cdr; }
    public DMLValue get2() { throw new ArrayIndexOutOfBoundsException(); }
    public DMLValue get3() { throw new ArrayIndexOutOfBoundsException(); }
    public DMLValue get4() { throw new ArrayIndexOutOfBoundsException(); }

    final public DMLValue getContent() {
	return new Tuple2(car,cdr);
    }

    final public Constructor getConstructor() {
	return List.cons;
    }

    /** wirft Fehler */
    final public DMLValue assign(DMLValue val) throws java.rmi.RemoteException {
	_RAISE(runtimeError,new STRING ("cannot assign "+val+" to "+this));
    }

    final public java.lang.String toString() {
	return "("+car+". "+cdr+")";
    }

    /** Gleichheit der  und Inhalte */
    final public boolean equals(java.lang.Object val) {
	return (val instanceof Cons) &&
	    car.equals(((Cons) val).car) &&
	    cdr.equals(((Cons) val).cdr);
    }

    _apply_fails ;
    _request_id ;
    _getValue_id ;
    _raise ;
}
