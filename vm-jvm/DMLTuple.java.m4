package de.uni_sb.ps.dml.runtime;

/** Tuple-Darstellung in DML.
 *  @see Record
 */
public interface DMLTuple extends DMLValue {
    /** gibt den i-ten Eintrag des Tuples oder Records*/
    public DMLValue getByIndex(int i);

    /** gibt die Stelligkeit des Tuples oder Records an */
    public int getArity();

    public DMLValue[] getVals();

    public DMLValue get0();
    public DMLValue get1();
    public DMLValue get2();
    public DMLValue get3();
    public DMLValue get4();
}
