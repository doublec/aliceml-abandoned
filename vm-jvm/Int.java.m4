package de.uni_sb.ps.dml.runtime;

/** Diese Klasse repräsentiert DMLInt.
 *  @see DMLReal
 *  @see DMLSCon
 *  @see DMLString
 *  @see DMLValue
 *  @see DMLWord
 */
final public class DMLInt extends DMLSCon {
    /** java-int Wert */
    private int value=0;

    /** Gleichheit auf Integer-Werten */
    final public boolean equals(Object val) {
	return (val instanceof DMLInt) && (((DMLInt) val).value==this.value);
    }

    /** Baut einen neuen DMLInt mit Wert <code>value</code>.
     *  @param value <code>int</code> Wert, der dem DMLInt entspricht.
     */
    public DMLInt(int value) {
	this.value=value;
    }

    /** Stringdarstellung des Wertes erzeugen.
     *  @return String Stringdarstellung des Wertes
     */
    final public String toString() {
	return value+": int or char";
    }

    /** Den Java-Wert des DMLInt auslesen.
     *  @return int Java-Wert der dem DMLInt-Wert entspricht
     */
    final public int getInt() {
	return value;
    }
}
