/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

/** Diese Klasse repräsentiert Int.
 *  @see Real
 *  @see SCon
 *  @see de.uni_sb.ps.dml.runtime.String
 *  @see DMLValue
 *  @see Word
 */
final public class Int extends SCon {
    /** java-int Wert */
    private int value=0;

    /** Gleichheit auf Integer-Werten */
    final public boolean equals(java.lang.Object val) {
	return (val instanceof Int) && (((Int) val).value==this.value);
    }

    /** Baut einen neuen Int mit Wert <code>value</code>.
     *  @param value <code>int</code> Wert, der dem Int entspricht.
     */
    public Int(int value) {
	this.value=value;
    }

    /** java.lang.Stringdarstellung des Wertes erzeugen.
     *  @return java.lang.String java.lang.Stringdarstellung des Wertes
     */
    final public java.lang.String toString() {
	return value+": int or char";
    }

    /** Den Java-Wert des Int auslesen.
     *  @return int Java-Wert der dem Int-Wert entspricht
     */
    final public int getInt() {
	return value;
    }
}
