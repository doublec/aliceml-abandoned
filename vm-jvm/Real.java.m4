/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

/** Diese Klasse repräsentiert Real.
 *  @see Int
 *  @see SCon
 *  @see de.uni_sb.ps.dml.runtime.String
 *  @see DMLValue
 *  @see Word
 */
final public class Real extends SCon {

    /** java-float Wert */
    private float value=0.0f;

    /** Baut einen neuen Real mit Inhalt <code>value</code>.
     *  @param value <code>float</code> Wert, der dem Real entspricht.
     */
    public Real(float value) {
	this.value=value;
    }

    /** Gleichheit der Real-Werte (Java-Floats) */
    final public boolean equals(java.lang.Object val) {
	return (val instanceof Real) && (((Real) val).value==this.value);
    }

    /** java.lang.Stringdarstellung des Wertes erzeugen.
     *  @return java.lang.String java.lang.Stringdarstellung des Wertes
     */
    final public java.lang.String toString() {
	return value+": real";
    }

    /** Den Java-Wert des Real auslesen.
     *  @return float Java-Wert der dem Real-Wert entspricht
     */
    final public float getFloat() {
	return value;
    }
}
