/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

/** Diese Klasse repräsentiert Word.
 *  @see Int
 *  @see Real
 *  @see SCon
 *  @see de.uni_sb.ps.dml.runtime.String
 *  @see DMLValue
 */
final public class Word extends SCon {

    /** java-long Wert */
    private long value=0;

    /** Gleichheit auf Long-Werten */
    final public boolean equals(java.lang.Object val) {
	return (val instanceof Word) && (((Word)val).value==this.value);
    }

    /** Baut ein neues Word mit Wert <code>value</code>.
     *  @param value <code>long</code> Wert, der dem Word entspricht.
     */
    public Word(long value) {
	this.value=value;
    }

    /** java.lang.Stringdarstellung des Wertes erzeugen.
     *  @return java.lang.String java.lang.Stringdarstellung des Wertes
     */
    final public java.lang.String toString() {
	return value+": word";
    }

    /** Den Java-Wert des Word auslesen.
     *  @return long Java-Wert der dem Word-Wert entspricht
     */
    final public long getLong() {
	return value;
    }
}
