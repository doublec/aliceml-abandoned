package de.uni_sb.ps.DML.DMLRuntime;

/** Diese Klasse repräsentiert DMLWord.
 *  @see DMLInt
 *  @see DMLReal
 *  @see DMLSCon
 *  @see DMLString
 *  @see DMLValue
 */
final public class DMLWord extends DMLSCon {

    /** java-long Wert */
    private long value=0;

    /** Gleichheit auf Long-Werten */
    final public boolean equals(Object val) {
	return (val instanceof DMLWord) && (((DMLWord)val).value==this.value);
    }

    /** Baut ein neues DMLWord mit Wert <code>value</code>.
     *  @param value <code>long</code> Wert, der dem DMLWord entspricht.
     */
    public DMLWord(long value) {
	this.value=value;
    }

    /** Stringdarstellung des Wertes erzeugen.
     *  @return String Stringdarstellung des Wertes
     */
    final public String toString() {
	return value+": word";
    }

    /** Den Java-Wert des DMLWord auslesen.
     *  @return long Java-Wert der dem DMLWord-Wert entspricht
     */
    final public long getLong() {
	return value;
    }
}
