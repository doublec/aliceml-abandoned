package de.uni_sb.ps.DML.DMLRuntime;

/** Diese Klasse repräsentiert DMLReal.
 *  @see DMLInt
 *  @see DMLSCon
 *  @see DMLString
 *  @see DMLValue
 *  @see DMLWord
 */
final public class DMLReal extends DMLSCon {

    /** java-float Wert */
    private float value=0.0f;

    /** Gleichheit der Real-Werte (Java-Floats) */
    final public boolean equals(Object val) {
	return (val instanceof DMLReal) && (((DMLReal) val).value==this.value);
    }

    /** Baut einen neuen DMLReal mit Inhalt <code>value</code>.
     *  @param value <code>float</code> Wert, der dem DMLReal entspricht.
     */
    public DMLReal(float value) {
	this.value=value;
    }

    /** Stringdarstellung des Wertes erzeugen.
     *  @return String Stringdarstellung des Wertes
     */
    final public String toString() {
	return value+": real";
    }
    
    /** Den Java-Wert des DMLReal auslesen.
     *  @return float Java-Wert der dem DMLReal-Wert entspricht
     */
    final public float getFloat() {
	return value;
    }
}
