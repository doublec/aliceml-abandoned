package de.uni_sb.ps.DML.DMLRuntime;

/** Diese Klasse repräsentiert DMLString.
 *  @see DMLInt
 *  @see DMLReal
 *  @see DMLSCon
 *  @see DMLValue
 *  @see DMLWord
 */
final public class DMLString extends DMLSCon {

    /** java-String Wert */
    private String value=null;

    /** Testet Gleichheit der Java-Strings */
    final public boolean equals(Object val) {
	return (val instanceof DMLString) &&
	    (((DMLString) val).value.equals(this.value));
    }

    /** Baut einen neuen DMLString mit Inhalt <code>value</code>.
     *  @param value <code>String</code> Wert, der dem DMLString entspricht.
     */
   public DMLString(String value) {
	this.value=value;
    }
    
    /** Stringdarstellung des Wertes erzeugen.
     *  @return String Stringdarstellung des Wertes
     */
    final public String toString() {
	return "\""+value+"\": string";
    }

    /** Den Java-Wert des DMLString auslesen.
     *  @return String Java-Wert der dem DMLString-Wert entspricht
     */
    final public String getString() {
	return value;
    }
}
