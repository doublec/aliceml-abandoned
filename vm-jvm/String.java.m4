/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

/** Diese Klasse repräsentiert de.uni_sb.ps.dml.runtime.String.
 *  @see Int
 *  @see Real
 *  @see SCon
 *  @see DMLValue
 *  @see Word
 */
final public class String extends SCon {

    /** java-String Wert */
    private java.lang.String value=null;

    /** Testet Gleichheit der Java-Strings */
    final public boolean equals(java.lang.Object val) {
	return (val instanceof de.uni_sb.ps.dml.runtime.String) &&
	    (((de.uni_sb.ps.dml.runtime.String) val).value.equals(this.value));
    }

    /** Baut einen neuen de.uni_sb.ps.dml.runtime.String mit Inhalt <code>value</code>.
     *  @param value <code>String</code> Wert, der dem de.uni_sb.ps.dml.runtime.String entspricht.
     */
   public String(java.lang.String value) {
	this.value=value;
    }
    
    /** java.lang.Stringdarstellung des Wertes erzeugen.
     *  @return java.lang.String java.lang.Stringdarstellung des Wertes
     */
    final public java.lang.String toString() {
	return "\""+value+"\": string";
    }

    /** Den Java-Wert des de.uni_sb.ps.dml.runtime.String auslesen.
     *  @return java.lang.String Java-Wert der dem de.uni_sb.ps.dml.runtime.String-Wert entspricht
     */
    final public java.lang.String getString() {
	return value;
    }
}
