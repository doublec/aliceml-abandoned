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

    /** Baut einen neuen de.uni_sb.ps.dml.runtime.String mit Inhalt <code>value</code>.
     *  @param value <code>String</code> Wert, der dem de.uni_sb.ps.dml.runtime.String entspricht.
     */
    public String(java.lang.String value) {
	this.value=value;
    }

    /** Testet Gleichheit der Java-Strings */
    final public boolean equals(java.lang.Object val) {
	return (val instanceof de.uni_sb.ps.dml.runtime.String) &&
	    (((de.uni_sb.ps.dml.runtime.String) val).value.equals(this.value));
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
    /** <code>structure Char : CHAR </code>*/
    /** <code>val maxSize : int </code>*/

    _BUILTIN(Size) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"String.length");
            DMLValue v = args[0].request();
            if (v instanceof de.uni_sb.ps.dml.runtime.String) {
		return new Int(((de.uni_sb.ps.dml.runtime.String) v).getString().length());
            } else {
		return _error("argument not String",val);
            }
        }
    }
    /** <code>val size : string -> int </code>*/
    _FIELD(size);

    /** <code>val sub : (string * int) -> Char.char </code>*/
    /** <code>val extract : (string * int * int option) -> string </code>*/
    /** <code>val substring : (string * int * int) -> string </code>*/
    /** <code>val concat : string list -> string </code>*/
    _BUILTIN(Concat) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"String.^");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.String)) {
		return _error("argument 1 not String",val);
	    }
	    DMLValue w = args[1].request();
	    if (!(w instanceof de.uni_sb.ps.dml.runtime.String)) {
		return _error("argument 2 not String",val);
	    }
	    return new
		de.uni_sb.ps.dml.runtime.String(((de.uni_sb.ps.dml.runtime.String) v).getString() +
						((de.uni_sb.ps.dml.runtime.String) w).getString());
	}
    }
    /** <code>val ^ : (string * string) -> string </code>*/
    _FIELD(concat);

    /** <code>val str : Char.char -> string </code>*/
    /** <code>val implode : Char.char list -> string </code>*/
    /** <code>val explode : string -> Char.char list </code>*/
    /** <code>val map : (Char.char -> Char.char) -> string -> string </code>*/
    /** <code>val translate : (Char.char -> string) -> string -> string </code>*/
    /** <code>val tokens : (Char.char -> bool) -> string -> string list </code>*/
    /** <code>val fields : (Char.char -> bool) -> string -> string list </code>*/
    /** <code>val isPrefix : string -> string -> bool </code>*/
    /** <code>val compare : (string * string) -> order </code>*/
    /** <code>val collate : ((Char.char * Char.char) -> order) -> (string * string) -> order </code>*/
    /** <code>val < : (string * string) -> bool </code>*/
    /** <code>val <= : (string * string) -> bool </code>*/
    /** <code>val > : (string * string) -> bool </code>*/
    /** <code>val >= : (string * string) -> bool </code>*/
    /** <code>val fromString : java.lang.String.string -> string option </code>*/
    /** <code>val toString : string -> java.lang.String.string </code>*/
    /** <code>val fromCString : java.lang.String.string -> string option </code>*/
    /** <code>val toCString : string -> java.lang.String.string </code>*/
}
