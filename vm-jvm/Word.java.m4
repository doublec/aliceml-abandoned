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
 *  @see STRING 
 *  @see DMLValue
 */
final public class Word implements DMLValue {

    /** java-long Wert */
    final protected long value;

    /** Baut ein neues Word mit Wert <code>value</code>.
     *  @param value <code>long</code> Wert, der dem Word entspricht.
     */
    public Word(long value) {
	this.value=value;
    }

    /** Gleichheit auf Long-Werten */
    final public boolean equals(java.lang.Object val) {
	return (val instanceof Word) && (((Word)val).value==this.value);
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

    _apply_fails;

    /** <code>val wordSize : int </code>*/
    /** <code>val toLargeWord : word -> LargeWord.word </code>*/
    /** <code>val toLargeWordX : word -> LargeWord.word </code>*/
    /** <code>val fromLargeWord : LargeWord.word -> word </code>*/
    /** <code>val toLargeInt : word -> LargeInt.int </code>*/
    /** <code>val toLargeIntX : word -> LargeInt.int </code>*/
    /** <code>val fromLargeInt : LargeInt.int -> word </code>*/
    /** <code>val toInt : word -> Int.int </code>*/
    /** <code>val toIntX : word -> Int.int </code>*/
    /** <code>val fromInt : Int.int -> word </code>*/
    /** <code>val orb : (word * word) -> word </code>*/
    /** <code>val xorb : (word * word) -> word </code>*/
    /** <code>val andb : (word * word) -> word </code>*/
    /** <code>val notb : word -> word </code>*/
    /** <code>val << : (word * Word.word) -> word </code>*/
    /** <code>val >> : (word * Word.word) -> word </code>*/
    /** <code>val ~>> : (word * Word.word) -> word </code>*/
    /** <code>val + : (word * word) -> word </code>*/
    /** <code>val - : (word * word) -> word </code>*/
    /** <code>val * : (word * word) -> word </code>*/
    /** <code>val div : (word * word) -> word </code>*/
    /** <code>val mod : (word * word) -> word </code>*/
    /** <code>val compare : (word * word) -> order </code>*/
    /** <code>val > : (word * word) -> bool </code>*/
    /** <code>val < : (word * word) -> bool </code>*/
    /** <code>val >= : (word * word) -> bool </code>*/
    /** <code>val <= : (word * word) -> bool </code>*/
    /** <code>val min : (word * word) -> word </code>*/
    /** <code>val max : (word * word) -> word </code>*/
    /** <code>val fmt : java.lang.StringCvt.radix -> word -> string </code>*/
    /** <code>val toString : word -> string </code>*/
    /** <code>val fromString : string -> word option </code>*/
    /** <code>val scan : java.lang.StringCvt.radix -> (char, 'a) java.lang.StringCvt.reader -> 'a -> (word, 'a) option</code>*/
}
