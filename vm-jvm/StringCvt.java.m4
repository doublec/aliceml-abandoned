/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.builtin;

import de.uni_sb.ps.dml.runtime.*;

final public class StringCvt {
    /** datatype radix = BIN | OCT | DEC | HEX */
    /** datatype realfmt
	= SCI of int option 
	| FIX of int option 
	| GEN of int option 
	| EXACT
    */
    /** type ('a, 'b) reader = 'b -> ('a * 'b) option*/
    /** <code>val padLeft : char -> int -> string -> string </code>*/
    /** <code>val padRight : char -> int -> string -> string </code>*/
    /** <code>val splitl : (char -> bool) -> (char, 'a) reader ->'a -> (string * 'a) </code>*/
    /** <code>val takel : (char -> bool) -> (char, 'a) reader ->'a -> string </code>*/
    /** <code>val dropl : (char -> bool) -> (char, 'a) reader ->'a -> 'a </code>*/
    /** <code>val skipWS : (char, 'a) reader -> 'a -> 'a </code>*/
    /**type cs*/
    /** <code>val scanString : ((char, cs) reader -> ('a, cs) reader) -> string -> 'a option</code>*/
}
