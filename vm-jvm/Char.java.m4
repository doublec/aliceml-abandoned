/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

final public class Char extends SCon {
    private char ch = '_';

    public Char(char c) {
	ch=c;
    }

    final public boolean equals(java.lang.Object o) {
	return (o instanceof Char) && (((Char) o).ch==ch);
    }

    final public java.lang.String toString() {
	return ch+" : char";
    }

    final public char getChar() {
	return ch;
    }
    // eqtype string
    /** <code>val minChar : char </code>*/
    /** <code>val maxChar : char </code>*/
    /** <code>val maxOrd : int </code>*/
    _BUILTIN(Ord) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"Char.ord");
	    DMLValue ch = args[0].request();
	    if (ch instanceof Char) {
		return new Int(((Char) ch).ch);
	    } else {
		return _error("argument not char",val);
	    }
	}
    }
    /** <code>val ord : char -> int </code>*/
    _FIELD(Char,ord);

    _BUILTIN(Chr) {
	_APPLY(val) {
	    _fromTuple(args,val,1,"Char.chr");
	    DMLValue ch = args[0].request();
	    if (ch instanceof Int) {
		int i = ((Int) ch).getInt(); // better?: ch.value
		if (i <= Character.MAX_VALUE && i >= Character.MIN_VALUE) {
		    return new Char((char) i);
		} else {
		    return General.Chr;
		}
	    } else {
		return _error("argument not char",val);
	    }
	}
    }
    /** <code>val chr : int -> char </code>*/
    _FIELD(Char,chr);

    /** <code>val succ : char -> char </code>*/
    /** <code>val pred : char -> char </code>*/
    /** <code>val < : (char * char) -> bool </code>*/
    /** <code>val <= : (char * char) -> bool </code>*/
    /** <code>val > : (char * char) -> bool </code>*/
    /** <code>val >= : (char * char) -> bool </code>*/
    /** <code>val compare : (char * char) -> order </code>*/
    /** <code>val contains : string -> char -> bool </code>*/
    /** <code>val notContains : string -> char -> bool </code>*/
    /** <code>val toLower : char -> char </code>*/
    /** <code>val toUpper : char -> char </code>*/
    /** <code>val isAlpha : char -> bool </code>*/
    /** <code>val isAlphaNum : char -> bool </code>*/
    /** <code>val isAscii : char -> bool </code>*/
    /** <code>val isCntrl : char -> bool </code>*/
    /** <code>val isDigit : char -> bool </code>*/
    /** <code>val isGraph : char -> bool </code>*/
    /** <code>val isHexDigit : char -> bool </code>*/
    /** <code>val isLower : char -> bool </code>*/
    /** <code>val isPrint : char -> bool </code>*/
    /** <code>val isSpace : char -> bool </code>*/
    /** <code>val isPunct : char -> bool </code>*/
    /** <code>val isUpper : char -> bool </code>*/
    /** <code>val fromString : java.lang.String.string -> char option </code>*/
    /** <code>val scan : (Char.char, 'a) java.lang.StringCvt.reader -> 'a -> (char * 'a) option </code>*/
    /** <code>val toString : char -> java.lang.String.string </code>*/
    /** <code>val fromCString : java.lang.String.string -> char option </code>*/
    /** <code>val toCString : char -> java.lang.String.string </code>*/
}
