/*
 * Author:
 *      Daniel Simon, <dansim@ps.uni-sb.de>
 *
 * Copyright:
 *      Daniel Simon, 1999
 *
 * Last change:
 *    $Date$ by $Author$
 * $Revision$
 *
 */
package de.uni_sb.ps.dml.runtime;

final public class Char implements DMLValue {

    final public char value;

    public Char(char c) {
	value=c;
    }

    final public boolean equals(Object o) {
	if (o instanceof Char) {
	    return (((Char) o).value==value);
	} else if (o instanceof DMLTransient) {
	    return o.equals(this);
	} else {
	    return false;
	}
    }

    final public java.lang.String toString() {
	return value+" : char";
    }

    _apply_fails;

    // eqtype string
    /** <code>val minChar : char </code>*/
    /** <code>val maxChar : char </code>*/
    /** <code>val maxOrd : int </code>*/
    _BUILTIN(Ord) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Char.ord");
	    if (val instanceof Char) {
		return new Int(((Char) val).value);
	    } else {
		_error("argument not char",val);
	    }
	}
    }
    /** <code>val ord : char -> int </code>*/
    _FIELD(Char,ord);

    _BUILTIN(Chr) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Char.chr");
	    if (val instanceof Int) {
		int i = ((Int) val).value; // better?: val.value
		if (i <= Character.MAX_VALUE && i >= Character.MIN_VALUE) {
		    return new Char((char) i);
		} else {
		    return General.Chr;
		}
	    } else {
		_error("argument not char",val);
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

    _BUILTIN(IsDigit) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Char.isDigit");
	    if (val instanceof Char) {
		return (Character.isDigit(((Char) val).value) ?
		    Constants.dmltrue :
			Constants.dmlfalse);
	    } else {
		_error("argument not char",val);
	    }
	}
    }
    /** <code>val isDigit : char -> bool </code>*/
    _FIELD(Char,isDigit);

    /** <code>val isGraph : char -> bool </code>*/

    _BUILTIN(IsHexDigit) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Char.isHexDigit");
	    if (val instanceof Char) {
		char c = ((Char) val).value;
		switch (c) {
		case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
		case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
		case '0': case '1': case '2': case '3': case '4': case '5':
		case '6': case '7': case '8': case '9':
			return Constants.dmltrue;
		default:
		    return Constants.dmlfalse;
		}
	    } else {
		_error("argument not char",val);
	    }
	}
    }
    /** <code>val isHexDigit : char -> bool </code>*/
    _FIELD(Char,isHexDigit);

    /** <code>val isLower : char -> bool </code>*/
    /** <code>val isPrint : char -> bool </code>*/

    _BUILTIN(IsSpace) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"Char.isSpace");
	    if (val instanceof Char) {
		return (Character.isWhitespace(((Char) val).value) ?
		    Constants.dmltrue :
			Constants.dmlfalse);
	    } else {
		_error("argument not char",val);
	    }
	}
    }
    /** <code>val isSpace : char -> bool </code>*/
    _FIELD(Char,isSpace);

    /** <code>val isPunct : char -> bool </code>*/
    /** <code>val isUpper : char -> bool </code>*/
    /** <code>val fromString : java.lang.String.string -> char option </code>*/
    /** <code>val scan : (Char.char, 'a) java.lang.StringCvt.reader -> 'a -> (char * 'a) option </code>*/
    /** <code>val toString : char -> java.lang.String.string </code>*/
    /** <code>val fromCString : java.lang.String.string -> char option </code>*/
    _BUILTIN(ToCString) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    return new STRING (java.lang.String.valueOf(((Char) val).value));
	}
    }
    /** <code>val toCString : char -> java.lang.String.string </code>*/
    _FIELD(Char,toCString);
}
