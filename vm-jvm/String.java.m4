/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

/** Diese Klasse repräsentiert STRING .
 *  @see Int
 *  @see Real
 *  @see SCon
 *  @see DMLValue
 *  @see Word
 */
final public class String implements DMLValue {

    /** java-String Wert */
    final public java.lang.String value;

    /** Baut einen neuen STRING  mit Inhalt <code>value</code>.
     *  @param value <code>String</code> Wert, der dem STRING  entspricht.
     */
    public String(java.lang.String value) {
	this.value=value;
    }

    final public static boolean equals(STRING s, STRING t) {
	return s.equals(t);
    }

    /** Testet Gleichheit der Java-Strings */
    final public boolean equals(java.lang.Object val) {
	return (val instanceof STRING) &&
	    (((STRING) val).value.equals(this.value));
    }

    /** java.lang.Stringdarstellung des Wertes erzeugen.
     *  @return java.lang.String java.lang.Stringdarstellung des Wertes
     */
    final public java.lang.String toString() {
	return "\""+value+"\": string";
    }

    /** Den Java-Wert des STRING  auslesen.
     *  @return java.lang.String Java-Wert der dem STRING -Wert entspricht
     */
    final public java.lang.String getString() {
	return value;
    }
    _apply_fails;
    
    _BUILTIN(Size) {
	_APPLY(val) {
	    _fromSingle(val,"String.length");
            if (val instanceof STRING) {
		return new Int(((STRING) val).value.length());
            } else {
		_error("argument not String",val);
            }
        }
    }
    /** <code>val size : string -> int </code>*/
    _FIELD(String,size);

    _BUILTIN(Extract) {
	_APPLY(val) {
	    _fromTuple(args,val,3,"String.extract");
	    _REQUESTDEC(DMLValue v,args[0]);
	    if (!(v instanceof STRING)) {
		_error("argument 1 not String",val);
	    }
	    java.lang.String s = ((STRING) v).value;
	    _REQUEST(v,args[1]);
	    if (!(v instanceof Int)) {
		_error("argument 2 not Int",val);
	    }
	    int from = ((Int) v).value;
	    _REQUEST(v,args[2]);
	    int to = s.length();
	    if (v instanceof DMLConVal) {
		DMLConVal cv = (DMLConVal) v;
		if (cv.getConstructor() == Option.SOME) {
		    v = cv.getContent();
		    if (v instanceof Int) {
			to = ((Int) v).value;
			return new STRING (s.substring(from,to));
		    } else {
			_error("argument 3 not Int option",val);
		    }
		} else {
		    _error("argument 3 not Int option",val);
		}
	    } else if (v != Option.NONE) {
		_error("argument 3 not Int option",val);
	    } else {
		return new STRING (s.substring(from,to));
	    }
	}
    }
    /** <code>val extract : (string * int * int option) -> string </code>*/
    _FIELD(String,extract);

    _BUILTIN(Substring) {
	_APPLY(val) {
	    _fromTuple(args,val,3,"String.substring");
	    _REQUESTDEC(DMLValue v,args[0]);
	    if (!(v instanceof STRING)) {
		_error("argument 1 not String",val);
	    }
	    java.lang.String s = ((STRING) v).value;
	    _REQUEST(v,args[1]);
	    if (!(v instanceof Int)) {
		_error("argument 2 not Int",val);
	    }
	    int from = ((Int) v).value;
	    _REQUEST(v,args[2]);
	    if (!(v instanceof Int)) {
		_error("argument 3 not Int",val);
	    }
	    int to = ((Int) v).value;
	    return new STRING (s.substring(from,to));
	}
    }
    /** <code>val substring : (string * int * int) -> string </code>*/
    _FIELD(String,substring);

    _BUILTIN(Concat) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"String.concat");
	    _REQUESTDEC(DMLValue list,args[0]);
	    if (list==List.nil) {
		return new STRING ("");
	    } else if (list instanceof Cons) {
		StringBuffer buff = new StringBuffer();
		do {
		    if (list instanceof Cons) {
			Cons co = (Cons) list;
			if (co.car instanceof STRING) {
			    buff.append(((STRING) co.car).value);
			} else {
			    _error("argument not String list",val);
			}
			list = co.cdr;
		    } else {
			_error("argument not String list",val);
		    }
		} while (list != List.nil);
		return new STRING (buff.toString());
	    } else {
		_error("argument not String list",val);
	    }
	}
    }
    /** <code>val concat : string list -> string </code>*/
    _FIELD(String,concat);

    _BUILTIN(Append) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"String.^");
	    _REQUESTDEC(DMLValue v,args[0]);
	    if (!(v instanceof STRING)) {
		_error("argument 1 not String",val);
	    }
	    _REQUESTDEC(DMLValue w,args[1]);
	    if (!(w instanceof STRING)) {
		_error("argument 2 not String",val);
	    }
	    return new
		STRING (((STRING) v).value +
						((STRING) w).value);
	}
    }
    /** <code>val ^ : (string * string) -> string </code>*/
    _FIELD(String,append);

    _BUILTIN(IsPrefix) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"String.isPrefix");
	    _REQUESTDEC(DMLValue v,args[0]);
	    if (!(v instanceof STRING)) {
		_error("argument 1 not String",val);
	    }
	    java.lang.String s = ((STRING) v).value;
	    _REQUEST(v,args[1]);
	    if (!(v instanceof STRING)) {
		_error("argument 2 not String",val);
	    }
	    java.lang.String t = ((STRING) v).value;
	    if (s.startsWith(t)) {
		return Constants.dmltrue;
	    } else {
		return Constants.dmlfalse;
	    }
	}
    }
    /** <code>val isPrefix : string -> string -> bool </code>*/
    _FIELD(String,isPrefix);

    _BUILTIN(Compare) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"String.compare");
	    _REQUESTDEC(DMLValue v,args[0]);
	    if (!(v instanceof STRING)) {
		_error("argument 1 not String",val);
	    }
	    java.lang.String s = ((STRING) v).value;
	    _REQUEST(v,args[1]);
	    if (!(v instanceof STRING)) {
		_error("argument 2 not String",val);
	    }
	    java.lang.String t = ((STRING) v).value;
	    int cmp = s.compareTo(t);
	    if (cmp < 0) {
		return General.LESS;
	    } else if (cmp==0) {
		return General.EQUAL;
	    } else {
		return General.GREATER;
	    }
	}
    }
    /** <code>val compare : (string * string) -> order </code>*/
    _FIELD(String,compare);

    _BUILTIN(Compare_) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"String.compare'");
	    _REQUESTDEC(DMLValue v,args[0]);
	    if (!(v instanceof STRING)) {
		_error("argument 1 not String",val);
	    }
	    java.lang.String s = ((STRING) v).value;
	    _REQUEST(v,args[1]);
	    if (!(v instanceof STRING)) {
		_error("argument 2 not String",val);
	    }
	    java.lang.String t = ((STRING) v).value;
	    int cmp = s.compareTo(t);
	    if (cmp < 0) {
		return Int.MONE;
	    } else if (cmp==0) {
		return Int.ZERO;
	    } else {
		return Int.ONE;
	    }
	}
    }
    /** <code>val compare : (string * string) -> order </code>*/
    _FIELD(String,compare_);

    _COMPARESTRING(less,<);
    _COMPARESTRING(leq,<=);
    _COMPARESTRING(greater,>);
    _COMPARESTRING(geq,>=);

    _BUILTIN(Str) {
	_APPLY(val) {
	    _fromSingle(val,"String.str");
	    if (val instanceof Char) {
		return new STRING (java.lang.String.valueOf(((Char) val).value));
	    } else {
		_error("argument not char",val);
	    }
	}
    }
    /** <code>val str : Char.char -> string </code>*/
    _FIELD(String,str);

    _BUILTIN(Sub) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"String.sub");
	    _REQUESTDEC(DMLValue s,args[0]);
	    if (s instanceof STRING) {
		_REQUESTDEC(DMLValue idx,args[1]);
		if (idx instanceof Int) {
		    return new Char(((STRING) s).value.charAt(((Int) idx).value));
		} else {
		    _error("argument 2 not int",val);
		}
	    } else {
		_error("argument 1 not string",val);
	    }
	}
    }
    /** <code>val sub : (string * int) -> Char.char </code>*/
    _FIELD(String,sub);

    /** <code>structure Char : CHAR </code>*/
    /** <code>val maxSize : int </code>*/
    /** <code>val fromString : java.lang.String.string -> string option </code>*/
    /** <code>val toString : string -> java.lang.String.string </code>*/
    /** <code>val fromCString : java.lang.String.string -> string option </code>*/
    /** <code>val toCString : string -> java.lang.String.string </code>*/
    /** <code>val implode : Char.char list -> string </code>*/
    /** <code>val explode : string -> Char.char list </code>*/
    /** <code>val map : (Char.char -> Char.char) -> string -> string </code>*/
    /** <code>val translate : (Char.char -> string) -> string -> string </code>*/
    /** <code>val tokens : (Char.char -> bool) -> string -> string list </code>*/
    /** <code>val fields : (Char.char -> bool) -> string -> string list </code>*/
    /** <code>val collate : ((Char.char * Char.char) -> order) -> (string * string) -> order </code>*/
}
