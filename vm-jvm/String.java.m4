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
    _FIELD(String,size);

    _BUILTIN(Extract) {
	_APPLY(val) {
	    _fromTuple(args,val,3,"String.extract");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.String)) {
		return _error("argument 1 not String",val);
	    }
	    java.lang.String s = ((de.uni_sb.ps.dml.runtime.String) v).getString();
	    v = args[1].request();
	    if (!(v instanceof Int)) {
		return _error("argument 2 not Int",val);
	    }
	    int from = ((Int) v).getInt();
	    v= args[2].request();
	    int to = s.length();
	    if (v instanceof DMLConVal) {
		DMLConVal cv = (DMLConVal) v;
		if (cv.getConstructor() == Option.SOME) {
		    v = cv.getContent();
		    if (v instanceof Int) {
			to = ((Int) v).getInt();
			return new de.uni_sb.ps.dml.runtime.String(s.substring(from,to));
		    } else {
			return _error("argument 3 not Int option",val);
		    }
		} else {
		    return _error("argument 3 not Int option",val);
		}
	    } else if (v != Option.NONE) {
		return _error("argument 3 not Int option",val);
	    } else {
		return new de.uni_sb.ps.dml.runtime.String(s.substring(from,to));
	    }
	}
    }
    /** <code>val extract : (string * int * int option) -> string </code>*/
    _FIELD(String,extract);

    _BUILTIN(Substring) {
	_APPLY(val) {
	    _fromTuple(args,val,3,"String.substring");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.String)) {
		return _error("argument 1 not String",val);
	    }
	    java.lang.String s = ((de.uni_sb.ps.dml.runtime.String) v).getString();
	    v = args[1].request();
	    if (!(v instanceof Int)) {
		return _error("argument 2 not Int",val);
	    }
	    int from = ((Int) v).getInt();
	    v= args[2].request();
	    if (!(v instanceof Int)) {
		return _error("argument 3 not Int",val);
	    }
	    int to = ((Int) v).getInt();
	    return new de.uni_sb.ps.dml.runtime.String(s.substring(from,to));
	}
    }
    /** <code>val substring : (string * int * int) -> string </code>*/
    _FIELD(String,substring);

    _BUILTIN(Concat) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"String.concat");
	    DMLValue list = args[0].request();
	    if (list==List.nil) {
		return new de.uni_sb.ps.dml.runtime.String("");
	    } else if (list instanceof Cons) {
		StringBuffer buff = new StringBuffer();
		do {
		    if (list instanceof Cons) {
			Cons co = (Cons) list;
			if (co.car instanceof de.uni_sb.ps.dml.runtime.String) {
			    buff.append(((de.uni_sb.ps.dml.runtime.String) co.car).getString());
			} else {
			    return _error("argument not String list",val);
			}
			list = co.cdr;
		    } else {
			return _error("argument not String list",val);
		    }
		} while (list != List.nil);
		return new de.uni_sb.ps.dml.runtime.String(buff.toString());
	    } else {
		return _error("argument not String list",val);
	    }
	}
    }
    /** <code>val concat : string list -> string </code>*/
    _FIELD(String,concat);

    _BUILTIN(Append) {
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
    _FIELD(String,append);

    _BUILTIN(IsPrefix) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"String.isPrefix");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.String)) {
		return _error("argument 1 not String",val);
	    }
	    java.lang.String s = ((de.uni_sb.ps.dml.runtime.String) v).getString();
	    v = args[1].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.String)) {
		return _error("argument 2 not String",val);
	    }
	    java.lang.String t = ((de.uni_sb.ps.dml.runtime.String) v).getString();
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
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.String)) {
		return _error("argument 1 not String",val);
	    }
	    java.lang.String s = ((de.uni_sb.ps.dml.runtime.String) v).getString();
	    v = args[1].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.String)) {
		return _error("argument 2 not String",val);
	    }
	    java.lang.String t = ((de.uni_sb.ps.dml.runtime.String) v).getString();
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

    _BUILTIN(Less) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"String.<");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.String)) {
		return _error("argument 1 not String",val);
	    }
	    java.lang.String s = ((de.uni_sb.ps.dml.runtime.String) v).getString();
	    v = args[1].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.String)) {
		return _error("argument 2 not String",val);
	    }
	    java.lang.String t = ((de.uni_sb.ps.dml.runtime.String) v).getString();
	    if (s.compareTo(t)<0) {
		return Constants.dmltrue;
	    } else {
		return Constants.dmlfalse;
	    }
	}
    }
    /** <code>val < : (string * string) -> bool </code>*/
    _FIELD(String,less);

    _BUILTIN(Leq) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"String.<=");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.String)) {
		return _error("argument 1 not String",val);
	    }
	    java.lang.String s = ((de.uni_sb.ps.dml.runtime.String) v).getString();
	    v = args[1].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.String)) {
		return _error("argument 2 not String",val);
	    }
	    java.lang.String t = ((de.uni_sb.ps.dml.runtime.String) v).getString();
	    if (s.compareTo(t)<=0) {
		return Constants.dmltrue;
	    } else {
		return Constants.dmlfalse;
	    }
	}
    }
    /** <code>val <= : (string * string) -> bool </code>*/
    _FIELD(String,leq);

    _BUILTIN(Greater) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"String.>");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.String)) {
		return _error("argument 1 not String",val);
	    }
	    java.lang.String s = ((de.uni_sb.ps.dml.runtime.String) v).getString();
	    v = args[1].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.String)) {
		return _error("argument 2 not String",val);
	    }
	    java.lang.String t = ((de.uni_sb.ps.dml.runtime.String) v).getString();
	    if (s.compareTo(t)>0) {
		return Constants.dmltrue;
	    } else {
		return Constants.dmlfalse;
	    }
	}
    }
    /** <code>val > : (string * string) -> bool </code>*/
    _FIELD(String,Greater);

    _BUILTIN(Geq) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"String.>=");
	    DMLValue v = args[0].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.String)) {
		return _error("argument 1 not String",val);
	    }
	    java.lang.String s = ((de.uni_sb.ps.dml.runtime.String) v).getString();
	    v = args[1].request();
	    if (!(v instanceof de.uni_sb.ps.dml.runtime.String)) {
		return _error("argument 2 not String",val);
	    }
	    java.lang.String t = ((de.uni_sb.ps.dml.runtime.String) v).getString();
	    if (s.compareTo(t)>=0) {
		return Constants.dmltrue;
	    } else {
		return Constants.dmlfalse;
	    }
	}
    }
    /** <code>val >= : (string * string) -> bool </code>*/
    _FIELD(String,geq);

    /** <code>structure Char : CHAR </code>*/
    /** <code>val maxSize : int </code>*/
    /** <code>val fromString : java.lang.String.string -> string option </code>*/
    /** <code>val toString : string -> java.lang.String.string </code>*/
    /** <code>val fromCString : java.lang.String.string -> string option </code>*/
    /** <code>val toCString : string -> java.lang.String.string </code>*/
    /** <code>val sub : (string * int) -> Char.char </code>*/
    /** <code>val str : Char.char -> string </code>*/
    /** <code>val implode : Char.char list -> string </code>*/
    /** <code>val explode : string -> Char.char list </code>*/
    /** <code>val map : (Char.char -> Char.char) -> string -> string </code>*/
    /** <code>val translate : (Char.char -> string) -> string -> string </code>*/
    /** <code>val tokens : (Char.char -> bool) -> string -> string list </code>*/
    /** <code>val fields : (Char.char -> bool) -> string -> string list </code>*/
    /** <code>val collate : ((Char.char * Char.char) -> order) -> (string * string) -> order </code>*/
}
