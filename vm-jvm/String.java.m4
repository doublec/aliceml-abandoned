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

    _apply_fails;

    _BUILTIN(Size) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"String.length");
	    if (val instanceof STRING) {
		return new Int(((STRING) val).value.length());
	    } else {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val size : string -> int </code>*/
    _FIELD(String,size);

    _BUILTIN(Extract) {
	_NOAPPLY0;_NOAPPLY2;_APPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _fromTuple(args,val,3,"String.extract");
	}
	_SAPPLY3(v) {
	    try {
	    _REQUESTDEC(DMLValue v,v1);
	    java.lang.String s = ((STRING) v).value;
	    _REQUEST(v,v2);
	    int from = ((Int) v).value;
	    _REQUEST(v,v3);
	    int to = s.length();
	    if (v instanceof DMLConVal) {
		DMLConVal cv = (DMLConVal) v;
		if (cv.getConstructor() == Option.SOME) {
		    v = cv.getContent();
		    if (v instanceof Int) {
			to = ((Int) v).value;
			return new STRING (s.substring(from,to));
		    } else {
			_RAISENAME(General.Match);
		    }
		} else {
		    _RAISENAME(General.Match);
		}
	    } else if (v != Option.NONE) {
		_RAISENAME(General.Match);
	    } else {
		return new STRING (s.substring(from,to));
	    }
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val extract : (string * int * int option) -> string </code>*/
    _FIELD(String,extract);

    _BUILTIN(Substring) {
	_NOAPPLY0;_NOAPPLY2;_APPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _fromTuple(args,val,3,"String.substring");
	}
	_SAPPLY3(v) {
	    try {
	    _REQUESTDEC(DMLValue v,v1);
	    java.lang.String s = ((STRING) v).value;
	    _REQUEST(v,v2);
	    int from = ((Int) v).value;
	    _REQUEST(v,v3);
	    int to = ((Int) v).value;
	    return new STRING (s.substring(from,to));
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val substring : (string * int * int) -> string </code>*/
    _FIELD(String,substring);

    _BUILTIN(Concat) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    //	    _FROMTUPLE(args,val,2,"String.concat");
	    try {
		_REQUESTDEC(DMLValue list,val);
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
				_RAISENAME(General.Match);
			    }
			    list = co.cdr;
			} else {
			    _RAISENAME(General.Match);
			}
		    } while (list != List.nil);
		    return new STRING (buff.toString());
		} else {
		    _RAISENAME(General.Match);
		}
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val concat : string list -> string </code>*/
    _FIELD(String,concat);

    _BUILTIN(Append) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _fromTuple(args,val,2,"String.^");
	}
	_SAPPLY2(v) {
	    try {
		_REQUESTDEC(DMLValue v,v1);
		_REQUESTDEC(DMLValue w,v2);
		return new STRING (((STRING) v).value + ((STRING) w).value);
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val ^ : (string * string) -> string </code>*/
    _FIELD(String,append);
    static {
	Builtin.builtins.put("String.^",append);
    }

    _BUILTIN(IsPrefix) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _fromTuple(args,val,2,"String.isPrefix");
	}
	_SAPPLY2(v) {
	    try {
		_REQUESTDEC(DMLValue v,v1);
		java.lang.String s = ((STRING) v).value;
		_REQUEST(v,v2);
		java.lang.String t = ((STRING) v).value;
		if (s.startsWith(t)) {
		    return Constants.dmltrue;
		} else {
		    return Constants.dmlfalse;
		}
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val isPrefix : string -> string -> bool </code>*/
    _FIELD(String,isPrefix);

    _BUILTIN(Compare) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _fromTuple(args,val,2,"String.compare");
	}
	_SAPPLY2(v) {
	    try {
		_REQUESTDEC(DMLValue v,v1);
		java.lang.String s = ((STRING) v).value;
		_REQUEST(v,v2);
		java.lang.String t = ((STRING) v).value;
		int cmp = s.compareTo(t);
		if (cmp < 0) {
		    return General.LESS;
		} else if (cmp==0) {
		    return General.EQUAL;
		} else {
		    return General.GREATER;
		}
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val compare : (string * string) -> order </code>*/
    _FIELD(String,compare);

    _BUILTIN(Compare_) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _fromTuple(args,val,2,"String.compare'");
	}
	_SAPPLY2(v) {
	    try {
	    _REQUESTDEC(DMLValue v,v1);
	    java.lang.String s = ((STRING) v).value;
	    _REQUEST(v,v2);
	    java.lang.String t = ((STRING) v).value;
	    int cmp = s.compareTo(t);
	    if (cmp < 0) {
		return Int.MONE;
	    } else if (cmp==0) {
		return Int.ZERO;
	    } else {
		return Int.ONE;
	    }
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
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
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    // _FROMSINGLE(val,"String.str");
	    if (val instanceof Char) {
		return new STRING (java.lang.String.valueOf(((Char) val).value));
	    } else {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val str : Char.char -> string </code>*/
    _FIELD(String,str);

    _BUILTIN(Sub) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _fromTuple(args,val,2,"String.sub");
	}
	_SAPPLY2(v) {
	    try {
	    _REQUESTDEC(DMLValue s,v1);
	    _REQUESTDEC(DMLValue idx,v2);
	    return new Char(((STRING) s).value.charAt(((Int) idx).value));
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
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

    _BUILTIN(ToCString) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    java.lang.String result = null;
	    try {
		 result = ((STRING) val).value;
		int idx=0;
		while ((idx=result.indexOf('\\',idx)) > 0) {
		    result = result.substring(0,idx-1)+"\\\\"+
			result.substring(idx+1);
		}
		idx = 0;
		while ((idx=result.indexOf('\"',idx)) > 0) {
		    result = result.substring(0,idx-1)+"\\\""+
			result.substring(idx+1);
		}
		idx = 0;
		while ((idx=result.indexOf('?',idx)) > 0) {
		    result = result.substring(0,idx-1)+"\\?"+
			result.substring(idx+1);
		}
		idx = 0;
		while ((idx=result.indexOf('?',idx)) > 0) {
		    result = result.substring(0,idx-1)+"\\?"+
			result.substring(idx+1);
		}
		idx = 0;
		while ((idx=result.indexOf('?',idx)) > 0) {
		    result = result.substring(0,idx-1)+"\\?"+
			result.substring(idx+1);
		}
		idx = 0;
		while ((idx=result.indexOf(0x7,idx)) > 0) {
		    result = result.substring(0,idx-1)+"\\"+java.lang.String.valueOf(0x7)+
			result.substring(idx+1);
		}
		idx = 0;
		while ((idx=result.indexOf('\b',idx)) > 0) {
		    result = result.substring(0,idx-1)+"\\\b"+
			result.substring(idx+1);
		}
		idx = 0;
		while ((idx=result.indexOf('\n',idx)) > 0) {
		    result = result.substring(0,idx-1)+"\\\n"+
			result.substring(idx+1);
		}
		idx = 0;
		while ((idx=result.indexOf(0xb,idx)) > 0) {
		    result = result.substring(0,idx-1)+"\\"+java.lang.String.valueOf(0xb)+
			result.substring(idx+1);
		}
		idx = 0;
		while ((idx=result.indexOf('\f',idx)) > 0) {
		    result = result.substring(0,idx-1)+"\\\f"+
			result.substring(idx+1);
		}
		idx = 0;
		while ((idx=result.indexOf('\t',idx)) > 0) {
		    result = result.substring(0,idx-1)+"\\\t"+
			result.substring(idx+1);
		}
		idx = 0;
		while ((idx=result.indexOf('\r',idx)) > 0) {
		    result = result.substring(0,idx-1)+"\\\r"+
			result.substring(idx+1);
		}
	    } catch (ClassCastException c) {
		_RAISENAME(General.Match);
	    }
	    return new STRING (result);
	}
    }
    /** <code>val toCString : string -> java.lang.String.string </code>*/
    _FIELD(String,toCString);

    /** <code>val implode : Char.char list -> string </code>*/
    _BUILTIN(Explode) {
	_NOAPPLY0;_NOAPPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    if (val instanceof STRING) {
		char cl[] = ((STRING) val).value.toCharArray();
		Cons first = new Cons(new Char(cl[0]),null);
		Cons next = first;
		for (int i=1; i<cl.length; i++) {
		    next.cdr = new Cons(new Char(cl[i]),null);
		    next = (Cons) next.cdr;
		}
		next.cdr = List.nil;
		return first;
	    } else {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val explode : string -> Char.char list </code>*/
    _FIELD(String,explode);
    /** <code>val map : (Char.char -> Char.char) -> string -> string </code>*/
    /** <code>val translate : (Char.char -> string) -> string -> string </code>*/
    /** <code>val tokens : (Char.char -> bool) -> string -> string list </code>*/
    /** <code>val fields : (Char.char -> bool) -> string -> string list </code>*/
    /** <code>val collate : ((Char.char * Char.char) -> order) -> (string * string) -> order </code>*/
}
