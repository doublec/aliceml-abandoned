dnl
dnl Author:
dnl      Daniel Simon, <dansim@ps.uni-sb.de>
dnl
dnl Copyright:
dnl      Daniel Simon, 1999
dnl
dnl Last change:
dnl    $Date$ by $Author$
dnl $Revision$
dnl
dnl
dnl Text-Tools
define(capitalize,`translit(substr($1,0,1),a-z,A-Z)`'substr($1,1)')
define(STRING,`de.uni_sb.ps.dml.runtime.String')
dnl
dnl _error
dnl
define(_error,`
	// MACRO: ERROR($1,$2)
	_RAISE(runtimeError,new Tuple2(new STRING`'($1),$2))
	// END MACRO: ERROR($1,$2)
')
define(_RAISE,`ExceptionWrapper E = new ExceptionWrapper(Constants.$1.apply($2));
	       E.printStackTrace();
	       throw E')
define(_RAISENAME,`ExceptionWrapper E = new ExceptionWrapper($1);
		   E.printStackTrace();
		   throw E')
dnl
dnl Hier machen wir die Macros rein, die quasi überall gebraucht werden
dnl
define(_apply_fails,`    final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException {
	_RAISE(runtimeError,new STRING`'("cannot apply "+this+" to "+val));
    }
final public DMLValue apply0() throws java.rmi.RemoteException {
	_RAISE(runtimeError,new STRING`'("cannot apply "+this+" to nothing"));
    }
final public DMLValue apply2(DMLValue v1, DMLValue v2) throws java.rmi.RemoteException {
	_RAISE(runtimeError,new STRING`'("cannot apply "+this+" to ("+v1+","+v2+")"));
    }
final public DMLValue apply3(DMLValue v1, DMLValue v2, DMLValue v3) throws java.rmi.RemoteException {
	_RAISE(runtimeError,new STRING`'("cannot apply "+this+" to ("+v1+","+v2+","+v3+")"));
    }
final public DMLValue apply4(DMLValue v1, DMLValue v2, DMLValue v3, DMLValue v4) throws java.rmi.RemoteException {
	_RAISE(runtimeError,new STRING`'("cannot apply "+this+" to ("+v1+","+v2+","+v3+","+v4+")"));
    }
')
dnl
dnl für ConValTuple<i>
dnl
define(_getConstructor,`    final public Constructor getConstructor() {
	return constructor;
    }
')
dnl
dnl fromTuple für Builtins
dnl _fromTuple(DMLArray[] wohin, DMLValue woher, int wieviele, String where)
define(_fromTuple,`
	_REQUEST($2,$2);
	DMLTuple t = null;
	try {
	t = (DMLTuple) $2;
	} catch (ClassCastException c) {
	    _RAISENAME(General.Match);
	}
	if (t.getArity()!=$3) {
	    _RAISENAME(General.Match);
	}
	ifelse($3,`2',`DMLValue _1 = t.get(0);
	DMLValue _2 = t.get(1);
	return apply2(_1,_2)',
	ifelse($3,`3',`DMLValue _1 = t.get(0);
	DMLValue _2 = t.get(1);
	DMLValue _3 = t.get(2);
	return apply3(_1,_2,_3)',
	ifelse($3,`4',`DMLValue _1 = t.get(0);
	DMLValue _2 = t.get(1);
	DMLValue _3 = t.get(2);
	DMLValue _4 = t.get(3);
	return apply4(_1,_2,_3,_4)',`
	DMLValue[] $1 = null;
	if ($2 instanceof DMLTuple) {
	  t = (DMLTuple) $2;
	  if (t.getArity()==$3) {
	    $1 = new DMLValue[$3];
	    for(int i=0; i<$3; i++)
		$1[i]=t.get(i);
	  } else {
	    _RAISENAME(General.Match);
	  }
	} else {
	    _RAISENAME(General.Match);
	}')))
')
dnl
dnl General:
dnl
define(_BINOP,`
    _BUILTIN(capitalize($1)) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _fromTuple(args,val,2,"General.$2");
	}
	_SAPPLY2(v) {
	    _REQUESTDEC(DMLValue number1,v1);
	    if (number1 instanceof Int) {
		_REQUESTDEC(DMLValue number2,v2);
		if (number2 instanceof Int) {
		    return new Int(((Int) number1).value $2 ((Int) number2).value);
		} else {
		    _RAISENAME(General.Match);
		}
	    } else if (number1 instanceof Word) {
		_REQUESTDEC(DMLValue number2,v2);
		if (number2 instanceof Word) {
		    long ago = ((Word) number1).value;
		    long day = ((Word) number2).value;
		    long way = (ago $2 day) & 0x7fffffff;
		    return new Word((int) way);
		} else {
		    _RAISENAME(General.Match);
		}
	    } else if (number1 instanceof Real) {
		_REQUESTDEC(DMLValue number2,v2);
		if (number2 instanceof Real) {
		    return new Real(((Real) number1).value $2 ((Real) number2).value);
		} else {
		    _RAISENAME(General.Match);
		}
	    } else {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** val $2 : (number * number) -> number */
    _FIELD(General,$1);')
define(_COMPARE,`
    _BUILTIN(capitalize($1)) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _fromTuple(args,val,2,"General.$2");
	}
	_SAPPLY2(v) {
	    _REQUESTDEC(DMLValue number1,v1);
	    if (number1 instanceof Int) {
		_REQUESTDEC(DMLValue number2,v2);
		if (number2 instanceof Int) {
		    return (((Int) number1).value $2 ((Int) number2).value ?
			    Constants.dmltrue :
			    Constants.dmlfalse);
		} else {
		    _RAISENAME(General.Match);
		}
	    } else if (number1 instanceof Word) {
		_REQUESTDEC(DMLValue number2,v2);
		if (number2 instanceof Word) {
		    return (((Word) number1).value $2 ((Word) number2).value ?
			    Constants.dmltrue :
			    Constants.dmlfalse);
		} else {
		    _RAISENAME(General.Match);
		}
	    } else if (number1 instanceof Real) {
		_REQUESTDEC(DMLValue number2,v2);
		if (number2 instanceof Real) {
		    return (((Real) number1).value $2 ((Real) number2).value ?
			    Constants.dmltrue :
			    Constants.dmlfalse);
		} else {
		    _RAISENAME(General.Match);
		}
	    } else if (number1 instanceof STRING`') {
		_REQUESTDEC(DMLValue number2,v2);
		if (number2 instanceof STRING) {
		    return (((STRING) number1).value.
			    compareTo(((STRING) number2).value) $2 0  ?
			    Constants.dmltrue :
			    Constants.dmlfalse);
		} else {
		    _RAISENAME(General.Match);
		}
	    } else {
		_RAISENAME(General.Match);
	    }
	}
    }
    /** val $2 : (number|string * number|string) -> bool */
    _FIELD(General,$1);
    static {
	Builtin.builtins.put("$2",$1);
    }')
dnl
dnl Int
dnl
define(_BINOPINT,`
	_BUILTIN(capitalize($1)) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _fromTuple(args,val,2,"Int.$2");
	}
	_SAPPLY2(v) {
	    try {
	    _REQUESTDEC(DMLValue v,v1);
	    _REQUESTDEC(DMLValue w,v2);
	    return new Int(((Int) v).value $2 ((Int) w).value);
	    } catch (ClassCastException c) {
	    _RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val $2 : (int * int) -> int </code>*/
    final public static DMLValue $1 = new capitalize($1)();
    static {
	Builtin.builtins.put("Int.$2",$1);
    }')
define(_COMPAREINT,`
    _BUILTIN(capitalize($1)) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _fromTuple(args,val,2,"Int.$2");
	}
	_SAPPLY2(v) {
	    try {
	    _REQUESTDEC(DMLValue v,v1);
	    _REQUESTDEC(DMLValue w,v2);
	    int i = ((Int) v).value;
	    int j = ((Int) w).value;
	    if (i $2 j) {
		return Constants.dmltrue;
	    } else {
		return Constants.dmlfalse;
	    }
	    } catch (ClassCastException c) {
	    _RAISENAME(General.Match);
	    }
	}
    }
    /** <code>val $2 : (int * int) -> bool </code>*/
    final public static DMLValue $1 = new capitalize($1)();
    static {
	Builtin.builtins.put("Int.$2",$1);
    }')
dnl
dnl String
dnl
define(_COMPARESTRING,`
    _BUILTIN(capitalize($1)) {
	_NOAPPLY0;_APPLY2;_NOAPPLY3;_NOAPPLY4;
	_APPLY(val) {
	    _fromTuple(args,val,2,"String.$2");
	}
	_SAPPLY2(v) { 
	    _REQUESTDEC(DMLValue v,v1);
	    if (!(v instanceof STRING)) {
		_RAISENAME(General.Match);
	    }
	    java.lang.String s = ((STRING) v).value;
	    _REQUEST(v,v2);
	    if (!(v instanceof STRING)) {
		_RAISENAME(General.Match);
	    }
	    java.lang.String t = ((STRING) v).value;
	    if (s.compareTo(t) $2 0) {
		return Constants.dmltrue;
	    } else {
		return Constants.dmlfalse;
	    }
	}
    }
    /** <code>val $2 : (string * string) -> bool </code>*/
    _FIELD(String,$1);')
dnl
dnl _BUILTIN(name)
dnl
define(_BUILTIN,`
	// Builtin $1
	final public static class $1 extends Builtin')
dnl _APPLY(val)
define(_NOAPPLY0,`final public DMLValue apply0() throws java.rmi.RemoteException {
	_RAISENAME(General.Match);
	}')
define(_NOAPPLY2,`final public DMLValue apply2(DMLValue v1, DMLValue v2) throws java.rmi.RemoteException {
	_RAISENAME(General.Match);
	}')
define(_NOAPPLY3,`final public DMLValue apply3(DMLValue v1, DMLValue v2, DMLValue v3) throws java.rmi.RemoteException {
	_RAISENAME(General.Match);
	}')
define(_NOAPPLY4,`final public DMLValue apply4(DMLValue v1, DMLValue v2, DMLValue v3, DMLValue v4) throws java.rmi.RemoteException {
	_RAISENAME(General.Match);
	}')
define(_APPLY0,`final public DMLValue apply0() throws java.rmi.RemoteException {
	return sapply0();
	}')
define(_APPLY2,`final public DMLValue apply2(DMLValue v1, DMLValue v2) throws java.rmi.RemoteException {
	return sapply2(v1,v2);
	}')
define(_APPLY3,`final public DMLValue apply3(DMLValue v1, DMLValue v2, DMLValue v3) throws java.rmi.RemoteException {
	return sapply3(v1,v2,v3);
	}')
define(_APPLY4,`final public DMLValue apply4(DMLValue v1, DMLValue v2, DMLValue v3, DMLValue v4) throws java.rmi.RemoteException {
	return sapply3(v1,v2,v3,v4);
	}')
define(_SAPPLY0,`final public DMLValue sapply0() throws java.rmi.RemoteException')
define(_SAPPLY2,`final public static DMLValue sapply2(DMLValue $1`'1, DMLValue $1`'2) throws java.rmi.RemoteException')
define(_SAPPLY3,`final public static DMLValue sapply3(DMLValue $1`'1, DMLValue $1`'2, DMLValue $1`'3) throws java.rmi.RemoteException')
define(_SAPPLY4,`final public static DMLValue sapply3(DMLValue $1`'1, DMLValue $1`'2, DMLValue $1`'3, DMLValue $1`'4) throws java.rmi.RemoteException')
define(_VAPPLY0,`final public DMLValue apply0() throws java.rmi.RemoteException')
define(_VAPPLY2,`final public DMLValue apply2(DMLValue $1`'1, DMLValue $1`'2) throws java.rmi.RemoteException')
define(_VAPPLY3,`final public DMLValue apply3(DMLValue $1`'1, DMLValue $1`'2, DMLValue $1`'3) throws java.rmi.RemoteException')
define(_VAPPLY4,`final public DMLValue apply3(DMLValue $1`'1, DMLValue $1`'2, DMLValue $1`'3, DMLValue $1`'4) throws java.rmi.RemoteException')
define(_APPLY,`
	// apply
	final public DMLValue apply(DMLValue $1) throws java.rmi.RemoteException' )
dnl FIELD(class,fieldname)
define(_FIELD,`final public static DMLValue $2 = new capitalize($2)();
	static {
	  Builtin.builtins.put("$1.$2",$2);
	}')
define(_REQUEST,`ifelse($1,$2,`if ($2 instanceof DMLLVar) {
	$1 = ((DMLLVar) $2).request(); }',`
	if ($2 instanceof DMLLVar) {
		    $1 = ((DMLLVar) $2).request();
		 } else {
		    $1 = $2;
		 }')')
define(_REQUESTDEC,`$1 = null;
		 if ($2 instanceof DMLLVar) {
		    patsubst($1,`[a-zA-z]+ \([a-z]+\)',`\1') = ((DMLLVar) $2).request();
		 } else {
		    patsubst($1,`[a-zA-z]+ \([a-z]+\)',`\1') = $2;
		 }')
define(_fromSingle,`')
define(_BUILTTUP,`
	final public DMLValue apply0() throws java.rmi.RemoteException { return apply(Constants.dmlunit); }
	final public DMLValue apply2(DMLValue v1, DMLValue v2) throws java.rmi.RemoteException {
	return apply(new Tuple2(v1,v2));
	}
	final public DMLValue apply3(DMLValue v1, DMLValue v2, DMLValue v3) throws java.rmi.RemoteException {
	return apply(new Tuple3(v1,v2,v3));
	}
	final public DMLValue apply4(DMLValue v1, DMLValue v2, DMLValue v3, DMLValue v4) throws java.rmi.RemoteException {
	return apply(new Tuple4(v1,v2,v3,v4));
	}
')
