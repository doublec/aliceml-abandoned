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
define(_RAISE,`throw new ExceptionWrapper(Constants.$1.apply($2))')
define(_RAISENAME,`throw new ExceptionWrapper($1)')
dnl
dnl Hier machen wir die Macros rein, die quasi überall gebraucht werden
dnl
define(_request_id,`    final public DMLValue request() {
	return this;
    }
')
define(_getValue_id,`    final public DMLValue getValue() {
	return this;
    }
')
define(_raise,`')
define(_apply_fails,`    final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException {
	_RAISE(runtimeError,new STRING`'("cannot apply "+this+" to "+val));
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
	$2=$2.request();
	DMLValue[] $1 = null;
	if ($2 instanceof DMLTuple) {
	  DMLTuple t = (DMLTuple) $2;
	  if (t.getArity()==$3) {
	    $1 = new DMLValue[$3];
	    for(int i=0; i<$3; i++)
		$1[i]=t.getByIndex(i);
	  } else {
	    return Constants.
		runtimeError.apply(new Tuple2(
		new STRING`'("wrong number of arguments for" + $4),$2));
	  }
	} else {
	    return Constants.
		runtimeError.apply(new Tuple2(
		new STRING`'("wrong arguments type for" + $4),$2));
	}
')
dnl
dnl General:
dnl
define(_BINOP,`
    _BUILTIN(capitalize($1)) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"General.$2");
	    DMLValue number1 = args[0].request();
	    if (number1 instanceof Int) {
		DMLValue number2 = args[1].request();
		if (number2 instanceof Int) {
		    return new Int(((Int) number1).getInt() $2 ((Int) number2).getInt());
		} else {
		    _error("arguments of different number type",val);
		}
	    } else if (number1 instanceof Word) {
		DMLValue number2 = args[1].request();
		if (number2 instanceof Word) {
		    return new Word(((Word) number1).getLong() $2 ((Word) number2).getLong());
		} else {
		    _error("arguments of different number type",val);
		}
	    } else if (number1 instanceof Real) {
		DMLValue number2 = args[1].request();
		if (number2 instanceof Real) {
		    return new Real(((Real) number1).getFloat() $2 ((Real) number2).getFloat());
		} else {
		    _error("arguments of different number type",val);
		}
	    } else {
		_error("argument 1 not number type",val);
	    }
	}
    }
    /** val $2 : (number * number) -> number */
    _FIELD(General,$1);') 
define(_COMPARE,`
    _BUILTIN(capitalize($1)) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"General.$2");
	    DMLValue number1 = args[0].request();
	    if (number1 instanceof Int) {
		DMLValue number2 = args[1].request();
		if (number2 instanceof Int) {
		    return (((Int) number1).getInt() $2 ((Int) number2).getInt() ?
			    Constants.dmltrue :
			    Constants.dmlfalse);
		} else {
		    _error("arguments of different number type",val);
		}
	    } else if (number1 instanceof Word) {
		DMLValue number2 = args[1].request();
		if (number2 instanceof Word) {
		    return (((Word) number1).getLong() $2 ((Word) number2).getLong() ?
			    Constants.dmltrue :
			    Constants.dmlfalse);
		} else {
		    _error("arguments of different number type",val);
		}
	    } else if (number1 instanceof Real) {
		DMLValue number2 = args[1].request();
		if (number2 instanceof Real) {
		    return (((Real) number1).getFloat() $2 ((Real) number2).getFloat() ?
			    Constants.dmltrue :
			    Constants.dmlfalse);
		} else {
		    _error("arguments of different number type",val);
		}
	    } else if (number1 instanceof STRING`') {
		DMLValue number2 = args[1].request();
		if (number2 instanceof STRING) {
		    return (((STRING) number1).getString().
			    compareTo(((STRING) number2).getString()) $2 0  ?
			    Constants.dmltrue :
			    Constants.dmlfalse);
		} else {
		    _error("arguments of different type",val);
		}
	    } else {
		_error("argument 1 not string or number type",val);
	    }
	}
    }
    /** val $2 : (number|string * number|string) -> bool */
    _FIELD(General,$1);')
dnl
dnl Int
dnl
define(_BINOPINT,`
	_BUILTIN(capitalize($1)) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Int.$2");
	    DMLValue v = args[0].request();
	    if (!(v instanceof Int)) {
		_error("argument 1 not Int",val);
	    }
	    DMLValue w = args[1].request();
	    if (!(w instanceof Int)) {
		_error("argument 2 not Int",val);
	    }
	    return new Int(((Int) v).getInt() $2 ((Int) w).getInt());
	}
    }
    /** <code>val $2 : (int * int) -> int </code>*/
    _FIELD(Int,$1);')
define(_COMPAREINT,`
    _BUILTIN(capitalize($1)) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"Int.$2");
	    DMLValue v = args[0].request();
	    if (!(v instanceof Int)) {
		_error("argument 1 not Int",val);
	    }
	    DMLValue w = args[1].request();
	    if (!(w instanceof Int)) {
		_error("argument 2 not Int",val);
	    }
	    int i = ((Int) v).getInt();
	    int j = ((Int) w).getInt();
	    if (i $2 j) {
		return Constants.dmltrue;
	    } else {
		return Constants.dmlfalse;
	    }
	}
    }
    /** <code>val $2 : (int * int) -> bool </code>*/
    _FIELD(Int,$1);')
dnl
dnl String
dnl
define(_COMPARESTRING,`
    _BUILTIN(capitalize($1)) {
	_APPLY(val) {
	    _fromTuple(args,val,2,"String.$2");
	    DMLValue v = args[0].request();
	    if (!(v instanceof STRING)) {
		_error("argument 1 not String",val);
	    }
	    java.lang.String s = ((STRING) v).getString();
	    v = args[1].request();
	    if (!(v instanceof STRING)) {
		_error("argument 2 not String",val);
	    }
	    java.lang.String t = ((STRING) v).getString();
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
define(_APPLY,`
	// apply
	final public DMLValue apply(DMLValue $1) throws java.rmi.RemoteException' )
dnl FIELD(class,fieldname)
define(_FIELD,`final public static DMLValue $2 = new capitalize($2)();
	static {
	  Builtin.builtins.put("$1.$2",$2);
	}')
define(_REQUEST,`$1 = $2.request()')
