dnl Text-Tools
define(capitalize,`translit(substr($1,0,1),a-z,A-Z)`'substr($1,1)')
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
define(_raise,`    final public DMLValue raise() {
	throw new ExceptionWrapper(this);
    }
')
define(_apply_fails,`    final public DMLValue apply(DMLValue val) throws java.rmi.RemoteException {
	return Constants.runtimeError.apply(new de.uni_sb.ps.dml.runtime.String("cannot apply "+this+" to "+val)).raise();
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
	// MACRO: FROMTUPLE($1,$2,$3,$4)
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
		new de.uni_sb.ps.dml.runtime.String("wrong number of arguments for" + $4),$2));
	  }
	} else {
	    return Constants.
		runtimeError.apply(new Tuple2(
		new de.uni_sb.ps.dml.runtime.String("wrong arguments type for" + $4),$2));
	}
	// END MACRO: FROMTUPLE($1,$2,$3,$4)
')
dnl
dnl _error
dnl
define(_error,`
	// MACRO: ERROR($1,$2)
	Constants.runtimeError.apply(new Tuple2(new de.uni_sb.ps.dml.runtime.String($1),$2)).raise()
	// END MACRO: ERROR($1,$2)
')
dnl
dnl _BUILTIN(name)
define(_BUILTIN,`
	// Builtin $1
	final public static class $1 extends Builtin')
dnl _APPLY(val)
define(_APPLY,`
	// apply
	final public DMLValue apply(DMLValue $1) throws java.rmi.RemoteException' )
dnl FIELD(fieldname)
define(_FIELD,`final public static DMLValue $1 = new capitalize($1)()')
