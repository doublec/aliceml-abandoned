dnl Hier machen wir die Macros rein, die quasi überall gebraucht werden

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

dnl für ConValTuple<i>
define(_getConstructor,`    final public Constructor getConstructor() {
	return constructor;
    }
')

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

dnl _error
dnl
define(_error,`
	// MACRO: ERROR($1,$2)
	Constants.runtimeError.apply(new Tuple2(new de.uni_sb.ps.dml.runtime.String($1),$2)).raise()
	// END MACRO: ERROR($1,$2)
')
