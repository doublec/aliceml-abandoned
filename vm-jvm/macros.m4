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
