package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLExceptionWrapper extends RuntimeException {

    DMLValue value = null;

    public DMLExceptionWrapper(DMLValue v){
	super();
	value=v;
    }

    public DMLValue getValue() {
	return value;
    }
}
