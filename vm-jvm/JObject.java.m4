package de.uni_sb.ps.dml.runtime;

final public class JObject implements DMLValue {

    java.lang.Object javaObject = null;

    public JObject(java.lang.Object o) {
	super();
	javaObject=o;
    }

    final public java.lang.Object getObject() {
	return javaObject;
    }

    final public java.lang.String toString() {
	return javaObject.getClass().getName()+": "+javaObject+" : APIObject";
    }

    _apply_fails ;
    _request_id ;
    _getValue_id ;
    _raise ;
}
