package de.uni_sb.ps.DML.DMLRuntime;
/**
   DMLValue
   Darstellung von Werten in DML, Mutter aller Klassen
*/

abstract public class DMLValue extends RuntimeException implements java.io.Serializable {

    /** gibt Wert \in Wert zurueck */
    public DMLValue getValue() {
	return this;
    }

    /** gibt Wert \in Wert \minus LVar zurueck */
    public DMLValue request() {
	return this;
    }

    abstract public DMLValue apply(DMLValue val);

    abstract public String toString();
}
