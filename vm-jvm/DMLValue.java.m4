package de.uni_sb.ps.DML.DMLRuntime;
/**
   DMLValue
   Darstellung von Werten in DML, Mutter aller Klassen
*/

public interface DMLValue extends java.io.Serializable {

    /** gibt Wert \in Wert zurueck */
    public DMLValue getValue();

    /** gibt Wert \in Wert \minus LVar zurueck */
    public DMLValue request();

    public DMLValue apply(DMLValue val);

    public String toString();

    public DMLValue raise();
}
