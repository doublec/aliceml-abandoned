package de.uni_sb.ps.dml.runtime;
/** Darstellung von Werten in DML. Klassen, die Werte in DML repräsentieren,
    müssen dieses Interface implementieren.
*/

public interface DMLValue extends java.io.Serializable {

    /** gibt Wert \in Wert zurueck */
    public DMLValue getValue() throws java.rmi.RemoteException;

    /** gibt Wert \in Wert \minus LVar zurueck */
    public DMLValue request() throws java.rmi.RemoteException;

    /** Funktionale Werte und Konstruktoren können mit dieser Methode
     *  appliziert werden. Andere Werte erzeugen Laufzeitfehler.
     *  @param val der formale Parameter der Funktion/des Konstruktors
     *  @return DMLValue das Ergebnis der Applikation
     */
    public DMLValue apply(DMLValue val) throws java.rmi.RemoteException;

    /** In DML kann jeder Wert als Exception geworfen werden.
     */
    public DMLValue raise() throws java.rmi.RemoteException;
}
