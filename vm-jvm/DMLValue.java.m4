/*
 * Author: 
 *      Daniel Simon, <dansim@ps.uni-sb.de>
 * 
 * Copyright:
 *      Daniel Simon, 1999
 *
 * Last change:
 *    $Date$ by $Author$
 * $Revision$
 * 
 */
package de.uni_sb.ps.dml.runtime;

/** Darstellung von Werten in DML. Klassen, die Werte in DML repräsentieren,
 *  müssen dieses Interface implementieren.
*/
public interface DMLValue extends java.io.Serializable {
    /** Funktionale Werte und Konstruktoren können mit dieser Methode
     *  appliziert werden. Andere Werte erzeugen Laufzeitfehler.
     *  @param val der formale Parameter der Funktion/des Konstruktors
     *  @return DMLValue das Ergebnis der Applikation
     */
    public DMLValue apply(DMLValue val) throws java.rmi.RemoteException;
    public DMLValue apply0() throws java.rmi.RemoteException;
    public DMLValue apply2(DMLValue v1, DMLValue v2)
	throws java.rmi.RemoteException;
    public DMLValue apply3(DMLValue v1, DMLValue v2, DMLValue v3)
	throws java.rmi.RemoteException;
    public DMLValue apply4(DMLValue v1, DMLValue v2, DMLValue v3, DMLValue v4)
	throws java.rmi.RemoteException;
    public boolean matchInt(int i)
	throws java.rmi.RemoteException;
    public boolean matchWord(int i)
	throws java.rmi.RemoteException;
    public boolean matchReal(float f)
	throws java.rmi.RemoteException;
    public boolean matchChar(char c)
	throws java.rmi.RemoteException;
    public boolean matchString(java.lang.String s)
	throws java.rmi.RemoteException;
}
