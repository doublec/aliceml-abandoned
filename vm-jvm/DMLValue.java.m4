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

import java.io.Serializable;
import java.rmi.RemoteException;

/** Darstellung von Werten in DML. Klassen, die Werte in DML repräsentieren,
 *  müssen dieses Interface implementieren.
*/
public interface DMLValue extends Serializable {
    /** Funktionale Werte und Konstruktoren können mit dieser Methode
     *  appliziert werden. Andere Werte erzeugen Laufzeitfehler.
     *  @param val der formale Parameter der Funktion/des Konstruktors
     *  @return DMLValue das Ergebnis der Applikation
     */
    public DMLValue apply(DMLValue val) throws RemoteException;
    public DMLValue apply0() throws RemoteException;
    public DMLValue apply2(DMLValue v1, DMLValue v2)
	throws RemoteException;
    public DMLValue apply3(DMLValue v1, DMLValue v2, DMLValue v3)
	throws RemoteException;
    public DMLValue apply4(DMLValue v1, DMLValue v2, DMLValue v3, DMLValue v4)
	throws RemoteException;
}
