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

public interface DMLConVal extends DMLValue {

    /** liefert den Inhalt */
    public DMLValue getContent() throws java.rmi.RemoteException;

    /** liefert den Constructor */
    public Constructor getConstructor() throws java.rmi.RemoteException;

    /*
     * the get shortcuts
     */
    public DMLValue get0() throws java.rmi.RemoteException;
    public DMLValue get1() throws java.rmi.RemoteException;
    public DMLValue get2() throws java.rmi.RemoteException;
    public DMLValue get3() throws java.rmi.RemoteException;
    public DMLValue get4() throws java.rmi.RemoteException;
}
