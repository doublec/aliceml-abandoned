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

import java.rmi.RemoteException;

public interface DMLConVal extends DMLValue {

    /** liefert den Inhalt */
    public DMLValue getContent() throws RemoteException;

    /** liefert den Constructor */
    public Constructor getConstructor() throws RemoteException;

    /*
     * the get shortcuts
     */
    public DMLValue get0() throws RemoteException;
    public DMLValue get1() throws RemoteException;
    public DMLValue get2() throws RemoteException;
    public DMLValue get3() throws RemoteException;

    public void set(DMLValue v0) throws RemoteException;
    public void set(DMLValue v0,DMLValue v1) throws RemoteException;
    public void set(DMLValue v0,DMLValue v1,DMLValue v2) throws RemoteException;
    public void set(DMLValue v0,DMLValue v1,DMLValue v2,DMLValue v3) throws RemoteException;
}
