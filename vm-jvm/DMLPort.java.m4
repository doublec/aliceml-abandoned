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
import java.rmi.Remote;

public interface DMLPort extends Remote, DMLValue {

    public DMLValue apply(DMLValue val) throws RemoteException;
    public DMLValue apply0() throws java.rmi.RemoteException;
    public DMLValue apply2(DMLValue v1, DMLValue v2)
	throws java.rmi.RemoteException;
    public DMLValue apply3(DMLValue v1, DMLValue v2, DMLValue v3)
	throws java.rmi.RemoteException;
    public DMLValue apply4(DMLValue v1, DMLValue v2, DMLValue v3, DMLValue v4)
	throws java.rmi.RemoteException;

    public DMLValue send(DMLValue msg) throws RemoteException;

}
