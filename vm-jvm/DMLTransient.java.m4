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

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface DMLTransient extends Remote, DMLValue {

    public DMLValue getValue() throws RemoteException;

    public DMLValue request() throws RemoteException;

    public DMLValue bind(DMLValue val) throws RemoteException;
}
