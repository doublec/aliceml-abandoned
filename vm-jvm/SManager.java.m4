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

/** This is the remote interface for the server manager of a reference cell.
 */
public interface SManager extends Remote {
    public DMLValue request(CManager cm) throws RemoteException;
}
