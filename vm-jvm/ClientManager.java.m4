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
import java.rmi.server.UnicastRemoteObject;

public class ClientManager extends UnicastRemoteObject
    implements CManager {

    Reference ref = null;

    public ClientManager(Reference r) throws RemoteException {
	ref=r;
    }

    final public DMLValue release() throws RemoteException {
	return ref.release();
    }
}
