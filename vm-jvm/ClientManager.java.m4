/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

public class ClientManager extends java.rmi.server.UnicastRemoteObject implements CManager {

    Reference ref = null;

    public ClientManager(Reference r) throws java.rmi.RemoteException {
	ref=r;
    }

    final public DMLValue release() throws java.rmi.RemoteException {
	return ref.release();
    }
}
