/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;
/** This is the implementation of the server manager of a reference cell.
 */
final public class ServerManager extends java.rmi.server.UnicastRemoteObject
    implements SManager {

    CManager contentOwner;

    public ServerManager(CManager initial) throws java.rmi.RemoteException {
	contentOwner=initial;
    }

    final public synchronized DMLValue request(CManager iWantIt) throws java.rmi.RemoteException {
	DMLValue val = contentOwner.release();
	contentOwner = iWantIt;
	return val;
    }
}
