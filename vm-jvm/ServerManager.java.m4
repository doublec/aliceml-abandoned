package de.uni_sb.ps.dml.runtime;

final public class ServerManager extends java.rmi.server.UnicastRemoteObject implements SManager {

    CManager contentOwner;

    public ServerManager(CManager initial) throws java.rmi.RemoteException {
	contentOwner=initial;
    }

    public synchronized DMLValue request(CManager iWantIt) throws java.rmi.RemoteException {
	DMLValue val = contentOwner.release();
	contentOwner = iWantIt;
	return val;
    }

}
