package de.uni_sb.ps.dml.runtime;

public class ClientManager implements CManager {

    Reference ref = null;

    public ClientManager(Reference r) {
	ref=r;
    }

    public DMLValue release() throws java.rmi.RemoteException {
	return ref.release();
    }
}
