package de.uni_sb.ps.dml.runtime;

public interface SManager extends java.rmi.Remote {
    public DMLValue request(CManager cm) throws java.rmi.RemoteException;
}
