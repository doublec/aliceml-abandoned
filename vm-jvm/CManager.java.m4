package de.uni_sb.ps.dml.runtime;

public interface CManager extends java.rmi.Remote {
    public DMLValue release() throws java.rmi.RemoteException;
}
