/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

/** This is the remote interface for the server manager of a reference cell.
 */
public interface SManager extends java.rmi.Remote {
    public DMLValue request(CManager cm) throws java.rmi.RemoteException;
}
