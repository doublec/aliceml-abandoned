/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

import java.rmi.RemoteException;
import java.rmi.Remote;

public interface DMLPort extends Remote, DMLValue {

    public DMLValue apply(DMLValue val) throws RemoteException;

    public DMLValue send(DMLValue msg) throws RemoteException;

    public DMLValue recieve() throws RemoteException;
}
