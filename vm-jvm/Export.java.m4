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

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.RMISecurityManager;
import java.rmi.server.*;

public interface Export extends java.rmi.Remote {

    /** This method returns objects of any DMLValue,
     *  not including class code.
     */
    public DMLValue get(java.lang.String what) throws java.rmi.RemoteException;

    /** This method returns the class code that can be loaded
     *  via the PickleClassLoader. The static fields are not transferred.
     */
    public byte[] getClass(java.lang.String className) throws java.rmi.RemoteException;

    /** This method gives the values of the class fields of DMLValues.
     */
    public Object getField(java.lang.String fieldName) throws java.rmi.RemoteException;
}
