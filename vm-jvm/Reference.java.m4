/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

/** This class is the representation of reference cells. It implements the
 *  mobile protocol when used in an distributed environment.
 */
final public class Reference implements DMLConVal, DMLReference {

    SManager mgr = null;      // Homesite-Manager
    DMLValue content = null;
    CManager cmgr = null;     // Clientsite-Manager

    public Reference(DMLValue content) throws java.rmi.RemoteException {
	this.content=content;
    }

    final public DMLValue release() {
	DMLValue t = content;
	content = null;
	return t;
    }

    final public DMLValue get0() throws java.rmi.RemoteException {
	if (content==null) {
	    content=mgr.request(cmgr);
	}
	if (content instanceof DMLTuple) {
	    return ((DMLTuple) content).get0();
	} else {
	    return content;
	}
    }


    final public DMLValue get1() throws java.rmi.RemoteException {
	if (content==null) {
	    content=mgr.request(cmgr);
	}
	if (content instanceof DMLTuple) {
	    return ((DMLTuple) content).get1();
	} else
	    throw new ArrayIndexOutOfBoundsException(); 
    }

    final public DMLValue get2() throws java.rmi.RemoteException {
	if (content==null) {
	    content=mgr.request(cmgr);
	}
	if (content instanceof DMLTuple)
	    return ((DMLTuple) content).get2();
	else
	    throw new ArrayIndexOutOfBoundsException(); 
    }

    final public DMLValue get3() throws java.rmi.RemoteException {
	if (content==null) {
	    content=mgr.request(cmgr);
	}
	if (content instanceof DMLTuple)
	    return ((DMLTuple) content).get3();
	else
	    throw new ArrayIndexOutOfBoundsException(); 
    }

    final public DMLValue get4() throws java.rmi.RemoteException {
	if (content==null) {
	    content=mgr.request(cmgr);
	}
	if (content instanceof DMLTuple)
	    return ((DMLTuple) content).get4();
	else
	    throw new ArrayIndexOutOfBoundsException(); 
    }

    final public DMLValue getContent() throws java.rmi.RemoteException {
	if (content==null) {
	    content=mgr.request(cmgr);
	}
	return content;
    }

    /** setzt Wert auf val und gibt alten Wert zurueck */
    final public DMLValue assign(DMLValue val) throws java.rmi.RemoteException {
	if (content==null) {
	    content=mgr.request(cmgr);
	}
	content=val;
	return Constants.dmlunit;
    }

    final public java.lang.String toString() {
	if (Function.DEBUG) {
	    java.lang.String s = (mgr==null ? "no server-manager, " : "server: "+mgr+", ");
	    s+= (cmgr==null ? "no client-manager, " : "client: "+cmgr+", ");
	    s+=(content==null?"remote":content.toString())+" : ref";
	    return s;
	} else {
	    return content+" : ref";
	}
	//	return (content==null?"remote":content.toString())+" : ref";
    }

    final public Constructor getConstructor() {
	return Constants.reference;
    }

    final public synchronized DMLValue exchange(DMLValue val) throws java.rmi.RemoteException {
	if (content==null) {
	    content=mgr.request(cmgr);
	}
	DMLValue ret = content;
	content = val;
	return ret;
    }

    final private void writeObject(java.io.ObjectOutputStream out)
	throws java.io.IOException {
	try {
	    if (mgr==null) {
		ClientManager CMGR=null;
		if (cmgr==null)
		    CMGR = new ClientManager(this);
		ServerManager MGR = new ServerManager(CMGR);
		mgr=MGR;
		DMLValue t = content;
		content = null;
		out.defaultWriteObject();
		cmgr=CMGR;
		content = t;
	    } else {
		CManager CMGR = cmgr;
		cmgr=null;
		DMLValue t = content;
		content = null;
		out.defaultWriteObject();
		content = t;
		cmgr = CMGR;
	    }
	} catch (java.rmi.RemoteException e) {
	    System.err.println(e);
	}
    }

    final private void readObject(java.io.ObjectInputStream in)
	throws java.io.IOException, ClassNotFoundException {
	in.defaultReadObject();
	cmgr = new ClientManager(this);
    }

    _apply_fails ;
}
