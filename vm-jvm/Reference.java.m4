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

import java.rmi.RemoteException;

/** This class is the representation of reference cells. It implements the
 *  mobile protocol when used in an distributed environment.
 *  SYNCHRONIZATION:<code>
 *  let
 *     val r1 = ref 0
 *     val r2 = ref 0
 *  in
 *     spawn (fn () => (r1 := 1; r2 := 2);
 *     (!r1, !r2)
 *  end
 *  </code>
 *  May: (0, 0), (1, 0), (1, 2)
 *  Must not: (0, 2);
 *  To avoid the second case, access methods have to be synchronized.
 */
final public class Reference implements DMLConVal {

    private SManager mgr = null;      // Homesite-Manager
    public DMLValue content = null;
    private CManager cmgr = null;     // Clientsite-Manager

    public Reference() {}

    public Reference(DMLValue ct) throws RemoteException {
	content = ct;
    }

    final synchronized public DMLValue release() {
	DMLValue t = content;
	content = null;
	return t;
    }

    final synchronized public DMLValue get0()
	throws RemoteException {
	if (content == null) {
	    content = mgr.request(cmgr);
	}
	_REQUEST(content,content);
	if (content instanceof DMLTuple) {
	    return ((DMLTuple) content).get0();
	} else {
	    return content;
	}
    }


    final synchronized public DMLValue get1()
	throws RemoteException {
	if (content == null) {
	    content = mgr.request(cmgr);
	}
	_REQUEST(content,content);
	if (content instanceof DMLTuple) {
	    return ((DMLTuple) content).get1();
	} else
	    throw new ArrayIndexOutOfBoundsException();
    }

    final synchronized public DMLValue get2()
	throws RemoteException {
	if (content == null) {
	    content = mgr.request(cmgr);
	}
	_REQUEST(content,content);
	if (content instanceof DMLTuple)
	    return ((DMLTuple) content).get2();
	else
	    throw new ArrayIndexOutOfBoundsException();
    }

    final synchronized public DMLValue get3()
	throws RemoteException {
	if (content == null) {
	    content = mgr.request(cmgr);
	}
	_REQUEST(content,content);
	if (content instanceof DMLTuple)
	    return ((DMLTuple) content).get3();
	else
	    throw new ArrayIndexOutOfBoundsException();
    }

    final synchronized public DMLValue getContent()
	throws RemoteException {
	if (content == null) {
	    content = mgr.request(cmgr);
	}
	return content;
    }

    /** setzt Wert auf val und gibt alten Wert zurueck */
    final synchronized public DMLValue assign(DMLValue val)
	throws RemoteException {
	if (content == null) {
	    content = mgr.request(cmgr);
	}
	content = val;
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
	//      return (content==null?"remote":content.toString())+" : ref";
    }

    final public java.lang.String toString(int level) throws java.rmi.RemoteException {
	if (level<1) {
	    return "...";
	} else {
	    if (Function.DEBUG) {
		java.lang.String s = (mgr==null ? "no server-manager, " : "server: "+mgr+", ");
		s+= (cmgr==null ? "no client-manager, " : "client: "+cmgr+", ");
		s+=(content==null?"remote":content.toString(level-1))+" : ref";
		return s;
	    } else {
		return "ref "+content.toString(level-1);
	    }
	    //      return (content==null?"remote":content.toString())+" : ref";
	}
    }

    final public Constructor getConstructor() {
	return Constants.reference;
    }

    final public synchronized DMLValue exchange(DMLValue val)
	throws RemoteException {
	if (content == null) {
	    content = mgr.request(cmgr);
	}
	DMLValue ret = content;
	content = val;
	return ret;
    }

    final private void writeObject(java.io.ObjectOutputStream out)
	throws java.io.IOException {
	try {
	    // the client manager is not written to the stream
	    // since the readObject method will install a new one
	    CManager CMGR = null;
	    if (cmgr == null) { // i.e. we are on the home site
		CMGR = new ClientManager(this);
	    } else { // previously installed by readObject
		CMGR = cmgr;
	    }
	    cmgr = null;
	    // the content will not be transferred immediately
	    DMLValue t = content;
	    content = null;
	    // install and export the server manager
	    if (mgr == null) { // i.e. we are at home
		mgr = new ServerManager(CMGR);
	    }
	    // write Reference to stream and restore the client
	    // manager and the content
	    out.defaultWriteObject();
	    cmgr = CMGR;
	    content = t;
	} catch (RemoteException e) {
	    System.err.println(e);
	}
    }

    final protected void readObject(java.io.ObjectInputStream in)
	throws java.io.IOException, ClassNotFoundException {
	in.defaultReadObject();
	cmgr = new ClientManager(this);
    }

    final public void set(DMLValue v0) throws RemoteException {
	content = v0;
    }

    final public void set(DMLValue v0,DMLValue v1) throws RemoteException {
	content = new Tuple2(v0,v1);
    }

    final public void set(DMLValue v0,DMLValue v1,DMLValue v2) throws RemoteException {
	content = new Tuple3(v0,v1,v2);
    }

    final public void set(DMLValue v0,DMLValue v1,DMLValue v2,DMLValue v3) throws RemoteException {
	content = new Tuple4(v0,v1,v2,v3);
    }
    _apply_fails ;
}
