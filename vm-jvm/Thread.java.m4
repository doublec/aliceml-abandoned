/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

/** Diese Klasse repräsentiert de.uni_sb.ps.dml.runtime.Threads.
 *  java.lang.Threads sind First-Class; indem das Interface <code>DMLValue</code>
 *  implementiert wird, können sie wie andere Werte in DML verwendet werden.
 *  @see DMLValue
 */

public class Thread extends java.lang.Thread implements DMLValue {
    /** Hier wird die Continuation für die Tail-Calls übergeben. */
    public DMLValue tail=null;
    private NoGood ng = null;
    /** Die Funktion (oder etwas, das zu einer Funktion wird),
     *  die der java.lang.Thread ausführt.
     *  Die Funktion sollte den Typ fcn : unit -> 'a haben.
     */
    private DMLValue fcn=null;

    /** Gesamtzahl de.uni_sb.ps.dml.runtime.Threads */
    public static int totalNumber=0;

    /** Nummer des java.lang.Threads */
    private int threadNumber=0;

    public Thread() {
    }

    /** Erzeugt einen neuen de.uni_sb.ps.dml.runtime.Thread.
     *  @param v sollte eine Funktion f : unit-> 'a sein.
     */
    public Thread(DMLValue v) {
	this.fcn=v;
	threadNumber=totalNumber++;
    }

    /** Appliziert den DMLValue des java.lang.Threads.
     *  Der Wert wird mit Argument unit appliziert. Der Rückgabewert wird
     *  verworfen.
     */
    public void run() {
	DMLValue v = null;
	DMLValue t = null;
	try {
	    v = fcn.apply(Constants.dmlunit);
	    while(tail!=null) {
		t=tail;
		tail=null;
		v=t.apply(v);
	    }
	} catch (java.rmi.RemoteException r) {
	    System.err.println(r);
	    r.printStackTrace();
	}
    }

    /** java.lang.Stringdarstellung des de.uni_sb.ps.dml.runtime.Thread.
     *  Gibt die Nummer des java.lang.Threads, den Status und die Gesamtzahl
     *  der bisher erzeugten java.lang.Threads an.
     */
    final public java.lang.String toString() {
	return "Thread["+threadNumber+"] ("+fcn+")\n"
	    +"Is a leif: "+this.isAlive()
	    +"Is interrupted: "+this.isInterrupted();
    }

    private Object writeReplace()
	throws java.io.ObjectStreamException {
	if (ng==null) { // falls zum ersten Mal serialisiert
	    GName gn=new GName();
	    ng=new NoGood(gn);
	    GName.gNames.put(gn,ng);
	    return ng;
	} else {
	    return ng;
	}
    }

    _apply_fails ;
    _request_id ;
    _getValue_id ;
    _raise ;
}
