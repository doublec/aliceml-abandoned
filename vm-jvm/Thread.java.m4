package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLThread extends Thread implements java.io.Serializable {

  DMLValue fcn=null;

  /** Gesamtzahl DMLThreads */
  static int totalNumber=0;

  /** Nummer des Threads */
  int threadNumber=0;

  public DMLThread(DMLValue v) {
    super();
    this.fcn=v;
    threadNumber=totalNumber++;
  }

  final public void run() {
    fcn.apply(DMLConstants.dmlunit);
  }

  final public String toString() {
    return "Thread["+threadNumber+"] ("+fcn+")\n"
      +"Is a leif: "+this.isAlive()
      +"Is interrupted: "+this.isInterrupted();
  }
}
