import java.io.*;
import de.uni_sb.ps.dml.runtime.*;
import de.uni_sb.ps.dml.runtime.PickleInputStream;

public class Exec extends de.uni_sb.ps.dml.runtime.Thread {

    private static void usage() {
	System.out.println("Exec:\nUsage: java Exec <filename> [runtimeargs]\n");
    }
    java.lang.String[] argv = null;

    public Exec(java.lang.String[] arg) {
	argv=arg;
    }

    public void run() {
	DMLValue v = null;
	DMLValue t = null;
	try {
	    FileInputStream fin = new FileInputStream(argv[0]);
	    PickleInputStream in = new PickleInputStream(fin);
	    DMLValue r = (DMLValue) in.readObject();
	    System.out.println(r);
	    v=((Record) r).getByLabel("main").apply(Constants.dmlunit);
	    while (tail!=null) {
		t = tail;
		tail = null;
		v=t.apply(v);
	    }
    } catch (Exception e) {
	    System.err.println(e);
	    e.printStackTrace();
	}
    }

    public static void main(java.lang.String[] args) {
	if (args.length<1) {
	    usage();
	    System.exit(2);
	}
	else
	    (new Exec(args)).start();
    }
}
