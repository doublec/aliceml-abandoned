import java.io.*;
import de.uni_sb.ps.dml.runtime.*;

public class Exec extends de.uni_sb.ps.dml.runtime.DMLThread {

    private void bla() {
	DMLValue a = new DMLInt(88);
	DMLValue b = new DMLString("99");
	a.equals(b);
    }
    private static void usage() {
	System.out.println("Exec:\nUsage: java Exec <filename> [runtimeargs]\n");
    }
    String[] argv = null;

    public Exec(String[] arg) {
	argv=arg;
    }

    public void run() {
	try {
	FileInputStream fin = new FileInputStream(argv[0]);
	DMLObjectInputStream in = new DMLObjectInputStream(fin);
	DMLValue r = (DMLValue) in.readObject();
	System.out.println(r);
	System.out.println(((DMLRecord) r).getByLabel("main").apply(DMLConstants.dmlunit));
	} catch (Exception e) {
	    System.err.println(e);
	    e.printStackTrace();
	}
    }

    public static void main(String[] args) {
	if (args.length<1) {
	    usage();
	    System.exit(2);
	}
	else
	    (new Exec(args)).start();
    }
}
