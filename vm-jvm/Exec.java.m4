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
package de.uni_sb.ps.dml.tools;

import java.io.*;
import de.uni_sb.ps.dml.runtime.*;
import de.uni_sb.ps.dml.runtime.PickleInputStream;

public class Exec extends de.uni_sb.ps.dml.runtime.Thread {

    java.lang.String filename = null;
    DMLValue arglist = null;
    boolean showpickle = false;
    boolean showtime = false;

    public Exec(java.lang.String[] arg) {
	super(null);
	int arglength = arg.length;
	int argc = 0; // arg counter
	java.lang.String ark = arg[0];
	while (ark.startsWith("-")) {
	    if (ark.indexOf('s') > -1) {
		showpickle = true;
	    }
	    if (ark.indexOf('t') > -1) {
		showtime = true;
	    }
	    argc++;
	    if (argc < arglength) {
		ark = arg[argc];
	    } else {
		System.err.println("Error: No input file specified.");
		usage();
		System.exit(1);
	    }
	}
	if (ark.endsWith(".pickle")) {
	    filename = ark;
	} else {
	    filename = ark + ".pickle";
	}
	argc++;
	if (argc >= arglength) {
	    arglist = List.nil;
	} else {
	    Cons c = new Cons(new de.uni_sb.ps.dml.runtime.String(arg[argc++]),null);
	    arglist = c;
	    for(int i=argc; i<arglength; i++) {
		c.cdr = new Cons(new de.uni_sb.ps.dml.runtime.String(arg[i]),null);
		c = (Cons) c.cdr;
	    }
	    c.cdr = List.nil;
	}
    }

    public void run() {
	long time = 0l;
	try {
	    FileInputStream fin = new FileInputStream(filename);
	    java.util.zip.GZIPInputStream zip = new java.util.zip.GZIPInputStream(fin);
	    PickleInputStream in = new PickleInputStream(zip);
	    in.readObject(); // read the literals class
	    DMLValue r = (DMLValue) in.readObject();
	    fin.close(); in.close(); zip.close();
	    if (showpickle) {
		System.out.println(r.toString(3));
	    }
	    if (showtime) {
		time = System.currentTimeMillis();
	    }
	    fcn = ((Record) r).get("main");
	    if (fcn == null) {
		System.err.println("No main function defined.");
		System.exit(1);
	    }
	    fcn.apply(arglist);
	}catch (FileNotFoundException f) {
	    System.err.println("Could not find: " + filename);
	    System.exit(1);
	} catch (Exception e) {
	    e.printStackTrace();
	}

	if (showtime) {
	    System.out.println("Execution time [ms]: "+ (System.currentTimeMillis() - time));
	}
    }

    public static void main(java.lang.String[] args) {
	if (args.length<1) {
	    usage();
	    System.exit(2);
	} else {
	    (new Exec(args)).start();
	}
    }

    private static void usage() {
	System.out.println("Exec usage:\n java Exec [ts] <filename> [runtimeargs]\n\tt : show execution time\n\ts : show content of pickle\n\tfilename : we try filename and filename.pickle");
    }
}
