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

    private static void usage() {
	System.out.println("Exec:\nUsage: java Exec <filename> [runtimeargs]\n");
    }
    java.lang.String filename = null;
    DMLValue arglist = null;

    public Exec(java.lang.String[] arg) {
	super(null);
	filename = arg[0];
	if (arg.length<2) {
	    arglist = List.nil;
	} else {
	    Cons c = new Cons(new de.uni_sb.ps.dml.runtime.String(arg[1]),null);
	    arglist = c;
	    for(int i=2; i<arg.length; i++) {
		c.setCdr(new Cons(new de.uni_sb.ps.dml.runtime.String(arg[i]),null));
		c = (Cons) c.getCdr();
	    }
	    c.setCdr(List.nil);
	}
    }

    public void run() {
	DMLValue v = null;
	DMLValue t = null;
	long time = 0l;
	try {
	    FileInputStream fin = new FileInputStream(filename);
	    PickleInputStream in = new PickleInputStream(fin);
	    in.readObject(); // read the literals class
	    DMLValue r = (DMLValue) in.readObject();
	    System.out.println(r);
	    time = System.currentTimeMillis();
	    t = ((Record) r).get("main");
	    if (t==null) {
		System.err.println("No main defined.");
		System.exit(1);
	    }
	    v=t.apply(arglist);
	    while (tail!=null) {
		t = tail;
		tail = null;
		v=t.apply(v);
	    }
	} catch (Exception e) {
	    e.printStackTrace();
	}
	    System.out.println("Execution time [ms]: "+ (System.currentTimeMillis() - time));
    }

    public static void main(java.lang.String[] args) {
	if (args.length<1) {
	    usage();
	    System.exit(2);
	} else {
	    (new Exec(args)).start();
	}
    }
}
