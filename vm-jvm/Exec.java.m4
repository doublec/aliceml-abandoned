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
	try {
	    FileInputStream fin = new FileInputStream(filename);
	    PickleInputStream in = new PickleInputStream(fin);
	    DMLValue r = (DMLValue) in.readObject();
	    System.out.println(r);
	    v=((Record) r).get("main").apply(arglist);
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
