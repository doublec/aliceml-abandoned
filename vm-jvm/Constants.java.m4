package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLConstants {
    public static final DMLConstructor0 dmlunit = new DMLConstructor0("unit");
    public static final DMLConstructor0 dmltrue = new DMLConstructor0("boolean");
    public static final DMLConstructor0 dmlfalse= new DMLConstructor0("boolean");
    public static final DMLConstructor0 dmlnil  = new DMLConstructor0("list");

    public static final DMLExName dmlmatch = new DMLExName("[Match]",0);
    public static final DMLExName dmlbind  = new DMLExName("[Bind]",0);

    public static final int CONAME  = 0;
    public static final int EXNAME  = 1;
    public static final int BUILTIN = 2;
    public static final int NOTAPPLICAPLE = -1;
}
