package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLConstants {
    public static final DMLCoEx0 dmlunit = new DMLCoEx0("unit");
    public static final DMLCoEx0 dmltrue = new DMLCoEx0("boolean");
    public static final DMLCoEx0 dmlfalse= new DMLCoEx0("boolean");
    public static final DMLCoEx0 dmlnil  = new DMLCoEx0("list");

    public static final DMLCoExName dmlmatch = new DMLCoExName("[Match]",0);
    public static final DMLCoExName dmlbind  = new DMLCoExName("[Bind]",0);

    public static final DMLCoExName runtimeError = new DMLCoExName("runtimeError",1);

}
