package de.uni_sb.ps.DML.DMLRuntime;

final public class DMLConstants {

    public static final DMLName dmlunit = new DMLName("unit");
    public static final DMLName dmltrue = new DMLName("boolean");
    public static final DMLName dmlfalse= new DMLName("boolean");
    public static final DMLName dmlnil  = new DMLName("list");

    public static final DMLName dmlmatch = new DMLName("[Match]");
    public static final DMLName dmlbind  = new DMLName("[Bind]");

    public static final DMLConstructor runtimeError = new DMLConstructor("runtimeError");
    public static final DMLConstructor reference    = new DMLConstructor("reference");
    public static final DMLConstructor cons         = new DMLConstructor("list");

}
