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

    /** Diese Exception gibt's, um java-Exceptions zu verpacken */
    public static final DMLConstructor javaAPIException = new DMLConstructor("javaAPIException");

    /** Diese Exception zeigt wirkliche FEHLER des Benutzers an:
     *  - nicht-existierende Methoden/Konstructoren
     *  - Sicherheitsfehler
     */
    public static final DMLConstructor javaAPIError = new DMLConstructor("javaAPIError");

}
