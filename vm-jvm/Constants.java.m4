package de.uni_sb.ps.dml.runtime;

final public class Constants {

    public static final Name dmlunit = new UniqueName("unit");
    public static final Name dmltrue = new UniqueName("true");
    public static final Name dmlfalse= new UniqueName("false");
    //    public static final UniqueName dmlnil  = new UniqueName("nil"); -> siehe List

    public static final Name dmlmatch = new UniqueName("[Match]");
    public static final Name dmlbind  = new UniqueName("[Bind]");

    public static final Constructor runtimeError = new UniqueConstructor("runtimeError");
    public static final Constructor reference    = new UniqueConstructor("reference");
    //    public static final Constructor cons         = new Constructor("cons");

    /** Diese Exception gibt's, um java-Exceptions zu verpacken */
    public static final Constructor javaAPIException = new Constructor("javaAPIException");

    /** Diese Exception zeigt wirkliche FEHLER des Benutzers an:
     *  - nicht-existierende Methoden/Konstructoren
     *  - Sicherheitsfehler
     */
    public static final Constructor javaAPIError = new Constructor("javaAPIError");

}
