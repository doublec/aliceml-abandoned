package de.uni_sb.ps.dml.runtime;

final public class Constants {

    public static final Name dmlunit = new Name("unit");
    public static final Name dmltrue = new Name("true");
    public static final Name dmlfalse= new Name("false");
    public static final Name dmlnil  = new Name("nil");

    public static final Name dmlmatch = new Name("[Match]");
    public static final Name dmlbind  = new Name("[Bind]");
    public static final Name none  = new Name("NONE");

    public static final Constructor some = new Constructor("SOME");
    public static final Constructor runtimeError = new Constructor("runtimeError");
    public static final Constructor reference    = new Constructor("reference");
    public static final Constructor cons         = new Constructor("cons");

    /** Diese Exception gibt's, um java-Exceptions zu verpacken */
    public static final Constructor javaAPIException = new Constructor("javaAPIException");

    /** Diese Exception zeigt wirkliche FEHLER des Benutzers an:
     *  - nicht-existierende Methoden/Konstructoren
     *  - Sicherheitsfehler
     */
    public static final Constructor javaAPIError = new Constructor("javaAPIError");

}
