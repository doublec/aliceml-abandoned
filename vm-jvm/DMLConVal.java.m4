package de.uni_sb.ps.dml.runtime;

public interface DMLConVal extends DMLValue {

    /** liefert den Inhalt */
    public DMLValue getContent();

    /** liefert den Constructor */
    public DMLConstructor getConstructor();

    /** wird nur bei Ref funktionieren */
    public DMLValue assign(DMLValue val);

    public DMLValue get0();
    public DMLValue get1();
    public DMLValue get2();
    public DMLValue get3();
    public DMLValue get4();
}
