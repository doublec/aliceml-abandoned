package de.uni_sb.ps.dml.builtin;

import de.uni_sb.ps.dml.runtime.*;

final public class Option {
    // datatype 'a option = NONE | SOME of 'a 
    final public static DMLName NONE = new DMLName("Option.NONE");
    final public static DMLConstructor SOME = new DMLConstructor("Option.SOME");

    // exception Option
    final public static DMLName Option = new DMLName("Option.Option");

    // val getOpt : ('a option * 'a) -> 'a 
    // val isSome : 'a option -> bool 
    // val valOf : 'a option -> 'a 
    // val filter : ('a -> bool) -> 'a -> 'a option 
    // val join : 'a option option -> 'a option 
    // val map : ('a -> 'b) -> 'a option -> 'b option 
    // val mapPartial : ('a -> 'b option) -> 'a option -> 'b option 
    // val compose : (('a -> 'b) * ('c -> 'a option)) -> 'c -> 'b option 
    // val composePartial : (('a -> 'b option) * ('c -> 'a option)) -> 'c -> 'b option
}
