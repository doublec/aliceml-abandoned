/*
 * $Date$
 * $Revision$
 * $Author$
 */

package de.uni_sb.ps.dml.runtime;

final public class Option {
    // datatype 'a option = NONE | SOME of 'a 
    final public static Name NONE = new UniqueName("Option.NONE");
    final public static Constructor SOME = new UniqueConstructor("Option.SOME");

    // exception Option
    final public static Name Option = new UniqueName("Option.Option");

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
