(*
 * Authors:
 *   Robert Grabowski <grabow@ps.uni-sb.de>
 *
 * Copyright:
 *   Robert Grabowski, 2003
 *
 * Last Change:
 *   $Date$ by $Author$
 *   $Revision$
 *
 *)

signature UTIL =
    sig
        datatype spaces = GDK | GTK | GNOMECANVAS | PANGO | MISC
        val allSpaces : spaces list
        val stdSpaces : spaces list

        val moduleName : spaces -> string
        val spaceName : spaces -> string
        val spaceFuncPrefix : spaces -> string
        val spaceEnumPrefix : spaces -> string
        val spaceStructPrefix : spaces -> string

        val firstLower : string -> string
        val firstUpper : string -> string
        val strUpper : string -> string
        val replaceChar : char * string -> string -> string

        val cutPrefix : string * string -> string
        val checkPrefix : string -> string -> bool

        val computeWrapperName : spaces * string -> string
        val computeEnumName : spaces * string -> string

        val indent : int -> string
        val makeTuple : string -> string -> string list -> string

        val filters : ('a -> bool) list -> 'a list -> 'a list
        val funNot : ('a -> bool) -> 'a -> bool
        val contains : ''a -> ''a list -> bool
        val removeDuplicates : ('a * 'a -> bool) -> 'a list -> 'a list

        type fileInfo = {name: string, intro: string list, outro: string list}
        type fileHandle
        val openFile : fileInfo -> fileHandle
        val outputStrings : fileHandle -> string list -> unit
        val closeFile : fileHandle -> unit
	
        val separator : string -> string
    end
