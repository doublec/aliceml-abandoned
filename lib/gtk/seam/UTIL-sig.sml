(*
 * Authors:
 *   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
 *
 * Copyright:
 *   Thorsten Brunklaus, 2001
 *
 * Last Change:
 *   $Date$ by $Author$
 *   $Revision$
 *
 *)

signature UTIL =
    sig
	(* Char Transformation *)
	val firstLower : string -> string
	val firstUpper : string -> string
	val strUpper : string -> string
        val replaceChar : char * string -> string -> string

        (* Namespace and prefix stuff *)
        datatype spaces = GDK | GTK | GNOMECANVAS | PANGO
	val allSpaces : spaces list

	val spaceName : spaces -> string
	val spaceFuncPrefix : spaces -> string
	val spaceEnumPrefix : spaces -> string
	val spaceStructPrefix : spaces -> string


	val cutPrefix : string * string -> string
	val checkPrefix : string -> string -> bool

	(* Func Name Translation : gtk_foo_bar -> gtkFooBar *)
	val translateName : string -> string
	(* Compute Wrapper Name : gtkBaz -> Baz *)
	val computeWrapperName : spaces * string -> string
	(* Compute Enum Name : GTK_WINDOW_TOPLEVEL -> WINDOW_TOPLEVEL *)
	val computeEnumName : spaces * string -> string

	val separator : string -> string
	(* Code generation *)
	val indent : int -> string
	val makeTuple : string -> string -> string list -> string

        (* Filter stuff *)
	val filters : ('a -> bool) list -> 'a list -> 'a list
	val funNot : ('a -> bool) -> 'a -> bool
	val contains : ''a -> ''a list -> bool
	val removeDuplicates : ('a * 'a -> bool) -> 'a list -> 'a list

        (* File stuff *)
	type fileInfo = {name: string, intro: string list, outro: string list}
	type fileHandle
	val openFile : fileInfo -> fileHandle
        val outputStrings : fileHandle -> string list -> unit
        val closeFile : fileHandle -> unit
	
    end
