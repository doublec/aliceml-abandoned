(*
 * Authors:
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *
 * Copyright:
 *   Andreas Rossberg, 2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)


(*
 *  Items marked with (**) are extensions to the Standard Basis.
 *)


local

structure __pervasive =
  struct
    structure Label =
    struct
	fun fromString x = fromString x
    end

    structure Path =
    struct
	fun fromLab x = fromLab x
	fun pervasive x = pervasive x
    end

    structure Type =
    struct
	fun var x = var x
	fun inVar x = inVar x
	fun inCon x = inCon x
	fun inApply x = inApply x
	fun inLambda x = inLambda x
	fun inArrow x = inArrow x
	fun inTuple x = inTuple x
	fun inProd x = inProd x
	fun inSum x = inSum x
	fun inAll x = inCon x
	fun inExist x = inExist x
	fun unknown x = unknown x
	fun fill x = fill x
	fun unknownRow x = unknownRow x
	fun emptyRow x = emptyRow x
	fun extendRow x = extendRow x
    end

    structure Inf =
    struct
    end
  end

structure __pervasive =
  struct
    open __pervasive

    __primitive type zero		= "zero"
    __primitive type 'a succ		= "succ"
    __primitive type ('a,'b) conarrow	= "conarrow"
  end

structure __pervasive =
  struct
    open __pervasive

    __primitive datatype exn = "exn"
    datatype 'a list = nil | op:: of 'a * 'a list

    structure Int	= struct __primitive eqtype int		= "int"     end
    structure LargeInt	= struct               type int		= Int.int   end
    structure Word	= struct __primitive eqtype word	= "word"    end
    structure LargeWord	= struct               type word	= Word.word end
    structure Real	= struct __primitive eqtype real	= "real"    end
    structure LargeReal	= struct               type real	= Real.real end
    structure Char	= struct __primitive eqtype char	= "char"    end
    structure WideChar	= struct               type char	= Char.char end
    structure String	= struct __primitive eqtype string	= "string"  end
    structure WideString= struct               type string	= String.string
									    end
    structure Vector	= struct __primitive eqtype 'a vector	= "vector"  end
    structure Array	= struct __primitive __eqeqtype 'a array= "array"   end
    structure Ref	= struct __primitive __reftype 'a ref	= ref of 'a end
    structure General	= struct __primitive datatype exn	= "exn"     end
(*
    structure Time	= struct __primitive eqtype time	= "time"    end
    structure Promise	= struct __primitive type 'a promise	= "promise" end
*)
  end

in

signature BASIS = sig

(*****************************************************************************
 * Top-level, part 1
 *****************************************************************************)

(* Fixity *)

infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  3  :=:			(**)
infix  0  before

(* Pervasives *)

structure __pervasive :
  sig
    type zero		= __pervasive.zero
    type succ		= __pervasive.succ
    type conarrow	= __pervasive.conarrow

    structure Label :
    sig
	val fromString : 'a -> 'a
    end

    structure Path :
    sig
	val fromLab : 'a -> 'a
	val pervasive : 'a -> 'a
    end

    structure Type :
    sig
	val var : 'a -> 'a
	val inVar : 'a -> 'a
	val inCon : 'a -> 'a
	val inApply : 'a -> 'a
	val inLambda : 'a -> 'a
	val inArrow : 'a -> 'a
	val inTuple : 'a -> 'a
	val inProd : 'a -> 'a
	val inSum : 'a -> 'a
	val inAll : 'a -> 'a
	val inExist : 'a -> 'a
	val unknown : 'a -> 'a
	val fill : 'a -> 'a
	val unknownRow : 'a -> 'a
	val emptyRow : 'a -> 'a
	val extendRow : 'a -> 'a
    end

    structure Inf :
    sig
    end

    type exn = __pervasive.exn
    datatype list = datatype __pervasive.list

    structure Int	: sig type int     = __pervasive.Int.int           end
    structure LargeInt	: sig type int     =             Int.int           end
    structure Word	: sig type word    = __pervasive.Word.word         end
    structure LargeWord	: sig type word    =             Word.word         end
    structure Real	: sig type real    = __pervasive.Real.real         end
    structure LargeReal	: sig type real    =             Real.real         end
    structure Char	: sig type char    = __pervasive.Char.char         end
    structure WideChar	: sig type char    =             Char.char         end
    structure String	: sig type string  = __pervasive.String.string     end
    structure WideString: sig type string  =             String.string     end
    structure Vector	: sig type vector  = __pervasive.Vector.vector     end
    structure Array	: sig type array   = __pervasive.Array.array       end
    structure Ref	: sig datatype ref = datatype __pervasive.Ref.ref  end
    structure General	: sig type exn     = exn                           end
(*
    structure Time	: sig type time    = __pervasive.Time.time         end
    structure Promise	: sig type promise = __pervasive.Promise.promise   end
*)
  end


type    unit		= {}
type    int		= __pervasive.Int.int
type    word		= __pervasive.Word.word
type    real		= __pervasive.Real.real
type    char		= __pervasive.Char.char
type    string		= __pervasive.String.string
type 'a vector		= 'a __pervasive.Vector.vector
type 'a array		= 'a __pervasive.Array.array

datatype exn		= datatype __pervasive.General.exn
datatype ref		= datatype __pervasive.Ref.ref

datatype bool		= false | true
datatype order		= LESS | EQUAL | GREATER
datatype ('a,'b) alt	= FST of 'a | SND of 'b				(**)
datatype 'a option	= NONE | SOME of 'a
datatype 'a list	= nil | op:: of 'a * 'a list



(* Generic and overloaded Identifiers (but we don't overload them :-P) *)

val op =  :	''a * ''a -> bool
val op <> :	''a * ''a -> bool

val ~ :		int -> int
val op + :	int * int -> int
val op - :	int * int -> int
val op * :	int * int -> int
val op / :	real * real -> real
val op div :	int * int -> int
val op mod :	int * int -> int

val abs :	int -> int

val op < :	int * int -> bool
val op > :	int * int -> bool
val op <= :	int * int -> bool
val op >= :	int * int -> bool



(*****************************************************************************
 * General
 *****************************************************************************)

signature GENERAL =
  sig
    eqtype unit
    type exn

    exception Bind
    exception Chr
    exception Div
    exception Domain
    exception Fail of string
    exception Match
    exception Overflow
    exception Size
    exception Span
    exception Subscript

    val exnName :	exn -> string
    val exnMessage :	exn -> string

    datatype order =	LESS | EQUAL | GREATER

    val ! :		'a ref -> 'a
    val op := :		'a ref * 'a -> unit
    val op :=: :	'a ref * 'a ref -> unit			(**)

    val flip :		('a * 'b -> 'c) -> ('b * 'a -> 'c)	(**)
    val op o :		('b -> 'c) * ('a -> 'b) -> 'a -> 'c
    val before :	'a * unit -> 'a
    val ignore :	'a -> unit
  end


structure General : GENERAL where type unit = unit
			    where type exn  = exn



(*****************************************************************************
 * Ref
 *****************************************************************************)

signature REF =								(**)
  sig
    datatype ref = datatype ref
    type  'a t   = 'a ref

    val new :		'a -> 'a ref
    val ! :		'a ref -> 'a
    val := :		'a ref * 'a -> unit
    val :=: :		'a ref * 'a ref -> unit
    val exchange :	'a ref * 'a -> 'a

    val map: ('a -> 'a) -> 'a ref -> 'a ref
  end


structure Ref : REF


(*****************************************************************************
 * Bool
 *****************************************************************************)

signature BOOL =
  sig
    datatype bool = false | true
    type     t    = bool			(**)

    val not :		bool -> bool

    val toString :	bool -> string
(*MISSING
    val fromString : 	string -> bool option
    val scan :		(char,'a) StringCvt.reader -> (bool,'a) StringCvt.reader
*)
  end


structure Bool : BOOL



(*****************************************************************************
 * Pair
 *****************************************************************************)

signature PAIR =							(**)
  sig
    type ('a,'b) pair = 'a * 'b
    type ('a,'b) t    = ('a,'b) pair

    val fst :		('a,'b) pair -> 'a
    val snd :		('a,'b) pair -> 'b

    val app :		('a -> unit) * ('b -> unit) -> ('a,'b) pair -> unit
    val appFst :	('a -> unit) -> ('a,'b) pair -> unit
    val appSnd :	('b -> unit) -> ('a,'b) pair -> unit
    val map :		('a -> 'c) * ('b -> 'd) -> ('a,'b) pair -> ('c,'d) pair
    val mapFst :	('a -> 'c) -> ('a,'b) pair -> ('c,'b) pair
    val mapSnd :	('b -> 'c) -> ('a,'b) pair -> ('a,'c) pair
  end


structure Pair : PAIR							(**)



(*****************************************************************************
 * Alt
 *****************************************************************************)

signature ALT =								(**)
  sig
    datatype ('a,'b) alt = FST of 'a | SND of 'b
    type     ('a,'b) t   = ('a,'b) alt

    exception Alt

    val isFst :		('a,'b) alt -> bool
    val isSnd :		('a,'b) alt -> bool
    val fst :		('a,'b) alt -> 'a		(* [Alt] *)
    val snd :		('a,'b) alt -> 'b		(* [Alt] *)
    val getFst :	('a,'b) alt * 'a -> 'a
    val getSnd :	('a,'b) alt * 'b -> 'b

    val app :		('a -> unit) * ('b -> unit) -> ('a,'b) alt -> unit
    val appFst :	('a -> unit) -> ('a,'b) alt -> unit
    val appSnd :	('b -> unit) -> ('a,'b) alt -> unit
    val map :		('a -> 'c) * ('b -> 'd) -> ('a,'b) alt -> ('c,'d) alt
    val mapFst :	('a -> 'c) -> ('a,'b) alt -> ('c,'b) alt
    val mapSnd :	('b -> 'c) -> ('a,'b) alt -> ('a,'c) alt
  end


structure Alt : ALT							(**)



(*****************************************************************************
 * Option
 *****************************************************************************)

signature OPTION =
  sig
    datatype 'a option = NONE | SOME of 'a
    type     'a t      = 'a option					(**)

    exception Option

    val getOpt :	'a option * 'a -> 'a
    val isSome :	'a option -> bool
    val isNone :	'a option -> bool				(**)
    val valOf :		'a option -> 'a
    val filter :	('a -> bool) -> 'a -> 'a option
    val join :		'a option option -> 'a option
    val app :		('a -> unit) -> 'a option -> unit		(**)
    val map :		('a -> 'b) -> 'a option -> 'b option
    val mapPartial :	('a -> 'b option) -> 'a option -> 'b option
    val fold :		('a * 'b -> 'b) -> 'b -> 'a option -> 'b	(**)
    val compose :	('a -> 'c) * ('b -> 'a option) -> 'b -> 'c option
    val composePartial:	('a -> 'c option) * ('b -> 'a option) -> 'b -> 'c option
  end


structure Option : OPTION



(*****************************************************************************
 * List
 *****************************************************************************)

signature LIST =
  sig
    datatype 'a list = nil | op:: of 'a * 'a list
    type     'a t    = 'a list						(**)

    exception Empty

    val null :		'a list -> bool
    val length :	'a list -> int

    val hd :		'a list -> 'a
    val tl :		'a list -> 'a list
    val last :		'a list -> 'a
    val getItem :	'a list -> ('a * 'a list) option
    val nth :		'a list * int -> 'a
    val sub :		'a list * int -> 'a				(**)
    val take :		'a list * int -> 'a list
    val drop :		'a list * int -> 'a list

    val rev :		'a list -> 'a list
    val op @ :		'a list * 'a list -> 'a list
    val append :	'a list * 'a list -> 'a list			(**)
    val revAppend :	'a list * 'a list -> 'a list
    val concat :	'a list list -> 'a list

    val app :		('a -> unit) -> 'a list -> unit
    val appr :		('a -> unit) -> 'a list -> unit			(**)
    val appi :		(int * 'a -> unit) -> 'a list -> unit		(**)
    val appri :		(int * 'a -> unit) -> 'a list -> unit		(**)
    val map :		('a -> 'b) -> 'a list -> 'b list
    val mapi :		(int * 'a -> 'b) -> 'a list -> 'b list		(**)
    val mapPartial :	('a -> 'b option) -> 'a list -> 'b list
    val find :		('a -> bool) -> 'a list -> 'a option
    val filter :	('a -> bool) -> 'a list -> 'a list
    val partition :	('a -> bool) -> 'a list -> 'a list * 'a list
    val foldl :		('a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val foldr :		('a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val foldli :	(int * 'a * 'b -> 'b) -> 'b -> 'a list -> 'b	(**)
    val foldri :	(int * 'a * 'b -> 'b) -> 'b -> 'a list -> 'b	(**)
    val all :		('a -> bool) -> 'a list -> bool
    val exists :	('a -> bool) -> 'a list -> bool

    val tabulate :	int * (int -> 'a) -> 'a list
  end


structure List : LIST



(*****************************************************************************
 * ListPair
 *****************************************************************************)

signature LIST_PAIR =
  sig
    val zip :		'a list * 'b list -> ('a * 'b) list
    val unzip :		('a * 'b) list -> 'a list * 'b list

    val app :		('a * 'b -> unit) -> 'a list * 'b list -> unit
    val map :		('a * 'b -> 'c) -> 'a list * 'b list -> 'c list
    val foldl :		('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
    val foldr :		('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
    val all :		('a * 'b -> bool) -> 'a list * 'b list -> bool
    val exists :	('a * 'b -> bool) -> 'a list * 'b list -> bool

    (**)
    val appr :		('a * 'b -> unit) -> 'a list * 'b list -> unit
    val appi :		(int * 'a * 'b -> unit) -> 'a list * 'b list -> unit
    val appri :		(int * 'a * 'b -> unit) -> 'a list * 'b list -> unit
    val mapi :		(int * 'a * 'b -> 'c) -> 'a list * 'b list -> 'c list
    val mapPartial :	('a * 'b -> 'c option) -> 'a list * 'b list -> 'c list
    val foldli :	(int * 'a * 'b * 'c -> 'c) -> 'c
						   -> 'a list * 'b list ->'c
    val foldri :	(int * 'a * 'b * 'c -> 'c) -> 'c
						   -> 'a list * 'b list -> 'c
    val find :		('a * 'b -> bool) -> 'a list * 'b list -> ('a*'b) option
  end


structure ListPair : LIST_PAIR



(*****************************************************************************
 * Char
 *****************************************************************************)

signature CHAR =
  sig
    eqtype char
    eqtype string
    type t = char					(**)

    val minChar :	char
    val maxChar :	char
    val maxOrd :	int

    val chr :		int -> char
    val ord :		char -> int

    val pred :		char -> char
    val succ :		char -> char

    val op < :		char * char -> bool
    val op <= :		char * char -> bool
    val op > :		char * char -> bool
    val op >= :		char * char -> bool
    val compare :	char * char -> order

    val contains :	string -> char -> bool
    val notContains :	string -> char -> bool

    val toLower :	char -> char
    val toUpper :	char -> char

    val isLower :	char -> bool
    val isUpper :	char -> bool
    val isAlpha :	char -> bool
    val isAlphaNum :	char -> bool
    val isDigit :	char -> bool
    val isHexDigit :	char -> bool
    val isPunct :	char -> bool
    val isPrint :	char -> bool
    val isGraph :	char -> bool
    val isSpace :	char -> bool
    val isCntrl :	char -> bool
    val isAscii :	char -> bool

    val toWide :	char -> __pervasive.WideChar.char		(**)
    val fromWide :	__pervasive.WideChar.char -> char		(**)

    val toString :	char -> string
    val toCString :	char -> string
(*MISSING
    val fromString :	string -> char option
    val fromCString :	string -> char option
    val scan :		(char,'a) StringCvt.reader -> (char,'a) StringCvt.reader
*)
  end


structure Char     : CHAR where type char   = char
			  where type string = string

structure WideChar : CHAR where type char   = char
			  where type string = string



(*****************************************************************************
 * String
 *****************************************************************************)

signature STRING =
  sig
    type string
    type t = string							(**)

    structure Char :	CHAR

    val maxSize :	int
    val maxLen :	int						(**)

    val size :		string -> int
    val length :	string -> int					(**)
    val str :		Char.char -> string
    val sub :		string * int -> Char.char
    val substring :	string * int * int -> string
    val extract :	string * int * int option -> string

    val op ^ :		string * string -> string
    val append :	string * string -> string			(**)
    val concat :	string list -> string
    val implode :	Char.char list -> string
    val explode :	string -> Char.char list
    val fromList :	Char.char list -> string			(**)
    val toList :	string -> Char.char list			(**)
    val tabulate :	int * (int -> Char.char) -> string		(**)

    val map :		(Char.char -> Char.char) -> string -> string
    val translate :	(Char.char -> string) -> string -> string
    val fields :	(Char.char -> bool) -> string -> string list
    val tokens :	(Char.char -> bool) -> string -> string list

    val op < :		string * string -> bool
    val op > :		string * string -> bool
    val op <= :		string * string -> bool
    val op >= :		string * string -> bool
    val compare :	string * string -> order
    val collate :	(Char.char * Char.char -> order) -> string * string
							 -> order
    val isPrefix :	string -> string -> bool
    val isSuffix :	string -> string -> bool			(**)

    val toWide :	string -> __pervasive.WideString.string		(**)
    val fromWide :	__pervasive.WideString.string -> string		(**)

    val toString :	string -> string
    val toCString :	string -> string
(*MISSING
    val fromString :	string -> string option
    val fromCString :	string -> string option
*)
  end


structure String     : STRING where type string = string
			      where Char = Char

structure WideString : STRING where type string = string
			      where Char = WideChar



(*****************************************************************************
 * Substring
 *****************************************************************************)

signature SUBSTRING =
  sig
    structure String :	STRING

    type substring
    type t = substring							(**)

    val base :		substring -> String.string * int * int
    val string :	substring -> String.string
    val substring :	String.string * int * int -> substring
    val extract :	String.string * int * int option -> substring

    val isEmpty :	substring -> bool
    val size :		substring -> int

    val all :		String.string -> substring
    val getc :		substring -> (String.Char.char * substring) option
    val first :		substring -> String.Char.char option
    val triml :		int -> substring -> substring
    val trimr :		int -> substring -> substring
    val sub :		substring * int -> char
    val slice :		substring * int * int option -> substring
    val concat :	substring list -> String.string
    val explode :	substring -> String.Char.char list

    val isPrefix :	String.string -> substring -> bool
    val compare :	substring * substring -> order
    val collate :	(String.Char.char * String.Char.char -> order) ->
					substring * substring -> order

    val splitl :	(String.Char.char -> bool) -> substring
						   -> substring * substring
    val splitr :	(String.Char.char -> bool) -> substring
						   -> substring * substring
    val splitAt :	substring * int -> substring * substring
    val dropl :		(String.Char.char -> bool) -> substring -> substring
    val dropr :		(String.Char.char -> bool) -> substring -> substring
    val takel :		(String.Char.char -> bool) -> substring -> substring
    val taker :		(String.Char.char -> bool) -> substring -> substring
    val position :	String.string -> substring -> substring * substring
    val span :		substring * substring -> substring
    val translate :	(String.Char.char -> String.string) -> substring
							   -> String.string
    val tokens :	(String.Char.char -> bool) -> substring -> substring list
    val fields :	(String.Char.char -> bool) -> substring -> substring list

    val app :		(String.Char.char -> unit) -> substring -> unit
    val foldl :		(String.Char.char * 'a -> 'a) -> 'a -> substring -> 'a
    val foldr :		(String.Char.char * 'a -> 'a) -> 'a -> substring -> 'a
  end


(*MISSING
structure Substring : SUBSTRING where String = String
*)



(*****************************************************************************
 * StringCvt
 *****************************************************************************)

signature STRING_CVT =
  sig
    datatype radix      = BIN | OCT | DEC | HEX
    datatype realfmt    = EXACT
			| SCI of int option
			| FIX of int option
			| GEN of int option

    type ('a,'b) reader = 'b -> ('a * 'b) option
    type cs

(*MISSING
    val padLeft :	char -> int -> string -> string
    val padRight :	char -> int -> string -> string
    val splitl :	(char -> bool) -> (char,'a) reader ->'a -> (string * 'a)
    val takel :		(char -> bool) -> (char,'a) reader -> 'a -> string
*)
    val dropl :		(char -> bool) -> (char,'a) reader -> 'a -> 'a
    val skipWS :	(char,'a) reader -> 'a -> 'a
    val scanString:	((char,cs) reader -> ('a,cs) reader) -> string
							     -> 'a option
  end


structure StringCvt : STRING_CVT



(*****************************************************************************
 * Int
 *****************************************************************************)

signature INTEGER =
  sig
    eqtype int
    type t = int							(**)

    val minInt :	int option
    val maxInt :	int option
    val precision :	__pervasive.Int.int option

    val toInt :		int -> __pervasive.Int.int
    val fromInt :	__pervasive.Int.int -> int
    val toLarge :	int -> __pervasive.LargeInt.int
    val fromLarge :	__pervasive.LargeInt.int -> int

    val ~ :		int -> int
    val op + :		int * int -> int
    val op - :		int * int -> int
    val op * :		int * int -> int
    val op div :	int * int -> int
    val op mod :	int * int -> int
    val op quot :	int * int -> int
    val op rem :	int * int -> int

    val op < :		int * int -> bool
    val op > :		int * int -> bool
    val op <= :		int * int -> bool
    val op >= :		int * int -> bool
    val compare :	int * int -> order

    val abs :		int -> int
    val min :		int * int -> int
    val max :		int * int -> int
    val sign :		int -> __pervasive.Int.int
    val sameSign :	int * int -> bool

    val toString :	int -> string
    val fromString:	string -> int option
(*MISSING
    val fmt :		StringCvt.radix -> int -> string
*)
    val scan :		StringCvt.radix -> (char,'a) StringCvt.reader -> 'a
					-> (int * 'a) option
  end


structure Int      : INTEGER where type int = int
structure LargeInt : INTEGER where type int = int
structure Position : INTEGER = Int



(*****************************************************************************
 * Word
 *****************************************************************************)

signature WORD =
  sig
    eqtype word
    type t = word							(**)

    val wordSize :	int

    val toLargeWord :	word -> __pervasive.LargeWord.word
(*MISSING
    val toLargeWordX :	word -> __pervasive.LargeWord.word
*)
    val fromLargeWord :	__pervasive.LargeWord.word -> word
    val toLargeInt :	word -> __pervasive.LargeInt.int
    val toLargeIntX :	word -> __pervasive.LargeInt.int
    val fromLargeInt :	__pervasive.LargeInt.int -> word
    val toInt :		word -> __pervasive.Int.int
    val toIntX :	word -> __pervasive.Int.int
    val fromInt :	__pervasive.Int.int -> word

    val notb :		word -> word
    val orb :		word * word -> word
    val xorb :		word * word -> word
    val andb :		word * word -> word
    val << :		word * word -> word
    val >> :		word * word -> word
    val ~>> :		word * word -> word

    val + :		word * word -> word
    val - :		word * word -> word
    val * :		word * word -> word
    val div :		word * word -> word
    val mod :		word * word -> word

(*MISSING
    val op > :		word * word -> bool
    val op < :		word * word -> bool
    val op >= :		word * word -> bool
    val op <= :		word * word -> bool
    val compare :	word * word -> order

    val min :		word * word -> word
    val max :		word * word -> word
*)

    val toString :	word -> string
(*MISSING
    val fromString :	string -> word option
    val fmt :		StringCvt.radix -> word -> string
*)
    val scan :		StringCvt.radix -> (char,'a) StringCvt.reader
					-> (word,'a) StringCvt.reader
  end


structure Word      : WORD where type word = word
structure LargeWord : WORD where type word = word



(*****************************************************************************
 * IEEEReal
 *****************************************************************************)

signature IEEE_REAL =
  sig
     exception Unordered

     datatype real_order	= LESS | EQUAL | GREATER | UNORDERED
     datatype nan_mode		= QUIET | SIGNALLING
     datatype float_class	= NAN of nan_mode
				| INF
				| ZERO
				| NORMAL
				| SUBNORMAL
     datatype rounding_mode	= TO_NEAREST
				| TO_NEGINF
				| TO_POSINF
				| TO_ZERO

(*MISSING
     val setRoundingMode :	rounding_mode -> unit
     val getRoundingMode :	unit -> rounding_mode
*)

     type decimal_approx	= { kind : float_class, sign : bool,
				    digits : int list,  exp : int }

(*MISSING
     val toString :		decimal_approx -> string
     val fromString :		string -> decimal_approx option
*)
  end


structure IEEEReal : IEEE_REAL



(*****************************************************************************
 * Math
 *****************************************************************************)

signature MATH =
  sig
    eqtype real

    val e :		real
    val pi :		real

    val sqrt :		real -> real
    val exp :		real -> real
    val pow :		real * real -> real
    val ln :		real -> real
    val log10 :		real -> real

    val sin :		real -> real
    val cos :		real -> real
    val tan :		real -> real
    val asin :		real -> real
    val acos :		real -> real
    val atan :		real -> real
    val atan2 :		real * real -> real
    val sinh :		real -> real
    val cosh :		real -> real
    val tanh :		real -> real
    val asinh :		real -> real					(**)
    val acosh :		real -> real					(**)
    val atanh :		real -> real					(**)
  end


structure Math : MATH where type real = real



(*****************************************************************************
 * Real
 *****************************************************************************)

signature REAL =
  sig
    eqtype real
    type t = real							(**)

    structure Math :	MATH where type real = real

(*MISSING
    val radix :		int
    val precision :	int
    val maxFinite :	real
    val minPos :	real
    val minNormalPos :	real
*)

    val posInf :	real
    val negInf :	real

    val ~ :		real -> real
    val op + :		real * real -> real
    val op - :		real * real -> real
    val op * :		real * real -> real
    val op / :		real * real -> real
    val rem :		real * real -> real
    val *+ :		real * real * real -> real
    val *- :		real * real * real -> real

    val abs :		real -> real
    val min :		real * real -> real
    val max :		real * real -> real
    val sign :		real -> int
    val signBit :	real -> bool
    val sameSign :	real * real -> bool
    val copySign :	real * real -> real

    val op < :		real * real -> bool
    val op > :		real * real -> bool
    val op <= :		real * real -> bool
    val op >= :		real * real -> bool
    val compare :	real * real -> order
    val compareReal :	real * real -> IEEEReal.real_order

    val == :		real * real -> bool
    val != :		real * real -> bool
    val ?= :		real * real -> bool
(*MISSING
    val unordered :	real * real -> bool

    val isFinite :	real -> bool
    val isNan :		real -> bool
    val isNormal :	real -> bool
    val class :		real -> IEEEReal.float_class
*)

    val ceil :		real -> Int.int
    val floor :		real -> Int.int
    val trunc :		real -> Int.int
    val round :		real -> Int.int
    val realFloor :	real -> real
    val realCeil :	real -> real
    val realTrunc :	real -> real
    val realRound :	real -> real					(**)

(*MISSING
    val nextAfter :	real * real -> real
    val checkFloat :	real -> real
*)

(*MISSING
    val toManExp :	real -> {exp:int, man:real}
    val fromManExp :	{exp:int, man:real} -> real
    val split :		real -> {frac:real, whole:real}
    val realMod :	real -> real
*)

    val toInt :		IEEEReal.rounding_mode -> real -> int
    val toLargeInt :	IEEEReal.rounding_mode ->
			    real -> __pervasive.LargeInt.int
    val fromInt :	int -> real
    val fromLargeInt :	__pervasive.LargeInt.int -> real
    val toLarge :	real -> __pervasive.LargeReal.real
    val fromLarge :	IEEEReal.rounding_mode ->
			    __pervasive.LargeReal.real -> real
(*MISSING
    val toDecimal :	real -> IEEEReal.decimal_approx
    val fromDecimal :	IEEEReal.decimal_approx -> real
*)

    val toString :	real -> string
    val fromString :	string -> real option
(*MISSING
    val fmt :		StringCvt.realfmt -> real -> string
*)
    val scan :		(char,'a) StringCvt.reader -> (real,'a) StringCvt.reader
  end


structure Real      : REAL where type real = real
structure LargeReal : REAL where type real = real



(*****************************************************************************
 * Vector
 *****************************************************************************)

signature VECTOR =
  sig
    eqtype 'a vector
    type   'a t = 'a vector						(**)

    val maxLen :	int

    val toList :	'a vector -> 'a list				(**)
    val fromList :	'a list -> 'a vector
    val tabulate :	int * (int -> 'a) -> 'a vector

    val length :	'a vector -> int
    val sub :		'a vector * int -> 'a
    val replace :	'a vector * int * 'a -> 'a vector		(**)
    val extract :	'a vector * int * int option -> 'a vector
    val append :	'a vector * 'a vector -> 'a vector		(**)
    val concat :	'a vector list -> 'a vector
    val rev :		'a vector -> 'a vector				(**)

    val app :		('a -> unit) -> 'a vector -> unit
    val appr :		('a -> unit) -> 'a vector -> unit		(**)
    val map :		('a -> 'b) -> 'a vector -> 'b vector
    val foldl :		('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
    val foldr :		('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
    val appi :		(int * 'a -> unit) -> 'a vector * int * int option
					   -> unit
    val appri :		(int * 'a -> unit) -> 'a vector * int * int option (**)
					   -> unit
    val mapi :		(int * 'a -> 'b) -> 'a vector * int * int option
					 -> 'b vector
    val foldli :	(int * 'a * 'b -> 'b) -> 'b
				       -> 'a vector * int * int option -> 'b
    val foldri :	(int * 'a * 'b -> 'b) -> 'b
				       -> 'a vector * int * int option -> 'b

    val all :		('a -> bool) -> 'a vector -> bool		(**)
    val exists :	('a -> bool) -> 'a vector -> bool		(**)
    val find :		('a -> bool) -> 'a vector -> 'a option		(**)
  end


structure Vector : VECTOR where type 'a vector = 'a vector



(*****************************************************************************
 * VectorPair
 *****************************************************************************)

signature VECTOR_PAIR =							(**)
  sig
    val zip :		'a vector * 'b vector -> ('a * 'b) vector
    val unzip :		('a * 'b) vector -> 'a vector * 'b vector

    val app :		('a * 'b -> unit) -> 'a vector * 'b vector -> unit
    val appr :		('a * 'b -> unit) -> 'a vector * 'b vector -> unit
    val map :		('a * 'b -> 'c) -> 'a vector * 'b vector -> 'c vector
    val foldl :		('a * 'b * 'c ->'c) -> 'c -> 'a vector * 'b vector -> 'c
    val foldr :		('a * 'b * 'c ->'c) -> 'c -> 'a vector * 'b vector -> 'c
    val all :		('a * 'b -> bool) -> 'a vector * 'b vector -> bool
    val exists :	('a * 'b -> bool) -> 'a vector * 'b vector -> bool

    val appi :		(int * 'a * 'b -> unit) ->
			   'a vector * 'b vector * int * int option -> unit
    val appri :		(int * 'a * 'b -> unit) ->
			   'a vector * 'b vector * int * int option -> unit
    val mapi :		(int * 'a * 'b -> 'c) ->
			   'a vector * 'b vector * int * int option -> 'c vector
    val foldli :	(int * 'a * 'b * 'c -> 'c) -> 'c ->
			   'a vector * 'b vector * int * int option -> 'c
    val foldri :	(int * 'a * 'b * 'c -> 'c) -> 'c ->
			   'a vector * 'b vector * int * int option -> 'c

    val find :		('a * 'b -> bool) -> 'a vector * 'b vector
					  -> ('a * 'b) option
  end


structure VectorPair : VECTOR_PAIR					(**)


(*****************************************************************************
 * Array
 *****************************************************************************)

signature ARRAY =
  sig
    type 'a array
    type 'a vector
    type 'a t = 'a array						(**)

    val maxLen :	int

    val array :		int * 'a -> 'a array
    val new :		int * 'a -> 'a array				(**)
    val toList :	'a array -> 'a list				(**)
    val fromList :	'a list -> 'a array
    val toVector :	'a array -> 'a vector				(**)
    val fromVector :	'a vector -> 'a array				(**)
    val tabulate :	int * (int -> 'a) -> 'a array

    val length :	'a array -> int
    val sub :		'a array * int -> 'a
    val update :	'a array * int * 'a -> unit
    val swap :		'a array * int * int -> unit			(**)
    val reverse :	'a array -> unit				(**)
    val extract :	'a array * int * int option -> 'a vector
    val copy :		{di:int, dst:'a array, len:int option,
			 si:int, src:'a array} -> unit
    val copyVec :	{di:int, dst:'a array, len:int option,
			 si:int, src:'a vector} -> unit

    val app :		('a -> unit) -> 'a array -> unit
    val appr :		('a -> unit) -> 'a array -> unit		(**)
    val modify :	('a -> 'a) -> 'a array -> unit
    val foldl :		('a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val foldr :		('a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val appi :		(int * 'a -> unit)
					-> 'a array * int * int option -> unit
    val appri :		(int * 'a -> unit) -> 'a array * int * int option  (**)
					   -> unit
    val modifyi :	(int * 'a -> 'a) -> 'a array * int * int option -> unit
    val foldli :	(int * 'a * 'b -> 'b) -> 'b
					-> 'a array * int * int option -> 'b
    val foldri :	(int * 'a * 'b -> 'b) -> 'b
					-> 'a array * int * int option -> 'b

    val all :		('a -> bool) -> 'a array -> bool		(**)
    val exists :	('a -> bool) -> 'a array -> bool		(**)
    val find :		('a -> bool) -> 'a array -> 'a option		(**)
  end


structure Array : ARRAY where type 'a array  = 'a array
			where type 'a vector = 'a vector



(*****************************************************************************
 * Time
 *****************************************************************************)

signature TIME =
  sig
    eqtype time
    type t = time							(**)

    exception Time

    val zeroTime :		time
(*MISSING
    val now :			unit -> time
*)

    val fromReal :		LargeReal.real -> time
    val toReal :		time -> LargeReal.real
    val toSeconds :		time -> LargeInt.int
    val toMilliseconds :	time -> LargeInt.int
    val toMicroseconds :	time -> LargeInt.int
    val fromSeconds :		LargeInt.int -> time
    val fromMilliseconds :	LargeInt.int -> time
    val fromMicroseconds :	LargeInt.int -> time

    val op + :			time * time -> time
    val op - :			time * time -> time

    val op < :			time * time -> bool
    val op > :			time * time -> bool
    val op <= :			time * time -> bool
    val op >= :			time * time -> bool
    val compare :		time * time -> order

(*MISSING
    val toString :	time -> string
    val fromString :	string -> time option
    val fmt :		int -> time -> string
    val scan :		(char,'a) StringCvt.reader -> 'a -> (time * 'a) option
*)
  end


structure Time : TIME (* where type time = __pervasive.Time.time *)



(*****************************************************************************
 * Cell
 *****************************************************************************)

signature CELL =
  sig
    __eqeqtype 'a cell

    val cell :		'a -> 'a cell
    val exchange :	'a cell * 'a -> 'a
  end


structure Cell : CELL



(*****************************************************************************
 * Hole
 *****************************************************************************)

signature HOLE =
  sig
    exception Hole
    exception Cyclic

    val hole :		unit -> 'a
    val future :	'a -> 'a

    val fill :		'a * 'a  -> unit	(* Hole, Cyclic *)
    val fail :		'a * exn -> unit	(* Hole *)

    val isHole :	'a -> bool
    val isFailed :	'a -> bool
  end


structure Hole : HOLE



(*****************************************************************************
 * Future
 *****************************************************************************)

signature FUTURE =
  sig
    exception Future of exn
    exception Cyclic

    val concur :	(unit -> 'a) -> 'a
    val byneed :	(unit -> 'a) -> 'a
    val alarm :		Time.time -> unit

    val await :		'a -> 'a
    val awaitOne :	'a * 'b -> 'a

    val isFuture :	'a -> bool
    val isFailed :	'a -> bool
  end


structure Future : FUTURE



(*****************************************************************************
 * Promise
 *****************************************************************************)

signature PROMISE =
  sig
    type 'a promise

    exception Promise

    val promise :	unit -> 'a promise
    val future :	'a promise -> 'a

    val fulfill :	'a promise * 'a  -> unit	(* Promise, Cyclic *)
    val fail :		'a promise * exn -> unit	(* Promise *)
  end


structure Promise : PROMISE (* where type promise = __pervasive.Promise.promise *)



(*****************************************************************************
 * Thread
 *****************************************************************************)

signature THREAD =
  sig
    type thread
    datatype state = RUNNABLE | BLOCKED | TERMINATED

    exception Terminate

    val spawn :		(unit -> 'a) -> thread * 'a
    val current :	unit -> thread
    val state :		thread -> state

    val yield :		thread -> unit
    val sleep :		Time.time -> unit

    val raiseIn :	thread * exn -> unit
    val terminate :	thread -> unit

    val suspend :	thread -> unit
    val resume :	thread -> unit
    val isSuspended :	thread -> bool
  end


structure Thread : THREAD



(*****************************************************************************
 * Top-level, part 2
 *****************************************************************************)

exception Bind		= General.Bind
exception Chr		= General.Chr
exception Div		= General.Div
exception Domain	= General.Domain
exception Fail		= General.Fail
exception Match		= General.Match
exception Overflow	= General.Overflow
exception Size		= General.Size
exception Span		= General.Span
exception Subscript	= General.Subscript
exception Alt		= Alt.Alt				(**)
exception Option	= Option.Option
exception Empty		= List.Empty

val exnName :		exn -> string
val exnMessage :	exn -> string
val ! :			'a ref -> 'a
val op := :		'a ref * 'a -> unit
val op :=: :		'a ref * 'a ref -> unit			(**)
val op flip :		('a * 'b -> 'c) -> 'b * 'a -> 'c
val op o :		('b -> 'c) * ('a -> 'b) -> 'a -> 'c
val before :		'a * unit -> 'a
val ignore :		'a -> unit

val not	:		bool -> bool

val fst	:		('a,'b) alt -> 'a			(**)
val snd	:		('a,'b) alt -> 'b			(**)
val isFst :		('a,'b) alt -> bool			(**)
val isSnd :		('a,'b) alt -> bool			(**)

val getOpt :		'a option * 'a -> 'a
val isSome :		'a option -> bool
val isNone :		'a option -> bool			(**)
val valOf :		'a option -> 'a

val null :		'a list -> bool
val length :		'a list -> int
val hd :		'a list -> 'a
val tl :		'a list -> 'a list
val rev :		'a list -> 'a list
val op @ :		'a list * 'a list -> 'a list
val app :		('a -> unit) -> 'a list -> unit
val map :		('a -> 'b) -> 'a list -> 'b list
val foldl :		('a * 'b -> 'b) -> 'b -> 'a list -> 'b
val foldr :		('a * 'b -> 'b) -> 'b -> 'a list -> 'b

val chr :		int -> char
val ord :		char -> int

val str :		Char.char -> string
val size :		string -> int
val op ^ :		string * string -> string
val concat :		string list -> string
val explode :		string -> Char.char list
val implode :		Char.char list -> string
val substring :		string * int * int -> string

val vector :		'a list -> 'a vector

val real :		int -> real
val ceil :		real -> Int.int
val floor :		real -> Int.int
val trunc :		real -> Int.int
val round :		real -> Int.int

(*MISSING
val use :		string -> unit
*)

exception Future	= Future.Future				(**)
exception Cyclic	= Future.Cyclic				(**)

val concur :		(unit -> 'a) -> 'a			(**)
val byneed :		(unit -> 'a) -> 'a			(**)


end (* sig *)

end (* local *)
