(*****************************************************************************
 * General
 *****************************************************************************)

signature GENERAL =
  sig
    include GENERAL

    val :=: :		'a ref * 'a ref -> unit
    val id :		'a -> 'a
    val const :		'a -> 'b -> 'a
    val curry :		('a * 'b -> 'c) -> ('a -> 'b -> 'c)
    val uncurry :	('a -> 'b -> 'c) -> ('a * 'b -> 'c)
    val flip :		('a * 'b -> 'c) -> ('b * 'a -> 'c)
    val inverse :	order -> order
  end


structure General : GENERAL =
  struct
    open General

    fun op :=: (r1 as ref x1, r2 as ref x2)	= (r1 := x2 ; r2 := x1)
    fun id x					= x
    fun const x y				= x
    fun curry f x y				= f(x,y)
    fun uncurry f (x,y)				= f x y
    fun flip f (x,y)				= f(y,x)
    fun inverse LESS				= GREATER
      | inverse EQUAL				= EQUAL
      | inverse GREATER				= LESS
  end



(*****************************************************************************
 * Ref
 *****************************************************************************)

signature REF =
  sig
    datatype ref = datatype ref
    type  'a t   = 'a ref

    val new :		'a -> 'a ref
    val ! :		'a ref -> 'a
    val := :		'a ref * 'a -> unit
    val :=: :		'a ref * 'a ref -> unit
    val exchange :	'a ref * 'a -> 'a

    val equal :		'a ref * 'a ref -> bool
    val map :		('a -> 'a) -> 'a ref -> 'a ref
  end


structure Ref : REF =
  struct
    datatype ref = datatype ref
    type  'a t   = 'a ref

    val new				= ref
    val !				= General.!
    val op :=				= General.:=
    val op :=:				= General.:=:

    fun exchange(r as ref x1, x2)	= (r := x2 ; x1)

    val equal				= op =
    fun map f (r as ref x)		= ref(f x)
  end



(*****************************************************************************
 * Bool
 *****************************************************************************)

signature BOOL =
  sig
    include BOOL

    type t = bool
    val equal :   bool * bool -> bool
    val compare : bool * bool -> order
  end


structure Bool : BOOL =
  struct
    open Bool

    type t			= bool
    val equal			= op =
    fun compare(false,true)	= LESS
      | compare(true,false)	= GREATER
      | compare _		= EQUAL
  end



(*****************************************************************************
 * Option
 *****************************************************************************)

signature OPTION =
  sig
    include OPTION

    type 'a t = 'a option

    val isNone :	'a option -> bool

    val equal :		('a * 'a -> bool) -> 'a option * 'a option -> bool
    val compare :	('a * 'a -> order) -> 'a option * 'a option -> order

    val app :		('a -> unit) -> 'a option -> unit
    val fold :		('a * 'b -> 'b) -> 'b -> 'a option -> 'b
  end


structure Option : OPTION =
  struct
    open Option

    type 'a t				= 'a option

    fun isNone NONE			= true
      | isNone  _			= false

    fun equal eq (NONE,   NONE)		= true
      | equal eq (SOME x, SOME y)	= eq(x,y)
      | equal eq _			= false

    fun compare cmp (NONE,   NONE)	= EQUAL
      | compare cmp (NONE,   SOME _)	= LESS
      | compare cmp (SOME _, NONE)	= GREATER
      | compare cmp (SOME x, SOME y)	= cmp(x,y)

    fun app f  NONE			= ()
      | app f (SOME x)			= f x

    fun fold f b  NONE			= b
      | fold f b (SOME a)		= f(a,b)
  end



(*****************************************************************************
 * Pair
 *****************************************************************************)

signature PAIR =
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

    val equal :		('a * 'a -> bool) * ('b * 'b -> bool) ->
			    ('a,'b) pair * ('a,'b) pair -> bool
    val compare :	('a * 'a -> order) * ('b * 'b -> order) ->
			    ('a,'b) pair * ('a,'b) pair -> order
  end


structure Pair : PAIR =
  struct
    type ('a,'b) pair	= 'a * 'b
    type ('a,'b) t	= ('a,'b) pair

    fun fst(x,y)	= x
    fun snd(x,y)	= y

    fun app (f,g) (x,y)	= (f x ; g y)
    fun appFst f  (x,y)	= f x
    fun appSnd f  (x,y)	= f y

    fun map (f,g) (x,y)	= (f x, g y)
    fun mapFst f  (x,y)	= (f x, y)
    fun mapSnd f  (x,y)	= (x, f y)

    fun equal (equalX,equalY) ((x1,y1), (x2,y2)) =
	equalX(x1,x2) andalso equalY(y1,y2)

    fun compare (compareX,compareY) ((x1,y1), (x2,y2)) =
	case compareX(x1,x2)
	 of EQUAL => compareY(y1,y2)
	  | other => other
  end



(*****************************************************************************
 * Alt
 *****************************************************************************)

signature ALT =
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

    val equal :		('a * 'a -> bool) * ('b * 'b -> bool) ->
			    ('a,'b) alt * ('a,'b) alt -> bool
    val compare :	('a * 'a -> order) * ('b * 'b -> order) ->
			    ('a,'b) alt * ('a,'b) alt -> order
  end


structure Alt : ALT =
  struct
    datatype ('a,'b) alt = FST of 'a | SND of 'b
    type     ('a,'b) t   = ('a,'b) alt

    exception Alt

    fun isFst(FST _)		= true
      | isFst(SND _)		= false
    fun isSnd(FST _)		= false
      | isSnd(SND _)		= true

    fun fst(FST x)		= x
      | fst(SND x)		= raise Alt
    fun snd(FST x)		= raise Alt
      | snd(SND x)		= x

    fun getFst(FST x, _)	= x
      | getFst(SND _, x)	= x
    fun getSnd(FST _, x)	= x
      | getSnd(SND x, _)	= x

    fun app (f,g) (FST x)	= f x
      | app (f,g) (SND x)	= g x
    fun appFst f (FST x)	= f x
      | appFst f (SND x)	= ()
    fun appSnd f (FST x)	= ()
      | appSnd f (SND x)	= f x

    fun map (f,g) (FST x)	= FST(f x)
      | map (f,g) (SND x)	= SND(g x)
    fun mapFst f (FST x)	= FST(f x)
      | mapFst f (SND x)	= SND x
    fun mapSnd f (FST x)	= FST x
      | mapSnd f (SND x)	= SND(f x)

    fun equal (equalX,equalY) =
	fn (FST x1, FST x2)	=> equalX(x1,x2)
	 | (SND y1, SND y2)	=> equalY(y1,y2)
	 | _			=> false

    fun compare (compareX,compareY) =
	fn (FST x1, FST x2)	=> compareX(x1,x2)
	 | (SND y1, SND y2)	=> compareY(y1,y2)
	 | (FST _,  SND _ )	=> LESS
	 | (SND _,  FST _ )	=> GREATER
  end



(*****************************************************************************
 * List
 *****************************************************************************)

signature LIST =
  sig
    include LIST

    type 'a t = 'a list

    val append :	'a list * 'a list -> 'a list
    val sub :		'a list * int -> 'a

    val appr :		('a -> unit) -> 'a list -> unit
    val appi :		(int * 'a -> unit) -> 'a list -> unit
    val appri :		(int * 'a -> unit) -> 'a list -> unit
    val mapr :		('a -> 'b) -> 'a list -> 'b list
    val mapi :		(int * 'a -> 'b) -> 'a list -> 'b list
    val foldli :	(int * 'a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val foldri :	(int * 'a * 'b -> 'b) -> 'b -> 'a list -> 'b

    val contains :	''a list -> ''a -> bool
    val notContains :	''a list -> ''a -> bool

    val equal :		('a * 'a -> bool) -> 'a list * 'a list -> bool
    val compare :	('a * 'a -> order) -> 'a list * 'a list -> order

    val isSorted :	('a * 'a -> order) -> 'a list -> bool
    val sort :		('a * 'a -> order) -> 'a list -> 'a list
  end


structure List : LIST =
  struct
    open List

    type 'a t			= 'a list

    val append			= op @
    val sub			= nth

    fun appr  f   xs		= appr'(f,xs)
    and appr'(f,  nil )		= ()
      | appr'(f, x::xs)		= (appr'(f,xs) ; f x)

    fun foldli f y xs		= foldli'(f,y,0,xs)
    and foldli'(f, y, i,  nil )	= y
      | foldli'(f, y, i, x::xs)	= foldli'(f, f(i,x,y), i+1, xs)

    fun foldri  f  y xs		= foldri'(f,y,0,xs)
    and foldri'(f, y, i,  nil )	= y
      | foldri'(f, y, i, x::xs)	= f(i, x, foldri'(f, y, i+1, xs))

    fun mapr  f   xs		= mapr'(f,xs)
    and mapr'(f,  nil )		= nil
      | mapr'(f, x::xs)		= let val xs' = mapr'(f,xs) in f x :: xs' end

    fun mapi  f xs		= mapi'(f,0,xs)
    and mapi'(f, i,  nil )	= nil
      | mapi'(f, i, x::xs)	= f(i,x) :: mapi'(f, i+1, xs)

    fun appi  f xs		= appi'(f,0,xs)
    and appi'(f, i,  nil )	= ()
      | appi'(f, i, x::xs)	= (f(i,x) ; appi'(f, i+1, xs))

    fun appri  f xs		= appri'(f,0,xs)
    and appri'(f, i,  nil )	= ()
      | appri'(f, i, x::xs)	= (appri'(f, i+1, xs) ; f(i,x))

    fun contains xs y		= contains'(y, xs)
    and contains'(y, nil)	= false
      | contains'(y, x::xs)	= y = x orelse contains'(y,xs)

    fun notContains xs y	= Bool.not(contains'(y,xs))

    fun equal  eq (xs,ys)	= equal'(eq,xs,ys)
    and equal'(eq, [], [])	= true
      | equal'(eq, x::xs,y::ys)	= eq(x,y) andalso equal'(eq,xs,ys)
      | equal' _		= false

    fun compare  cmp (xs,ys)	= compare'(cmp,xs,ys)
    and compare'(cmp, [], [])	= EQUAL
      | compare'(cmp, [], _ )	= LESS
      | compare'(cmp, _,  [])	= GREATER
      | compare'(cmp, x::xs, y::ys) =
				  case cmp(x,y)
				    of EQUAL => compare'(cmp,xs,ys)
				     | other => other

    fun isSorted  compare xs	= isSorted'(compare, xs)
    and isSorted'(compare,(nil|_::nil))
				= true
      | isSorted'(compare, x1::(xs as x2::_))
				= compare(x1,x2) <> GREATER
					  andalso isSorted'(compare, xs)

    fun split nil		= (nil, nil)
      | split(xs as _::nil)	= (xs, nil)
      | split(x1::x2::xs)	= let val (xs1,xs2) = split xs
				  in (x1::xs1, x2::xs2) end
    fun sort compare		=
    let
	fun merge(xs, nil)	= xs
	  | merge(nil, ys)	= ys
	  | merge(xs as x::xs',
		  ys as y::ys')	= case compare(x,y)
				    of LESS    => x::merge(xs',ys)
				     | EQUAL   => x::y::merge(xs',ys')
				     | GREATER => y::merge(xs,ys')
	fun sort nil		= nil
	  | sort(xs as _::nil)	= xs
	  | sort xs		= let val (ys,zs) = split xs
				  in merge(sort ys, sort zs) end
    in
	sort
    end
  end



(*****************************************************************************
 * ListPair
 *****************************************************************************)

signature LIST_PAIR =
  sig
    include LIST_PAIR

    val mapPartial :	('a * 'b -> 'c option) -> 'a list * 'b list -> 'c list
    val appr :		('a * 'b -> unit) -> 'a list * 'b list -> unit
    val appi :		(int * 'a * 'b -> unit) -> 'a list * 'b list -> unit
    val appri :		(int * 'a * 'b -> unit) -> 'a list * 'b list -> unit
    val mapi :		(int * 'a * 'b -> 'c) -> 'a list * 'b list -> 'c list
    val foldli :	(int * 'a * 'b * 'c -> 'c) -> 'c
						   -> 'a list * 'b list ->'c
    val foldri :	(int * 'a * 'b * 'c -> 'c) -> 'c
						   -> 'a list * 'b list -> 'c
    val find :		('a * 'b -> bool) -> 'a list * 'b list -> ('a*'b) option
  end


structure ListPair : LIST_PAIR =
  struct
    open ListPair

    fun mapPartial  f (xs,ys)		= mapPartial'(f,xs,ys)
    and mapPartial'(f,  nil,    _  )	= nil
      | mapPartial'(f,   _,    nil )	= nil
      | mapPartial'(f, x::xs, y::ys)	= case f(x,y)
					    of NONE   => mapPartial'(f,xs,ys)
					     | SOME z => z::mapPartial'(f,xs,ys)

    fun appr  f (xs,ys)			= appr'(f,xs,ys)
    and appr'(f,  nil,    _  )		= ()
      | appr'(f,   _,    nil )		= ()
      | appr'(f, x::xs, y::ys)		= (appr'(f,xs,ys) ; f(x,y))

    fun appi  f (xs,ys)			= appi'(f,0,xs,ys)
    and appi'(f, i,  nil,    _  )	= ()
      | appi'(f, i,   _,    nil )	= ()
      | appi'(f, i, x::xs, y::ys)	= (f(i,x,y) ; appi'(f,i+1,xs,ys))

    fun appri  f (xs,ys)		= appri'(f,0,xs,ys)
    and appri'(f, i,  nil,    _  )	= ()
      | appri'(f, i,   _,    nil )	= ()
      | appri'(f, i, x::xs, y::ys)	= (appri'(f,i+1,xs,ys) ; f(i,x,y))

    fun mapi f (xs,ys)			= mapi'(f,0,xs,ys)
    and mapi'(f, i,  nil,    _  )	= nil
      | mapi'(f, i,   _,    nil )	= nil
      | mapi'(f, i, x::xs, y::ys)	= f(i,x,y) :: mapi'(f,i+1,xs,ys)

    fun foldli  f  z (xs,ys)		= foldli'(f,z,0,xs,ys)
    and foldli'(f, z, i,  nil,    _  )	= z
      | foldli'(f, z, i,   _,    nil )	= z
      | foldli'(f, z, i, x::xs, y::ys)	= foldli'(f, f(i,x,y,z), i+1, xs, ys)

    fun foldri  f  z (xs,ys)      	= foldri'(f,z,0,xs,ys)
    and foldri'(f, z, i,  nil,    _  ) 	= z
      | foldri'(f, z, i,   _,    nil ) 	= z
      | foldri'(f, z, i, x::xs, y::ys) 	= f(i, x, y, foldri'(f,z,i+1,xs,ys))

    fun find f (xs,ys)			= find'(f,xs,ys)
    and find'(f,  nil,    _  )		= NONE
      | find'(f,   _,    nil )		= NONE
      | find'(f, x::xs, y::ys)		= if f(x,y) then SOME(x,y)
						    else find'(f,xs,ys)
  end



(*****************************************************************************
 * Char
 *****************************************************************************)

local

structure Char =
  struct
    open Char

    type t		= char

    val equal		= op =
    val hash		= ord

    fun toWide c	= c
    fun fromWide c	= c
  end


structure WideChar = Char


in (* local *)


signature CHAR =
  sig
    include CHAR

    type t = char

    val equal :		char * char -> bool
    val hash :		char -> int

    val toWide :	char -> WideChar.char
    val fromWide :	WideChar.char -> char
  end


structure Char     : CHAR = Char
structure WideChar : CHAR = WideChar

end (* local *)



(*****************************************************************************
 * String
 *****************************************************************************)

local

structure String =
  struct
    open String

    type t		= string

    fun hash s =	(* hashpjw [Aho/Sethi/Ullman "Compilers"] *)
	let
	    open Word
	    infix << >> andb xorb

	    val n = String.size s

	    fun iter(i,h) =
		if i = n then h else
		let
		    val c  = Word.fromInt(Char.ord(String.sub(s,i)))
		    val h' = (h << 0w4) + c
		    val g  = h' andb 0wxf00000
		in
		    iter(Int.+(i,1), h' xorb g xorb (g >> 0w16))
		end
	in
	    Word.toInt(iter(0,0w0))
	end

    val equal		= op =

    fun toWide s	= s
    fun fromWide s	= s

    val maxLen		= maxSize
    val length		= size
    val append		= op ^
    val fromList	= implode
    val toList		= explode
    val tabulate	= implode o List.tabulate

    fun separate s' ss		= concat(separate'(s', ss))
    and separate'(s', nil)	= nil
      | separate'(s', s::nil)	= s::nil
      | separate'(s', s::ss)	= s::s'::separate'(s', ss)

    fun isSuffix s1 s2			= isSuffix'(s1,s2,size s1-1,size s2-1)
    and isSuffix'(s1,s2,i1,i2)		= i1 = ~1 orelse
					  i2 <> ~1 andalso
					  sub(s1,i1) = sub(s2,i1)
					  andalso isSuffix'(s1,s2,i1-1,i2-1)
  end


structure WideString = String


in (* local *)


signature STRING =
  sig
    include STRING (* where structure Char : CHAR *)

    type t = string

    val equal :		string * string -> bool
    val hash :		string -> int

    val maxLen :	int
    val length :	string -> int
    val append :	string * string -> string

    val toWide :	string -> WideString.string
    val fromWide :	WideString.string -> string

    val fromList :	Char.char list -> string
    val toList :	string -> Char.char list
    val tabulate :	int * (int -> Char.char) -> string

    val separate :	string -> string list -> string

    val isSuffix :	string -> string -> bool
  end


structure String     : STRING = String
structure WideString : STRING = WideString

end (* local *)


(*****************************************************************************
 * Substring
 *****************************************************************************)

signature SUBSTRING =
  sig
    include SUBSTRING (* where structure String : STRING *)

    type t = substring

    val equal :	substring * substring -> bool
    val hash :	substring -> int
  end

structure Substring =
  struct
    open Substring

    fun hash ss		= String.hash(string ss)
    fun equal(ss, st)	= string ss = string st
  end


(*****************************************************************************
 * UniqueString
 *****************************************************************************)

signature UNIQUE_STRING =
sig
    structure String : STRING

    eqtype unique_string
    type t = unique_string

    val unique :	String.string -> unique_string
    val string :	unique_string -> String.string

    val equal :		unique_string * unique_string -> bool
    val compare :	unique_string * unique_string -> order
    val hash :		unique_string -> int
end

structure UniqueString :> UNIQUE_STRING where String = String =
struct
    structure String	= String

    type unique_string	= string
    type t		= unique_string

    fun unique s	= s
    fun string s	= s

    val equal		= String.equal
    val compare		= String.compare
    val hash		= String.hash
end


(*****************************************************************************
 * Int
 *****************************************************************************)

signature INTEGER =
  sig
    include INTEGER

    type t = int

    val equal :	int * int -> bool
    val hash :	int -> Int.int
  end


structure Int : INTEGER =
  struct
    open Int

    type t	= int

    val equal	= op =
    fun hash i	= abs i handle Overflow => 0
  end


structure LargeInt : INTEGER =
  struct
    open LargeInt

    type t	= int

    val equal	= op =
    fun hash i	= toInt(abs i mod valOf maxInt) handle Overflow => 0
  end


structure Position : INTEGER =
  struct
    open Position

    type t	= int

    val equal	= op =
    fun hash i	= toInt(abs i mod valOf maxInt) handle Overflow => 0
  end



(*****************************************************************************
 * Word
 *****************************************************************************)

signature WORD =
  sig
    include WORD

    type t = word

    val equal :	word * word -> bool
    val hash :	word -> int
  end


structure Word : WORD =
  struct
    open Word

    type t	= word

    val equal	= op =
    fun hash w	= abs(toInt w) handle Overflow => 0
  end


structure LargeWord : WORD =
  struct
    open LargeWord

    type t	= word

    val equal	= op =
    fun hash w	= abs(toInt w) handle Overflow => 0
  end



(*****************************************************************************
 * Real
 *****************************************************************************)

signature REAL =
  sig
    include REAL

    type t = real

    val equal : real * real -> bool
  end


structure Real : REAL =
  struct
    open Real

    type t	= real

    val equal	= op ==
  end


structure LargeReal : REAL =
  struct
    open LargeReal

    type t	= real

    val equal	= op ==
  end



(*****************************************************************************
 * Vector
 *****************************************************************************)

signature VECTOR =
  sig
    include VECTOR

    type 'a t = 'a vector

    val toList :	'a vector -> 'a list

    val append :	'a vector * 'a vector -> 'a vector
    val rev :		'a vector -> 'a vector
    val replace :	'a vector * int * 'a -> 'a vector

    val appr :		('a -> unit) -> 'a vector -> unit
    val appri :		(int * 'a -> unit) -> 'a vector * int * int option
					   -> unit
    val all :		('a -> bool) -> 'a vector -> bool
    val exists :	('a -> bool) -> 'a vector -> bool
    val find :		('a -> bool) -> 'a vector -> 'a option
    val contains :	''a vector -> ''a -> bool
    val notContains :	''a vector -> ''a -> bool

    val equal :		('a * 'a -> bool) -> 'a vector * 'a vector -> bool
    val compare :	('a * 'a -> order) -> 'a vector * 'a vector -> order

    val isSorted :	('a * 'a -> order) -> 'a vector -> bool
    val sort :		('a * 'a -> order) -> 'a vector -> 'a vector
  end


structure Vector : VECTOR =
  struct
    open Vector

    type 'a t = 'a vector

    fun toList v		= toList'(v, length v - 1, [])
    and toList'(v, ~1, xs)	= xs
      | toList'(v, i, xs)	= toList'(v, i-1, sub(v,i)::xs)

    fun append(v1,v2)		= concat[v1,v2]

    fun rev v			= let val len = length v
				      fun f i = sub(v, len-i-1)
				  in tabulate(len, f) end

    fun replace(v,i,x)		= let fun f j = if i = j then x else sub(v,j)
				  in tabulate(length v, f) end

    fun sliceLength(v,i,NONE)	= if i > length v then raise Subscript else
				  length v - i
      | sliceLength(v,i,SOME n)	= if i+n > length v orelse n < 0
				  then raise Subscript else n

    fun appr  f v		= appr'(f, v, length v - 1)
    and appr'(f,v,~1)		= ()
      | appr'(f,v,i)		= (f(sub(v,i)) ; appr'(f,v,i-1))

    fun appri f (v,i,no)	= appri'(f, v, i, i-1+sliceLength(v,i,no))
    and appri'(f,v,i,j)		= if i < j then () else
				  (f(i, sub(v,i)) ; appri'(f,v,i-1,j))

    fun all  f v		= all'(f,v,0)
    and all'(f,v,i) 		= i = length v orelse
				  f(sub(v,i)) andalso all'(f,v,i+1)

    fun exists  f v		= exists'(f,v,0)
    and exists'(f,v,i) 		= i <> length v andalso
				  (f(sub(v,i)) orelse exists'(f,v,i+1))

    fun contains v x		= contains'(v,x,0)
    and contains'(v,x,i)	= i <> length v andalso
				  (x = sub(v,i) orelse contains'(v,x,i+1))
    fun notContains v x		= Bool.not(contains'(v,x,0))

    fun find  f v		= find'(f,v,0)
    and find'(f,v,i) 		= if i = length v then NONE else
				  let val x = sub(v,i) in
				      if f x then SOME x
					     else find'(f,v,i+1)
				  end

    fun equal  eq (v1,v2)	= length v1 = length v2 andalso
				  equal'(eq,v1,v2,0)
    and equal'(eq,v1,v2,i)	= i = length v1 orelse
				  eq(sub(v1,i), sub(v2,i)) andalso
				  equal'(eq,v1,v2,i+1)

    fun compare  cmp (v1,v2)	= compare'(cmp,v1,v2,0)
    and compare'(cmp,v1,v2,i)	= case (i = length v1, i = length v2)
				    of (true,  true)  => EQUAL
				     | (true,  false) => LESS
				     | (false, true)  => GREATER
				     | (false, false) =>
				  case cmp(sub(v1,i), sub(v2,i))
				    of EQUAL => compare'(cmp,v1,v2,i+1)
				     | other => other

    fun sort compare		= sort'(List.sort compare)
    and sort' sortList v	= fromList(sortList(toList v))

    fun isSorted compare v	= isSorted'(compare,v,1)
    and isSorted'(compare,v,i)	= i >= length v orelse
				  compare(sub(v,i-1), sub(v,i)) <> GREATER
				  andalso isSorted'(compare,v,i+1)
  end



(*****************************************************************************
 * VectorPair
 *****************************************************************************)

signature VECTOR_PAIR =
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


structure VectorPair : VECTOR_PAIR =
  struct
    val sub = Vector.sub

    fun length (v1,v2)		= Int.min(Vector.length v1, Vector.length v2)
    fun sliceLength(v1,v2,i,NONE)
				= if i > Vector.length v1 orelse
				     i > Vector.length v2
				  then raise General.Subscript
				  else Int.min(Vector.length v1,
					       Vector.length v2) - i
      | sliceLength(v1,v2,i,SOME n)
				= if i+n > Vector.length v1 orelse
 				     i+n > Vector.length v2 orelse n < 0
				  then raise General.Subscript
				  else n


    fun zip(v1,v2)		= Vector.tabulate(length(v1,v2),
				     fn i => (sub(v1,i), sub(v2,i)))
    fun unzip(v: ('a*'b)vector)	= (Vector.map #1 v, Vector.map #2 v)

    fun map f (v1,v2)		= Vector.tabulate(length(v1,v2),
				     fn i => f(sub(v1,i), sub(v2,i)))
    fun mapi f (v1,v2,i,no)	= Vector.tabulate(sliceLength(v1,v2,i,no),
				     fn k => f(i+k, sub(v1,i+k), sub(v2,i+k)))

    fun app f (v1,v2)		= app'(f, v1, v2, 0, length(v1,v2))
    and app'(f,v1,v2,i,0)	= ()
      | app'(f,v1,v2,i,n)	= (f(sub(v1,i), sub(v2,i)) ;
				   app'(f,v1,v2,i+1,n-1))

    fun appr f (v1,v2)		= appr'(f, v1, v2, length(v1,v2)-1)
    and appr'(f,v1,v2,~1)	= ()
      | appr'(f,v1,v2,i)	= (f(sub(v1,i),sub(v2,i)) ; appr'(f,v1,v2,i-1))

    fun foldl f x (v1,v2)	= foldl'(f, x, v1, v2, 0, length(v1,v2))
    and foldl'(f,x,v1,v2,i,0)	= x
      | foldl'(f,x,v1,v2,i,n)	= foldl'(f, f(sub(v1,i), sub(v2,i), x),
					 v1, v2, i+1, n-1)

    fun foldr f x (v1,v2)	= foldr'(f, x, v1, v2, length(v1,v2)-1)
    and foldr'(f,x,v1,v2,~1)	= x
      | foldr'(f,x,v1,v2,i)	= foldr'(f, f(sub(v1,i), sub(v2,i), x),
					 v1, v2, i-1)

    fun appi f (v1,v2,i,no)	= appi'(f, v1, v2, i, sliceLength(v1,v2,i,no))
    and appi'(f,v1,v2,i,0)	= ()
      | appi'(f,v1,v2,i,n)	= (f(i, sub(v1,i), sub(v2,i)) ;
				   appi'(f,v1,v2,i+1,n-1))

    fun appri f (v1,v2,i,no)	= appri'(f,v1,v2,i, i-1+sliceLength(v1,v2,i,no))
    and appri'(f,v1,v2,i,j)	= if i < j then () else
				  (f(i, sub(v1,i), sub(v2,i)) ;
  				   appri'(f,v1,v2,i-1,j))

    fun foldli f x (v1,v2,i,no)	= foldli'(f,x,v1,v2, i, sliceLength(v1,v2,i,no))
    and foldli'(f,x,v1,v2,i,0)	= x
      | foldli'(f,x,v1,v2,i,n)	= foldli'(f, f(i, sub(v1,i), sub(v2,i), x),
					  v1, v2, i+1, n-1)

    fun foldri f x (v1,v2,i,no)	= foldri'(f, x, v1, v2,
					  i-1+sliceLength(v1,v2,i,no), i)
    and foldri'(f,x,v1,v2,i,j)	= if i < j then x else
				  foldri'(f, f(i, sub(v1,i), sub(v2,i), x),
					  v1, v2, i-1, j)

    fun all f (v1,v2)		= all'(f, v1, v2, 0, length(v1,v2))
    and all'(f,v1,v2,i,n) 	= n = 0 orelse ( f(sub(v1,i), sub(v2,i))
					andalso  all'(f,v1,v2,i+1,n-1) )

    fun exists f (v1,v2)	= exists'(f, v1, v2, 0, length(v1,v2))
    and exists'(f,v1,v2,i,n)	= n <> 0 andalso ( f(sub(v1,i), sub(v2,i))
					   orelse  exists'(f,v1,v2,i+1,n-1) )

    fun find  f (v1,v2)		= find'(f, v1, v2, 0, length(v1,v2))
    and find'(f,v1,v2,i,0)	= NONE
      | find'(f,v1,v2,i,n)	= let val xy = (sub(v1,i), sub(v2,i)) in
				      if f xy then SOME xy
					      else find'(f,v1,v2,i+1,n-1)
				  end
  end



(*****************************************************************************
 * Array
 *****************************************************************************)

signature ARRAY =
  sig
    include ARRAY

    type 'a t = 'a array

    val new :		int * 'a -> 'a array

    val fromVector :	'a vector -> 'a array
    val toVector :	'a array -> 'a vector
    val toList :	'a array -> 'a list

    val swap :		'a array * int * int -> unit
    val reverse :	'a array -> unit

    val appr :		('a -> unit) -> 'a array -> unit
    val appri :		(int * 'a -> unit) -> 'a array * int * int option
					   -> unit

    val all :		('a -> bool) -> 'a array -> bool
    val exists :	('a -> bool) -> 'a array -> bool
    val find :		('a -> bool) -> 'a array -> 'a option

    val equal :		('a * 'a -> bool) -> 'a array * 'a array -> bool (**)
    val compare :	('a * 'a -> order) -> 'a array * 'a array -> order

    val isSorted :	('a * 'a -> order) -> 'a array -> bool
    val sort :		('a * 'a -> order) -> 'a array -> unit
  end


structure Array : ARRAY =
  struct
    open Array

    type 'a t		= 'a array

    val new		= array
    fun fromVector v	= tabulate(Vector.length v, fn i => Vector.sub(v,i))
    fun toVector a	= Vector.tabulate(length a, fn i => sub(a,i))

    fun toList a		= toList'(a, length a - 1, [])
    and toList'(a, ~1, xs)	= xs
      | toList'(a, i, xs)	= toList'(a, i-1, sub(a,i)::xs)

    fun swap(a,i,j)		= let val x = sub(a,i) in
				      update(a, i, sub(a,j)) ;
				      update(a, j, x)
				  end

    fun reverse a		= let fun reverse'(i,j) =
				      if i >= j then () else
					  (swap(a,i,j) ; reverse'(i+1, j-1))
				  in reverse'(0, length a - 1) end

    fun sliceLength(v,i,NONE)	= if i > length v then raise Subscript else
				  length v - i
      | sliceLength(v,i,SOME n)	= if i+n > length v orelse n < 0
				  then raise Subscript else n

    fun appr  f v		= appr'(f, v, length v - 1)
    and appr'(f,v,~1)		= ()
      | appr'(f,v,i)		= (f(sub(v,i)) ; appr'(f,v,i-1))

    fun appri f (v,i,no)	= appri'(f, v, i, i-1+sliceLength(v,i,no))
    and appri'(f,v,i,j)		= if i < j then () else
				  (f(i, sub(v,i)) ; appri'(f,v,i-1,j))

    fun all  f a		= all'(f,a,0)
    and all'(f,a,i) 		= i = length a orelse
				  f(sub(a,i)) andalso all'(f,a,i+1)

    fun exists  f a		= exists'(f,a,0)
    and exists'(f,a,i) 		= i <> length a andalso
				  (f(sub(a,i)) orelse exists'(f,a,i+1))

    fun find  f a		= find'(f,a,0)
    and find'(f,a,i) 		= if i = length a then NONE else
				  let val x = sub(a,i) in
				      if f x then SOME x
					     else find'(f,a,i+1)
				  end

    fun equal  eq (a1,a2)	= length a1 = length a2 andalso
				  equal'(eq,a1,a2,0)
    and equal'(eq,a1,a2,i)	= i = length a1 orelse
				  eq(sub(a1,i), sub(a2,i)) andalso
				  equal'(eq,a1,a2,i+1)

    fun compare  cmp (a1,a2)	= compare'(cmp,a1,a2,0)
    and compare'(cmp,a1,a2,i)	= case (i = length a1, i = length a2)
				    of (true,  true)  => EQUAL
				     | (true,  false) => LESS
				     | (false, true)  => GREATER
				     | (false, false) =>
				  case cmp(sub(a1,i), sub(a2,i))
				    of EQUAL => compare'(cmp,a1,a2,i+1)
				     | other => other

    fun isSorted compare a	= isSorted'(compare,a,1)
    and isSorted'(compare,a,i)	= i >= length a orelse
				  compare(sub(a,i-1), sub(a,i)) <> GREATER
				  andalso isSorted'(compare,a,i+1)

    fun sort compare a =
    let
	fun partition(i,j,p) =
	    if i = j then j
	    else if compare(sub(a,i),p) <> GREATER then partition(i+1,j,p)
	    else if compare(p,sub(a,j-1)) = LESS   then partition(i,j-1,p)
	    else (swap(a,i,j-1); partition(i+1,j-1,p))

	fun sort(i,j) =
	    if j-i <= 1 then ()
	    else if j-i = 2 then
		if compare(sub(a,i), sub(a,j-1)) <> GREATER
		then ()
		else swap(a,i,j-1)
	    else let
		val mid = (i+j) div 2
		val _ = if compare(sub(a,i), sub(a,mid)) <> GREATER then ()
			else swap(a,i,mid)
		val _ = if compare(sub(a,mid), sub(a,j-1)) <> GREATER then ()
			else (swap(a,mid,j-1);
			      if compare(sub(a,i),sub(a,mid)) <> GREATER then ()
			      else swap(a,i,mid))
	  	val k = partition(i+1,j-1, sub(a,mid))
	    in
		sort(i,k); sort(k,j)
	    end
    in
	sort(0, length a)
    end
  end



(*****************************************************************************
 * Time
 *****************************************************************************)

signature TIME =
  sig
    include TIME

    type t = time

    val equal :	time * time -> bool
    val hash :	time -> int
  end


structure Time : TIME =
  struct
    open Time

    type t	= time

    val equal	= op =
    fun hash t	= LargeInt.hash(toMicroseconds t)
  end



(*****************************************************************************
 * Top-level
 *****************************************************************************)

infix 3 :=:

val op :=:	= General.:=:
val flip	= General.flip
