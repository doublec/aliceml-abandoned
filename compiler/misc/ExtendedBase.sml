(*****************************************************************************
 * General
 *****************************************************************************)

signature GENERAL =
  sig
    include GENERAL

    val :=: :		'a ref * 'a ref -> unit
  end


structure General : GENERAL =
  struct
    open General

    fun op :=: (r1 as ref x1, r2 as ref x2)	= (r1 := x2 ; r2 := x1)
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

    val map: ('a -> 'a) -> 'a ref -> 'a ref
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

    fun map f (r as ref x)		= ref(f x)
  end



(*****************************************************************************
 * Bool
 *****************************************************************************)

signature BOOL =
  sig
    include BOOL

    type t = bool
  end


structure Bool : BOOL =
  struct
    open Bool

    type t = bool
  end



(*****************************************************************************
 * Option
 *****************************************************************************)

signature OPTION =
  sig
    include OPTION

    type 'a t

    val isNone :	'a option -> bool

    val app :		('a -> unit) -> 'a option -> unit
    val fold :		('a * 'b -> 'b) -> 'b -> 'a option -> 'b
  end


structure Option : OPTION =
  struct
    open Option

    type 'a t			= 'a option

    fun isNone NONE		= true
      | isNone  _		= false

    fun app f  NONE		= ()
      | app f (SOME x)		= f x

    fun fold f b  NONE		= b
      | fold f b (SOME a)	= f(a,b)
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
    val mapi :		(int * 'a -> 'b) -> 'a list -> 'b list
    val foldli :	(int * 'a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val foldri :	(int * 'a * 'b -> 'b) -> 'b -> 'a list -> 'b
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

    fun mapi  f xs		= mapi'(f,0,xs)
    and mapi'(f, i,  nil )	= nil
      | mapi'(f, i, x::xs)	= f(i,x) :: mapi'(f, i+1, xs)

    fun appi  f xs		= appi'(f,0,xs)
    and appi'(f, i,  nil )	= ()
      | appi'(f, i, x::xs)	= (f(i,x) ; appi'(f, i+1, xs))

    fun appri  f xs		= appri'(f,0,xs)
    and appri'(f, i,  nil )	= ()
      | appri'(f, i, x::xs)	= (appri'(f, i+1, xs) ; f(i,x))
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


structure ListPair =
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
      | foldli'(f, z, i, x::xs, y::ys)	= foldli'(f, f(x,y,z), i+1, xs, ys)

    fun foldri  f  z (xs,ys)      	= foldri'(f,z,0,xs,ys)
    and foldri'(f, z, i,  nil,    _  ) 	= z
      | foldri'(f, z, i,   _,    nil ) 	= z
      | foldri'(f, z, i, x::xs, y::ys) 	= f(x, y, foldri'(f,z,i+1,xs,ys))

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

    fun toWide c	= c
    fun fromWide c	= c
  end


structure WideChar = Char


in (* local *)


signature CHAR =
  sig
    include CHAR

    type t = char

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

    fun toWide s	= s
    fun fromWide s	= s

    val maxLen		= maxSize
    val length		= size
    val append		= op ^
    val fromList	= implode
    val toList		= explode
    val tabulate	= implode o List.tabulate

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

    val maxLen :	int
    val length :	string -> int
    val append :	string * string -> string

    val toWide :	string -> WideString.string
    val fromWide :	WideString.string -> string

    val fromList :	Char.char list -> string
    val toList :	string -> Char.char list
    val tabulate :	int * (int -> Char.char) -> string

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
  end



(*****************************************************************************
 * Int
 *****************************************************************************)

signature INTEGER =
  sig
    include INTEGER

    type t = int
  end


structure Int : INTEGER =
  struct
    open Int

    type t = int
  end


structure LargeInt : INTEGER =
  struct
    open LargeInt

    type t = int
  end


structure Position : INTEGER =
  struct
    open Position

    type t = int
  end



(*****************************************************************************
 * Word
 *****************************************************************************)

signature WORD =
  sig
    include WORD

    type t = word
  end


structure Word : WORD =
  struct
    open Word

    type t = word
  end


structure LargeWord : WORD =
  struct
    open LargeWord

    type t = word
  end



(*****************************************************************************
 * Real
 *****************************************************************************)

signature REAL =
  sig
    include REAL

    type t = real
  end


structure Real : REAL =
  struct
    open Real

    type t = real
  end


structure LargeReal : REAL =
  struct
    open LargeReal

    type t = real
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
  end


structure Vector =
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
    and exists'(f,v,i) 		= i = length v orelse
				  f(sub(v,i)) orelse exists'(f,v,i+1)

    fun find  f v		= find'(f,v,0)
    and find'(f,v,i) 		= if i = length v then NONE else
				  let val x = sub(v,i) in
				      if f x then SOME x
					     else find'(f,v,i+1)
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
    and exists'(f,a,i) 		= i = length a orelse
				  f(sub(a,i)) orelse exists'(f,a,i+1)

    fun find  f a		= find'(f,a,0)
    and find'(f,a,i) 		= if i = length a then NONE else
				  let val x = sub(a,i) in
				      if f x then SOME x
					     else find'(f,a,i+1)
				  end
  end



(*****************************************************************************
 * Time
 *****************************************************************************)

signature TIME =
  sig
    include TIME

    type t = time
  end


structure Time : TIME =
  struct
    open Time

    type t = time
  end



(*****************************************************************************
 * Top-level
 *****************************************************************************)

infix 3 :=:

val op :=:	= General.:=:
