(*****************************************************************************
 * General
 *****************************************************************************)

structure General =
  struct
    open General

    exception Unordered = IEEEReal.Unordered
    exception Assert of string * int

    fun op :=: (r1 as ref x1, r2 as ref x2)	= (r1 := x2 ; r2 := x1)
    fun id x					= x
    fun const x y				= x
    fun curry f x y				= f(x,y)
    fun uncurry f (x,y)				= f x y
    fun flip f (x,y)				= f(y,x)
    fun inverse LESS				= GREATER
      | inverse EQUAL				= EQUAL
      | inverse GREATER				= LESS

    fun assert exp = if exp then Unsafe.cast () else raise Assert ("assert", 0)
  end

exception Unordered = General.Unordered
exception Assert = General.Assert
val op :=:	= General.:=:
val id		= General.id
val const	= General.const
val curry	= General.curry
val uncurry	= General.uncurry
val flip	= General.flip
val inverse	= General.inverse
val assert	= General.assert

(*****************************************************************************
 * Ref
 *****************************************************************************)

structure Ref =
  struct
    datatype ref = datatype ref
    type  'a t   = 'a ref

    val !				= General.!
    val op :=				= General.:=
    val op :=:				= General.:=:

    fun exchange(r as ref x1, x2)	= (r := x2 ; x1)

    val equal				= op =
    fun app f (ref x)			= f x
    fun map f (ref x)			= ref(f x)
    fun modify f (r as ref x)		= r := f(!r)
  end



(*****************************************************************************
 * Bool
 *****************************************************************************)

structure Bool =
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

structure Option =
  struct
    open Option

    type 'a t				= 'a option

    fun isNone NONE			= true
      | isNone  _			= false

    fun equal eq (NONE,   NONE)		= true
      | equal eq (SOME x, SOME y)	= eq(x,y)
      | equal eq _			= false

    fun collate cmp (NONE,   NONE)	= EQUAL
      | collate cmp (NONE,   SOME _)	= LESS
      | collate cmp (SOME _, NONE)	= GREATER
      | collate cmp (SOME x, SOME y)	= cmp(x,y)

    fun app f  NONE			= ()
      | app f (SOME x)			= f x

    fun fold f b  NONE			= b
      | fold f b (SOME a)		= f(a,b)
  end



(*****************************************************************************
 * Pair
 *****************************************************************************)

structure Pair =
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

    fun equal (eqX,eqY) ((x1,y1), (x2,y2)) =
	eqX(x1,x2) andalso eqY(y1,y2)

    fun collate (cmpX,cmpY) ((x1,y1), (x2,y2)) =
	case cmpX(x1,x2)
	 of EQUAL => cmpY(y1,y2)
	  | other => other
  end



(*****************************************************************************
 * Alt
 *****************************************************************************)

structure Alt =
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

    fun collate (cmpX,cmpY) =
	fn (FST x1, FST x2)	=> cmpX(x1,x2)
	 | (SND y1, SND y2)	=> cmpY(y1,y2)
	 | (FST _,  SND _ )	=> LESS
	 | (SND _,  FST _ )	=> GREATER
  end



(*****************************************************************************
 * List
 *****************************************************************************)

structure List =
  struct
    open List

    type 'a t			= 'a list

    val sub			= nth

    fun index xs		= index'(0,xs)
    and index'(i, nil  )	= nil
      | index'(i, x::xs)	= (i,x) :: index'(i+1,xs)

    fun appr  f   xs		= appr'(f,xs)
    and appr'(f,  nil )		= ()
      | appr'(f, x::xs)		= (appr'(f,xs) ; f x)

    fun appi  f xs		= appi'(f,0,xs)
    and appi'(f, i,  nil )	= ()
      | appi'(f, i, x::xs)	= (f(i,x) ; appi'(f, i+1, xs))

    fun appri  f xs		= appri'(f,0,xs)
    and appri'(f, i,  nil )	= ()
      | appri'(f, i, x::xs)	= (appri'(f, i+1, xs) ; f(i,x))

    fun foldli f y xs		= foldli'(f,y,0,xs)
    and foldli'(f, y, i,  nil )	= y
      | foldli'(f, y, i, x::xs)	= foldli'(f, f(i,x,y), i+1, xs)

    fun mapi  f xs		= mapi'(f,0,xs)
    and mapi'(f, i,  nil )	= nil
      | mapi'(f, i, x::xs)	= f(i,x) :: mapi'(f, i+1, xs)

    fun mapiPartial  f   xs	= mapiPartial'(f,0,xs)
    and mapiPartial'(f,i, nil )	= nil
      | mapiPartial'(f,i,x::xs)	= case f(i,x)
				   of NONE   => mapiPartial'(f,i+1,xs)
				    | SOME y => y::mapiPartial'(f,i+1,xs)

    fun foldri  f  y xs		= foldri'(f,y,0,xs)
    and foldri'(f, y, i,  nil )	= y
      | foldri'(f, y, i, x::xs)	= f(i, x, foldri'(f, y, i+1, xs))

    fun alli  f   xs		= alli'(f,0,xs)
    and alli'(f, i,  nil )	= true
      | alli'(f, i, x::xs)	= f(i,x) andalso alli'(f,i+1,xs)

    fun existsi  f   xs		= existsi'(f,0,xs)
    and existsi'(f, i,  nil )	= false
      | existsi'(f, i, x::xs)	= f(i,x) orelse existsi'(f,i+1,xs)

    fun findi  f   xs		= findi'(f,0,xs)
    and findi'(f, i,  nil )	= NONE
      | findi'(f, i, x::xs)	= if f(i,x) then SOME(i,x) else findi'(f,i+1,xs)

    fun filteri  f   xs			= filteri'(f,0,xs,nil)
    and filteri'(f, i,  nil,  zs)	= rev zs
      | filteri'(f, i, x::xs, zs)	= filteri'(f,i+1,xs,
					       if f(i,x) then (i,x)::zs else zs)

    fun partitioni  f xs		= partitioni'(f,0,xs,nil,nil)
    and partitioni'(f,i,  nil,  ys, zs)	= (rev ys, rev zs)
      | partitioni'(f,i, x::xs, ys, zs)	= if f(i,x)
					  then partitioni'(f,i+1,xs,(i,x)::ys,zs)
					  else partitioni'(f,i+1,xs,ys,(i,x)::zs)

    fun contains xs y		= contains'(y, xs)
    and contains'(y, nil)	= false
      | contains'(y, x::xs)	= y = x orelse contains'(y,xs)

    fun notContains xs y	= Bool.not(contains'(y,xs))

    fun equal  eq (xs,ys)	= equal'(eq,xs,ys)
    and equal'(eq, [], [])	= true
      | equal'(eq, x::xs,y::ys)	= eq(x,y) andalso equal'(eq,xs,ys)
      | equal' _		= false

    fun collate  cmp (xs,ys)	= collate'(cmp,xs,ys)
    and collate'(cmp, [], [])	= EQUAL
      | collate'(cmp, [], _ )	= LESS
      | collate'(cmp, _,  [])	= GREATER
      | collate'(cmp, x::xs, y::ys) =
				  case cmp(x,y)
				    of EQUAL => collate'(cmp,xs,ys)
				     | other => other

    fun isSorted  cmp xs	= isSorted'(cmp, xs)
    and isSorted'(cmp,(nil|_::nil))
				= true
      | isSorted'(cmp, x1::(xs as x2::_))
				= cmp(x1,x2) <> GREATER
					  andalso isSorted'(cmp, xs)

    fun split nil		= (nil, nil)
      | split(xs as _::nil)	= (xs, nil)
      | split(x1::x2::xs)	= let val (xs1,xs2) = split xs
				  in (x1::xs1, x2::xs2) end
    fun sort cmp		=
    let
	fun merge(xs, nil)	= xs
	  | merge(nil, ys)	= ys
	  | merge(xs as x::xs',
		  ys as y::ys')	= case cmp(x,y)
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

structure ListPair =
  struct
    open ListPair

    fun allEq f (x::xs, y::ys)		= f(x,y) andalso allEq f (xs,ys)
      | allEq f (nil,   nil)		= true
      | allEq f (  _,     _)		= false

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


(*****************************************************************************
 * String
 *****************************************************************************)

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

    val tabulate	= implode o List.tabulate

    fun concatWith s' ss	= concat(concatWith'(s', ss))
    and concatWith'(s', nil)	= nil
      | concatWith'(s', s::nil)	= s::nil
      | concatWith'(s', s::ss)	= s::s'::concatWith'(s', ss)

    fun isSuffix s1 s2			= isSuffix'(s1,s2,size s1-1,size s2-1)
    and isSuffix'(s1,s2,i1,i2)		= i1 = ~1 orelse
					  i2 <> ~1 andalso
					  sub(s1,i1) = sub(s2,i1)
					  andalso isSuffix'(s1,s2,i1-1,i2-1)
  end

structure WideString = String


(*****************************************************************************
 * Substring
 *****************************************************************************)

structure Substring =
  struct
    open Substring

    val full		= all
    fun hash ss		= String.hash(string ss)
    fun equal(ss, st)	= string ss = string st

    fun appr f ss	= List.appr f (explode ss)
  end

structure WideSubstring = Substring


(*****************************************************************************
 * CharVectorSlice
 *****************************************************************************)

structure CharVectorSlice =
  struct
    fun all f ss =
	Substring.size ss = 0 orelse
	f(Substring.sub(ss,0)) andalso all f (Substring.triml 1 ss)
  end


(*****************************************************************************
 * Int
 *****************************************************************************)

structure Int =
  struct
    open Int

    type t	= int

    val equal	= op =
    fun hash i	= abs i handle Overflow => 0
  end


structure LargeInt =
  struct
    open LargeInt

    type t	= int

    val equal	= op =
    fun hash i	= toInt(abs i mod valOf maxInt) handle Overflow => 0
  end

structure Position =
  struct
    open Position

    type t	= int

    val equal	= op =
    fun hash i	= toInt(abs i mod valOf maxInt) handle Overflow => 0
  end



(*****************************************************************************
 * Word
 *****************************************************************************)

structure Word =
  struct
    open Word

    type t	  = word

    fun ~w	  = 0w0-w
    val fromLarge = fromLargeWord
    val toLarge   = toLargeWord
    val toLargeX  = toLargeWordX
    val equal	  = op =
    fun hash w	  = abs(toInt w) handle Overflow => 0
  end

structure Word8 =
  struct
    open Word8

    type t	  = word

    fun ~w	  = 0w0-w
    val fromLarge = fromLargeWord
    val toLarge   = toLargeWord
    val toLargeX  = toLargeWordX
    val equal	  = op =
    fun hash w	  = abs(toInt w) handle Overflow => 0
  end

structure Word32 =
  struct
    open Word32

    type t	  = word

    fun ~w	  = 0w0-w
    val fromLarge = fromLargeWord
    val toLarge   = toLargeWord
    val toLargeX  = toLargeWordX
    val equal	  = op =
    fun hash w	  = abs(toInt w) handle Overflow => 0
  end

structure LargeWord =
  struct
    open LargeWord

    type t	  = word

    fun ~w	  = 0w0-w
    val fromLarge = fromLargeWord
    val toLarge   = toLargeWord
    val toLargeX  = toLargeWordX
    val equal	  = op =
    fun hash w	  = abs(toInt w) handle Overflow => 0
  end


(*****************************************************************************
 * Real
 *****************************************************************************)

structure Real =
  struct
    open Real

    type t	= real

    val equal	= op ==
    fun hash x	= raise Fail "Real.hash"
  end

structure LargeReal =
  struct
    open LargeReal

    type t	= real

    val equal	= op ==
    fun hash x	= raise Fail "Real.hash"
  end


(*****************************************************************************
 * Atom
 *****************************************************************************)

structure Atom =
  struct
    open Atom
    type t = atom
    val equal = sameAtom
    fun hash a = Word.toIntX(Atom.hash a)
  end



(*****************************************************************************
 * Vector
 *****************************************************************************)

structure Vector =
  struct
    open Vector

    fun appi  f  vec		= appi'(f, vec, 0)
    and appi'(f, vec, i)	= if i = length vec then () else
				  (f(i, sub(vec, i)); appi'(f, vec, i+1))

    fun foldli f init vec	= foldli'(f, init, vec, 0)
    and foldli'(f, x, vec, i)	= if i = length vec then x else
				  foldli'(f, f(i, sub(vec, i), x), vec, i+1)
    fun foldri f init vec	= foldri'(f, init, vec, length vec)
    and foldri'(f, x, vec, i)	= if i = 0 then x else
				  foldri'(f, f(i-1, sub(vec, i-1), x), vec, i-1)

    fun mapi f vec		= fromList
				    (foldri (fn (i, x, l) => f(i, x)::l) [] vec)

    type 'a t			= 'a vector

    fun toList v		= toList'(v, length v - 1, [])
    and toList'(v, ~1, xs)	= xs
      | toList'(v, i, xs)	= toList'(v, i-1, sub(v,i)::xs)

    fun rev v			= let val len = length v
				      fun f i = sub(v, len-i-1)
				  in tabulate(len, f) end

    fun update(v,i,x)		= let fun f j = if i = j then x else sub(v,j)
				  in tabulate(length v, f) end

    fun appr  f v		= appr'(f, v, length v - 1)
    and appr'(f,v,~1)		= ()
      | appr'(f,v,i)		= (f(sub(v,i)) ; appr'(f,v,i-1))

    fun appri f v		= appri'(f, v, length v - 1)
    and appri'(f,v,~1)		= ()
      | appri'(f,v,i)		= (f(i, sub(v,i)) ; appri'(f,v,i-1))

    fun all  f v		= all'(f,v,0)
    and all'(f,v,i) 		= i = length v orelse
				  f(sub(v,i)) andalso all'(f,v,i+1)

    fun alli  f v		= alli'(f,v,0)
    and alli'(f,v,i) 		= i = length v orelse
				  f(i,sub(v,i)) andalso alli'(f,v,i+1)

    fun exists  f v		= exists'(f,v,0)
    and exists'(f,v,i) 		= i <> length v andalso
				  (f(sub(v,i)) orelse exists'(f,v,i+1))

    fun existsi  f v		= existsi'(f,v,0)
    and existsi'(f,v,i) 	= i <> length v andalso
				  (f(i,sub(v,i)) orelse existsi'(f,v,i+1))

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

    fun findi  f v		= findi'(f,v,0)
    and findi'(f,v,i) 		= if i = length v then NONE else
				  let val x = sub(v,i) in
				      if f(i,x) then SOME x
					        else findi'(f,v,i+1)
				  end

    fun equal  eq (v1,v2)	= length v1 = length v2 andalso
				  equal'(eq,v1,v2,0)
    and equal'(eq,v1,v2,i)	= i = length v1 orelse
				  eq(sub(v1,i), sub(v2,i)) andalso
				  equal'(eq,v1,v2,i+1)

    fun collate  cmp (v1,v2)	= collate'(cmp,v1,v2,0)
    and collate'(cmp,v1,v2,i)	= case (i = length v1, i = length v2)
				    of (true,  true)  => EQUAL
				     | (true,  false) => LESS
				     | (false, true)  => GREATER
				     | (false, false) =>
				  case cmp(sub(v1,i), sub(v2,i))
				    of EQUAL => collate'(cmp,v1,v2,i+1)
				     | other => other

    fun sort cmp		= sort'(List.sort cmp)
    and sort' sortList v	= fromList(sortList(toList v))

    fun isSorted cmp v		= isSorted'(cmp,v,1)
    and isSorted'(cmp,v,i)	= i >= length v orelse
				  cmp(sub(v,i-1), sub(v,i)) <> GREATER
				  andalso isSorted'(cmp,v,i+1)
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
    val find :		('a * 'b -> bool) -> 'a vector * 'b vector
					  -> ('a * 'b) option

    val appi :		(int * 'a * 'b -> unit) -> 'a vector * 'b vector -> unit
    val appri :		(int * 'a * 'b -> unit) -> 'a vector * 'b vector -> unit
    val mapi :		(int * 'a * 'b -> 'c) ->
			   'a vector * 'b vector -> 'c vector
    val foldli :	(int * 'a * 'b * 'c -> 'c) -> 'c ->
			   'a vector * 'b vector -> 'c
    val foldri :	(int * 'a * 'b * 'c -> 'c) -> 'c ->
			   'a vector * 'b vector -> 'c
    val alli :		(int * 'a * 'b -> bool) -> 'a vector * 'b vector -> bool
    val existsi :	(int * 'a * 'b -> bool) -> 'a vector * 'b vector -> bool
    val findi :		(int * 'a * 'b -> bool) -> 'a vector * 'b vector ->
						   (int * 'a * 'b) option
  end


structure VectorPair : VECTOR_PAIR =
  struct
    val sub' = Vector.sub

    fun length (v1,v2)		= Int.min(Vector.length v1, Vector.length v2)

    fun zip(v1,v2)		= Vector.tabulate(length(v1,v2),
				     fn i => (sub'(v1,i), sub'(v2,i)))
    fun unzip(v: ('a*'b)vector)	= (Vector.map #1 v, Vector.map #2 v)

    fun map f (v1,v2)		= Vector.tabulate(length(v1,v2),
				     fn i => f(sub'(v1,i), sub'(v2,i)))
    fun mapi f (v1,v2)		= Vector.tabulate(length(v1,v2),
				     fn i => f(i, sub'(v1,i), sub'(v2,i)))

    fun app f (v1,v2)		= app'(f, v1, v2, 0, length(v1,v2))
    and app'(f,v1,v2,i,0)	= ()
      | app'(f,v1,v2,i,n)	= (f(sub'(v1,i), sub'(v2,i)) ;
				   app'(f,v1,v2,i+1,n-1))

    fun appi f (v1,v2)		= appi'(f, v1, v2, 0, length(v1,v2))
    and appi'(f,v1,v2,i,0)	= ()
      | appi'(f,v1,v2,i,n)	= (f(i, sub'(v1,i), sub'(v2,i)) ;
				   appi'(f,v1,v2,i+1,n-1))

    fun appr f (v1,v2)		= appr'(f, v1, v2, length(v1,v2)-1)
    and appr'(f,v1,v2,~1)	= ()
      | appr'(f,v1,v2,i)	= (f(sub'(v1,i),sub'(v2,i)); appr'(f,v1,v2,i-1))

    fun appri f (v1,v2)		= appri'(f,v1,v2, length(v1,v2)-1)
    and appri'(f,v1,v2,~1)	= ()
      | appri'(f,v1,v2,i)	= (f(i, sub'(v1,i), sub'(v2,i)) ;
  				   appri'(f,v1,v2,i-1))

    fun foldl f x (v1,v2)	= foldl'(f, x, v1, v2, 0, length(v1,v2))
    and foldl'(f,x,v1,v2,i,0)	= x
      | foldl'(f,x,v1,v2,i,n)	= foldl'(f, f(sub'(v1,i), sub'(v2,i), x),
					 v1, v2, i+1, n-1)

    fun foldli f x (v1,v2)	= foldli'(f,x,v1,v2, 0, length(v1,v2))
    and foldli'(f,x,v1,v2,i,0)	= x
      | foldli'(f,x,v1,v2,i,n)	= foldli'(f, f(i, sub'(v1,i), sub'(v2,i), x),
					  v1, v2, i+1, n-1)

    fun foldr f x (v1,v2)	= foldr'(f, x, v1, v2, length(v1,v2)-1)
    and foldr'(f,x,v1,v2,~1)	= x
      | foldr'(f,x,v1,v2,i)	= foldr'(f, f(sub'(v1,i), sub'(v2,i), x),
					 v1, v2, i-1)

    fun foldri f x (v1,v2)	= foldri'(f, x, v1, v2, length(v1,v2))
    and foldri'(f,x,v1,v2,~1)	= x
      | foldri'(f,x,v1,v2,i)	= foldri'(f, f(i, sub'(v1,i), sub'(v2,i), x),
					  v1, v2, i-1)

    fun all f (v1,v2)		= all'(f, v1, v2, 0, length(v1,v2))
    and all'(f,v1,v2,i,n) 	= n = 0 orelse ( f(sub'(v1,i), sub'(v2,i))
					andalso  all'(f,v1,v2,i+1,n-1) )

    fun alli f (v1,v2)		= alli'(f, v1, v2, 0, length(v1,v2))
    and alli'(f,v1,v2,i,n) 	= n = 0 orelse ( f(i, sub'(v1,i), sub'(v2,i))
					andalso  alli'(f,v1,v2,i+1,n-1) )

    fun exists f (v1,v2)	= exists'(f, v1, v2, 0, length(v1,v2))
    and exists'(f,v1,v2,i,n)	= n <> 0 andalso ( f(sub'(v1,i), sub'(v2,i))
					 orelse  exists'(f,v1,v2,i+1,n-1) )

    fun existsi f (v1,v2)	= existsi'(f, v1, v2, 0, length(v1,v2))
    and existsi'(f,v1,v2,i,n)	= n <> 0 andalso ( f(i, sub'(v1,i), sub'(v2,i))
					 orelse  existsi'(f,v1,v2,i+1,n-1) )

    fun find  f (v1,v2)		= find'(f, v1, v2, 0, length(v1,v2))
    and find'(f,v1,v2,i,0)	= NONE
      | find'(f,v1,v2,i,n)	= let val xy = (sub'(v1,i), sub'(v2,i)) in
				      if f xy then SOME xy
					      else find'(f,v1,v2,i+1,n-1)
				  end

    fun findi  f (v1,v2)	= findi'(f, v1, v2, 0, length(v1,v2))
    and findi'(f,v1,v2,i,0)	= NONE
      | findi'(f,v1,v2,i,n)	= let val ixy = (i, sub'(v1,i), sub'(v2,i)) in
				      if f ixy then SOME ixy
					      else findi'(f,v1,v2,i+1,n-1)
				  end
  end


(*****************************************************************************
 * VectorSlice
 *****************************************************************************)

structure VectorSlice =
struct
    type 'a slice		= 'a Vector.vector * int * int

    fun base sl			= sl : 'a slice
    fun length (v,i,n)		= n
    fun isEmpty sl		= length sl = 0
    fun sub((v,i,n), j)		= if j < 0 orelse n <= j then raise Subscript
							 else Vector.sub(v, i+j)

    fun subslice(sl, i, NONE)	= subslice(sl, i, SOME(length sl - i))
      | subslice(sl, i, SOME n)	= (if i < 0 orelse n < 0 orelse length sl < i+n
				   then raise Subscript
				   else (#1 sl, #2 sl + i, n)
				  ) handle Overflow => raise Subscript
    fun full v			= (v, 0, Vector.length v)
    fun slice(v, i, sz)		= subslice(full v, i, sz)

    fun vector sl		= Vector.tabulate(length sl, fn i => sub (sl,i))
    fun concat l		= Vector.concat(List.map vector l)

    fun getItem sl		= if isEmpty sl then NONE else
				  SOME(sub(sl, 0), subslice(sl, 1, NONE))

    fun appi  f  sl		= appi'(f, sl, 0)
    and appi'(f, sl, i)		= if i = length sl then () else
				  (f(i, sub(sl, i)); appi'(f, sl, i+1))

    fun foldli f init sl	= foldli'(f, init, sl, 0)
    and foldli'(f, x, sl, i)	= if i = length sl then x else
				  foldli'(f, f(i, sub(sl, i), x), sl, i+1)
    fun foldri f init sl	= foldri'(f, init, sl, length sl)
    and foldri'(f, x, sl, i)	= if i = 0 then x else
				  foldli'(f, f(i-1, sub(sl, i-1), x), sl, i-1)

    fun mapi f sl		= Vector.fromList
				      (foldri (fn (i,a,l) => f(i,a)::l) [] sl)

    fun findi  f  sl		= findi'(f, sl, 0)
    and findi'(f, sl, i)	= if i = length sl then NONE
				  else if f(i, sub(sl, i))
				  then SOME(i, sub(sl, i))
				  else findi'(f, sl, i+1)

    fun app f sl		= appi (f o #2) sl
    fun map f sl		= mapi (f o #2) sl
    fun foldl f init sl		= foldli (fn (_, a, x) => f(a, x)) init sl
    fun foldr f init sl		= foldri (fn (_, a, x) => f(a, x)) init sl
    fun find f sl		= Option.map #2 (findi (f o #2) sl)
    fun exists f sl		= Option.isSome(find f sl)
    fun all f sl		= Bool.not(exists (Bool.not o f) sl)

    fun collate f ((v1,i1,0),  (v2,i2,0))  = EQUAL
      | collate f ((v1,i1,0),  (v2,i2,n2)) = LESS
      | collate f ((v1,i1,n1), (v2,i2,0) ) = GREATER
      | collate f ((v1,i1,n1), (v2,i2,n2)) =
	case f(Vector.sub(v1, i1), Vector.sub(v2, i2))
	  of EQUAL => collate f ((v1, i1+1, n1-1), (v2, i2+1, n2-1))
	   | other => other
end


(*****************************************************************************
 * Array
 *****************************************************************************)

structure Array =
  struct
    open Array
    val sub' = sub

    fun vector a		= Vector.tabulate(length a, fn i => sub(a,i))

    fun copy{src, dst, di}	= if di < 0 orelse length dst < di + length src
				  then raise Subscript
				  else copy'(src, dst, di, 0)
    and copy'(src, dst, di, i)	= if i = length src then () else
				  ( update(dst, di+i, sub(src, i))
				  ; copy'(src, dst, di, i+1)
				  )

    fun copyVec{src, dst, di}	= if di < 0
				  orelse length dst < di + Vector.length src
				  then raise Subscript
				  else copyVec'(src, dst, di, 0)
    and copyVec'(src,dst,di, i)	= if i = Vector.length src then () else
				  ( update(dst, di+i, Vector.sub(src, i))
				  ; copyVec'(src, dst, di, i+1)
				  )

    type 'a t		= 'a array

    fun fromVector v	= tabulate(Vector.length v, fn i => Vector.sub(v,i))
    fun toVector a	= Vector.tabulate(length a, fn i => sub(a,i))

    fun toList a		= toList'(a, length a - 1, [])
    and toList'(a, ~1, xs)	= xs
      | toList'(a, i, xs)	= toList'(a, i-1, sub(a,i)::xs)

    fun swap(a,i,j)		= let val x = sub(a,i) in
				      update(a, i, sub(a,j)) ;
				      update(a, j, x)
				  end

    fun rev a			= let fun rev'(i,j) =
				      if i >= j then () else
					  (swap(a,i,j) ; rev'(i+1, j-1))
				  in rev'(0, length a - 1) end

    fun appr  f v		= appr'(f, v, length v - 1)
    and appr'(f,v,~1)		= ()
      | appr'(f,v,i)		= (f(sub(v,i)) ; appr'(f,v,i-1))

    fun appri f v		= appri'(f, v, length v - 1)
    and appri'(f,v,~1)		= ()
      | appri'(f,v,i)		= (f(i, sub(v,i)) ; appri'(f,v,i-1))

    fun all  f a		= all'(f,a,0)
    and all'(f,a,i) 		= i = length a orelse
				  f(sub(a,i)) andalso all'(f,a,i+1)

    fun alli  f a		= alli'(f,a,0)
    and alli'(f,a,i) 		= i = length a orelse
				  f(i,sub'(a,i)) andalso alli'(f,a,i+1)

    fun exists  f a		= exists'(f,a,0)
    and exists'(f,a,i) 		= i <> length a andalso
				  (f(sub'(a,i)) orelse exists'(f,a,i+1))

    fun existsi  f a		= existsi'(f,a,0)
    and existsi'(f,a,i) 	= i <> length a andalso
				  (f(i,sub'(a,i)) orelse existsi'(f,a,i+1))

    fun find  f a		= find'(f,a,0)
    and find'(f,a,i) 		= if i = length a then NONE else
				  let val x = sub(a,i) in
				      if f x then SOME x
					     else find'(f,a,i+1)
				  end

    fun findi  f a		= findi'(f,a,0)
    and findi'(f,a,i) 		= if i = length a then NONE else
				  let val x = sub(a,i) in
				      if f(i,x) then SOME x
					        else findi'(f,a,i+1)
				  end

    fun contains v x		= contains'(v,x,0)
    and contains'(v,x,i)	= i <> length v andalso
				  (x = sub(v,i) orelse contains'(v,x,i+1))
    fun notContains v x		= Bool.not(contains'(v,x,0))

    fun equal  eq (a1,a2)	= length a1 = length a2 andalso
				  equal'(eq,a1,a2,0)
    and equal'(eq,a1,a2,i)	= i = length a1 orelse
				  eq(sub(a1,i), sub(a2,i)) andalso
				  equal'(eq,a1,a2,i+1)

    fun collate  cmp (a1,a2)	= collate'(cmp,a1,a2,0)
    and collate'(cmp,a1,a2,i)	= case (i = length a1, i = length a2)
				    of (true,  true)  => EQUAL
				     | (true,  false) => LESS
				     | (false, true)  => GREATER
				     | (false, false) =>
				  case cmp(sub(a1,i), sub(a2,i))
				    of EQUAL => collate'(cmp,a1,a2,i+1)
				     | other => other

    fun isSorted cmp a		= isSorted'(cmp,a,1)
    and isSorted'(cmp,a,i)	= i >= length a orelse
				  cmp(sub(a,i-1), sub(a,i)) <> GREATER
				  andalso isSorted'(cmp,a,i+1)

    fun sort cmp a =
    let
	fun partition(i,j,p) =
	    if i = j then j
	    else if cmp(sub(a,i),p) <> GREATER then partition(i+1,j,p)
	    else if cmp(p,sub(a,j-1)) = LESS   then partition(i,j-1,p)
	    else (swap(a,i,j-1); partition(i+1,j-1,p))

	fun sort(i,j) =
	    if j-i <= 1 then ()
	    else if j-i = 2 then
		if cmp(sub(a,i), sub(a,j-1)) <> GREATER
		then ()
		else swap(a,i,j-1)
	    else let
		val mid = (i+j) div 2
		val _ = if cmp(sub(a,i), sub(a,mid)) <> GREATER then ()
			else swap(a,i,mid)
		val _ = if cmp(sub(a,mid), sub(a,j-1)) <> GREATER then ()
			else (swap(a,mid,j-1);
			      if cmp(sub(a,i),sub(a,mid)) <> GREATER then ()
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

structure Time =
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

type ('a,'b) pair = 'a * 'b

exception Assert = General.Assert
val op :=:	= General.:=:
val id		= General.id
val const	= General.const
val curry	= General.curry
val uncurry	= General.uncurry
val flip	= General.flip
val inverse	= General.inverse
val assert	= General.assert

exception Alt	= Alt.Alt
val fst		= Alt.fst
val snd		= Alt.snd
val isFst	= Alt.isFst
val isSnd	= Alt.isSnd
val isNone	= Option.isNone
val appr	= List.appr
