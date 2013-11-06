(* Toplevel *)

infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

;

(* DUMMY arithmetics *)

fun ~ (x:int)           = x
fun (x:int) + (y:int)   = x
fun (x:int) - (y:int)   = x
fun (x:int) * (y:int)   = x
fun (x:int) div (y:int) = x
fun (x:int) mod (y:int) = x

fun (x:int) < (y:int)   = true
fun (x:int) > (y:int)   = true
fun (x:int) <= (y:int)  = true
fun (x:int) >= (y:int)  = true
fun (x:int) <> (y:int)  = true

fun (x:string) ^ (y:string) = x

;

(* General *)

structure General =
  struct

    type unit = {}
    type exn  = exn

    exception Bind = Bind
    exception Chr
    exception Div
    exception Domain
    exception Fail of string 
    exception Match = Match
    exception Overflow
    exception Size
    exception Span
    exception Subscript

    fun exnName ex    = "ex" : string	(* dummy *)
    fun exnMessage ex = exnName ex	(* dummy *)

    datatype order = LESS | EQUAL | GREATER

    fun !(ref v) = v
    val op:=     = op:=

    fun (f o g) a  = f(g a)
    fun a before b = a
    fun ignore a   = () 

  end


open General

;

(* Option *)

structure Option =
  struct

    datatype 'a option = NONE | SOME of 'a

    exception Option

    fun getOpt(SOME v, a) = v
      | getOpt(NONE,   a) = a

    fun isSome(SOME v) = true
      | isSome NONE    = false

    fun valOf(SOME v) = v
      | valOf NONE    = raise Option

    fun filter f a = if f a then SOME a else NONE

    fun join NONE    = NONE
      | join(SOME v) = v

    fun map f  NONE    = NONE
      | map f (SOME v) = SOME(f v)

    fun mapPartial f  NONE    = NONE
      | mapPartial f (SOME v) = f v

    fun compose (f,g) a = 
	case g a
	  of NONE   => NONE
	   | SOME v => SOME(f v)

    fun composePartial (f,g) a = 
	case g a
	  of NONE   => NONE
	   | SOME v => f v

  end

datatype option = datatype Option.option

;

(* Bool *)

structure Bool =
  struct

    datatype bool = datatype bool

    fun not true  = false
      | not false = true

    fun fromString s = SOME true	(* DUMMY *)

    fun scan getc strm = NONE	(* DUMMY *)

    fun toString true  = "true"  : string
      | toString false = "false" : string

  end

;

(* List *)

structure List =
  struct

    datatype list = datatype list

    exception Empty

    fun null [] = true
      | null _  = false

    fun length xs =
	let fun length(nil,   n) = n
	      | length(x::xs, n) = length(xs,n+1)
	in length(xs,0) end

    infixr 5 @
    fun   nil   @ ys = ys
      | (x::xs) @ ys = x :: xs @ ys

    fun hd(x::xs) = x
      | hd  _     = raise Empty

    fun tl(x::xs) = xs
      | tl  _     = raise Empty

    fun last(x::nil) = x
      | last(x::xs)  = last xs
      | last  nil    = raise Empty

    fun getItem(x::xs) = SOME(x,xs)
      | getItem  nil   = NONE

    fun nth(l,i) =
	let fun nth(x::xs, 0) = x
	      | nth(x::xs, i) = nth(xs, i-1)
	      | nth(nil,   i) = raise Subscript
	in if i >= 0 then nth(l,i) else raise Subscript end

    fun rev l =
	let fun rev( nil,  ys) = ys
	      | rev(x::xs, ys) = rev(xs, x::ys)
	in rev(l,nil) end

    fun take(l,i) =
	let fun take(x::xs, 0, ys) = rev ys
	      | take(x::xs, i, ys) = take(xs, i-1, x::ys)
	      | take(nil,   i, ys) = raise Subscript
	in if i >= 0 then take(l,i,nil) else raise Subscript end

    fun drop(l,i) =
	let fun drop(l,     0) = l
	      | drop(x::xs, i) = drop(xs, i-1)
	      | drop(nil,   i) = raise Subscript
	in if i >= 0 then drop(l,i) else raise Subscript end

    fun concat  nil   = nil
      | concat(l::ls) = l @ concat ls

    fun revAppend( nil,  ys) = ys
      | revAppend(x::xs, ys) = revAppend(xs, x::ys)

    fun app f   nil   = ()
      | app f (x::xs) = (f x ; app f xs)

    fun map f   nil   = nil
      | map f (x::xs) = f x :: map f xs

    fun mapPartial f   nil   = nil
      | mapPartial f (x::xs) = 
	let val yo = f x
	    val ys = mapPartial f xs
	in case yo of NONE => ys | SOME y => y::ys end

    fun find f   nil   = NONE
      | find f (x::xs) = if f x then SOME x else find f xs

    fun filter f   nil   = nil
      | filter f (x::xs) =
	let val b  = f x
	    val ys = map f xs
	in if b then x::ys else ys end

    fun partition f l =
	let fun partition( nil,  pos, neg) = (rev pos, rev neg)
	      | partition(x::xs, pos, neg) =
		if f x then partition(xs, x::pos, neg)
		       else partition(xs, pos, x::neg)
	in partition(l,nil,nil) end

    fun foldl f b   nil   = b
      | foldl f b (x::xs) = foldl f (f(x,b)) xs

    fun foldr f b   nil   = b
      | foldr f b (x::xs) = f(x, foldr f b xs)

    fun exists f   nil   = false
      | exists f (x::xs) = f x orelse exists f xs

    fun all f   nil   = true
      | all f (x::xs) = f x andalso all f xs

    fun tabulate(n, f) =
	let fun tabulate(i,l) = if i = n then rev l
					 else tabulate(i+1, f i :: l)
	in if n >= 0 then tabulate(n, nil) else raise Size end

  end

;

(* ListPair *)

structure ListPair =
  struct

    fun zip(l1, l2) =
	let fun zip( nil,    _,   l) = List.rev l
	      | zip(  _,    nil,  l) = List.rev l
	      | zip(x::xs, y::ys, l) = zip(xs, ys, (x,y)::l)
	in zip(l1, l2, nil) end

    fun unzip l =
	let fun unzip(    nil,  l1, l2) = (List.rev l1, List.rev l2)
	      | unzip((x,y)::l, l1, l2) = unzip(l, x::l1, y::l2)
	in unzip(l, nil, nil) end

    fun map f ( nil,    _  ) = nil
      | map f (  _,    nil ) = nil
      | map f (x::xs, y::ys) = f(x,y) :: map f (xs,ys)

    fun app f ( nil,    _  ) = ()
      | app f (  _,    nil ) = ()
      | app f (x::xs, y::ys) = (f(x,y) ; app f (xs,ys))

    fun foldl f c ( nil,    _  ) = c
      | foldl f c (  _,    nil ) = c
      | foldl f c (x::xs, y::ys) = f(x, y, foldl f c (xs,ys))

    fun foldr f c ( nil,    _  ) = c
      | foldr f c (  _,    nil ) = c
      | foldr f c (x::xs, y::ys) = foldr f (f(x,y,c)) (xs,ys)

    fun exists f ( nil,    _  ) = false
      | exists f (  _,    nil ) = false
      | exists f (x::xs, y::ys) = f(x,y) orelse exists f (xs,ys)

    fun all f ( nil,    _  ) = true
      | all f (  _,    nil ) = true
      | all f (x::xs, y::ys) = f(x,y) andalso all f (xs,ys)

  end

;
