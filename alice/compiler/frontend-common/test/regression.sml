__prebound P


(* Toplevel *)

infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

type unit      = {}
type int       = P.int
type word      = P.word
type real      = P.real
type char      = P.char
type string    = P.string
type substring = string * int * int
type exn       = P.exn

datatype bool      = datatype P.bool
datatype 'a option = NONE | SOME of 'a
datatype order     = LESS | EQUAL | GREATER 
datatype list      = datatype P.list
datatype ref       = datatype P.ref

type 'a array  = 'a ref			(* DUMMY *)
type 'a vector = 'a array		(* DUMMY *)

exception Bind = P.Bind
exception Chr
exception Div
exception Domain
exception Fail of string 
exception Match = P.Match
exception Option
exception Overflow
exception Size
exception Span
exception Subscript

__primitive val op= : ''a * ''a -> bool = ""

;

(* Some test junk *)

val _ = (1,2.4) : int * real

val _ = true::nil

fun f x : int = x
val _ = f(f(f 4))

fun g(x,y) : int = g(f 0, g(f x, f y))

fun cons(x: 'a, xs: 'a list) : 'a list = nil : 'a list
val _ = cons(true, cons(false, nil))

val _ = op::(true, op::(false, nil))

val _ = true::false::nil

fun f g =	(* tests whether shifting of type levels work correctly *)
    let
	fun h l = if true then h((g 0)::l) else l
    in
	h nil
    end

;

val nil = nil
val nil = true::nil
val true::nil = nil
val true::nil = true::nil

;

val (x,y,z) = {2=true, 1="hi", 3=0} : string*bool*int : {3:int,1:string,2:bool}
val {2=x,4=y:real,...} = (true,Bind,"hu",3.3)
val 2 = 3:int

;

__primitive val Thread_spawn: (unit -> unit) -> unit = "Thread.spawn"

fun g{} = {}

val _ = Thread_spawn
val _ = Thread_spawn g
val _ = Thread_spawn: (unit -> unit) -> unit

;

datatype blub = Blub
(*val rec rec f = fn() => ()
val rec f = fn() => () and rec g = fn() => ()
val rec f as g = fn() => ()
val rec Blub = fn() => ()
val 'a rec f = fn x => x
*)
(* val 'a x = let val 'a y = () in () end  (* error detected *) *)

;

(* ... *)

;

(* DUMMY int *)

fun ~x  = x : int
fun x+y = (x:int ; y:int)
fun x-y = x+y
fun x*y = x+y
fun x div y = x*y
fun x mod y = x-y

fun x<y = true
fun x>y = false
fun x<=y = true
fun x>=y = false
fun x<>y = true

;

(* Record update *)

val r1 = {a = 1, b = 2, c = 3}
val r2 = {r1 where b = 4}

fun f x = {x where 1 = 2}

;

(* Test signature matching *)

signature S =
  sig
    type t
    val x : t
  end

structure S :> S =
  struct
    type t = int
    val x : int = 0
  end


signature SIG =
  sig
    type t
    type u = int
    datatype b = T | F
    type v
    type w = v
    val x : t
    datatype a = A of c
    and      c = C of a
    structure S : sig type t end
    type st = S.t
    structure M : sig structure N : sig type t end end
  end

structure Str :> SIG =
  struct
    type t = bool
    type u = int
    datatype b = T | F
    datatype w = W
    type v = w
    val x = true
    structure S = struct type t end
    datatype c = C of a
    and      a = A of c
    type st = S.t
    structure M = struct structure N = struct type u type t = u end end
  end

;


(* Signature intersection *)

signature S1 =
  sig
    type t
    type u = t
    val t : t
    val u : u
    structure M : sig type t end
    type v = M.t
    val Mt : M.t
    val v : v
  end

signature S2 = S1
  where type t   = int
    and type M.t = bool


;

(* Sharing constraints *)

signature TOKENS = sig type t end

signature DATA =
  sig
    structure Token : TOKENS
  end

signature LRVALS =
  sig
    structure Tokens : TOKENS
    structure Data : DATA
    sharing type Data.Token.t = Tokens.t
  end


signature TOKEN =
  sig
    structure LrTable : sig end
  end

signature PARSER =
  sig
     structure LrTable : sig end
     structure Token : TOKEN
     sharing LrTable = Token.LrTable
  end

;


(* Checking eta conversion during type comparison *)

structure M =
  struct
    datatype ('a,'b) t = T
  end

signature S =
  sig
    type ('a,'b) t = ('a,'b) M.t
  end

structure M' :> S = M

;


(* Manifest structures *)

structure M =
  struct
    datatype t = T
  end

signature S =
  sig
    structure X = M
    val x : M.t
  end

structure N :> S =
  struct
    structure X = M
    val x = X.T
  end

val M.T = N.x


signature LR_TABLE =
    sig
        datatype ('a,'b) pairlist = EMPTY | PAIR of 'a * 'b * ('a,'b) pairlist
	datatype state = STATE
	datatype term = T
	datatype nonterm = NT
	datatype action = ERROR
	type table
	val mkLrTable : ('a,'b) pairlist
    end

signature TOKEN =
    sig
	structure LrTable : LR_TABLE
        datatype ('a,'b) token = TOKEN of ('a * 'b * 'b)
	val sameToken : ('a,'b) token
    end

signature LR_PARSER =
    sig
	structure LrTable : LR_TABLE
	structure Token : TOKEN where LrTable = LrTable
    end

signature LEXER =
   sig
       structure UserDeclarations :
	   sig
	        type ('a,'b) token
		type pos
		type svalue
	   end
   end

signature PARSER_DATA =
   sig
	type pos
	type svalue
 	type arg
 
	type result

	structure LrTable : LR_TABLE
	structure Token : TOKEN where LrTable = LrTable
    end

functor Join(structure ParserData: PARSER_DATA
	     structure Lex : LEXER
where type UserDeclarations.svalue = ParserData.svalue
where type UserDeclarations.pos = ParserData.pos
where type ('a,'b) UserDeclarations.token = ('a,'b) ParserData.Token.token
	     structure LrParser : LR_PARSER
where type ('a,'b) LrTable.pairlist = ('a,'b) ParserData.LrTable.pairlist
where type LrTable.state = ParserData.LrTable.state
where type LrTable.term = ParserData.LrTable.term
where type LrTable.nonterm = ParserData.LrTable.nonterm
where type LrTable.action = ParserData.LrTable.action
where type LrTable.table = ParserData.LrTable.table

where type ('a,'b) Token.LrTable.pairlist = ('a,'b) ParserData.Token.LrTable.pairlist
where type Token.LrTable.state = ParserData.Token.LrTable.state
where type Token.LrTable.term = ParserData.Token.LrTable.term
where type Token.LrTable.nonterm = ParserData.Token.LrTable.nonterm
where type Token.LrTable.action = ParserData.Token.LrTable.action
where type Token.LrTable.table = ParserData.Token.LrTable.table
where type ('a,'b) Token.token = ('a,'b) ParserData.Token.token
)
  = struct end

functor Join(structure ParserData: PARSER_DATA
	     structure Lex : LEXER
where type UserDeclarations.svalue = ParserData.svalue
where type UserDeclarations.pos = ParserData.pos
where type ('a,'b) UserDeclarations.token = ('a,'b) ParserData.Token.token
	     structure LrParser : LR_PARSER
		where LrTable = ParserData.LrTable
		where Token = ParserData.Token
)
  = struct end
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

    fun exnName ex    = "ex" : string
    fun exnMessage ex = exnName ex

    datatype order = LESS | EQUAL | GREATER

    fun !(ref v) = v
    __primitive val op:= : 'a ref * 'a -> unit = ""

    fun (f o g) a  = f(g a)
    fun a before b = a
    fun ignore a   = () 

  end

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
	let fun rev'( nil,  ys) = ys
	      | rev'(x::xs, ys) = rev'(xs, x::ys)
	in rev'(l,nil) end

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
	    val ys = filter f xs
	in if b then x::ys else ys end

    fun partition f l =
	let fun partition( nil,  pos, neg) = (rev pos, rev neg)
	      | partition(x::xs, pos, neg) =
		if f x then partition(xs, x::pos, neg)
		       else partition(xs, pos, x::neg)
	in partition(l,nil,nil) end

    fun foldl f b   nil   = b
      | foldl f b (x::xs) = f(x, foldl f b xs)

    fun foldr f b   nil   = b
      | foldr f b (x::xs) = foldr f (f(x,b)) xs

    fun exists f   nil   = false
      | exists f (x::xs) = f x orelse exists f xs

    fun all f   nil   = true
      | all f (x::xs) = f x andalso all f xs

    fun tabulate(n, f) =
	let fun tabulate(i,l) = if i = n then rev l
					 else tabulate(i+1, (f i)::l)
	in if n >= 0 then tabulate(0, nil) else raise Size end

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





fun s1 ^ s2 = s1 : string
fun concat  nil   = ""
  | concat(l::ls) = l ^ concat ls

structure Int =
  struct
    fun toString n = "0" : string
  end

;


(* lib-base-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *)

signature LIB_BASE =
  sig

    exception Unimplemented of string
        (* raised to report unimplemented features *)
    exception Impossible of string
        (* raised to report internal errors *)

    exception NotFound
        (* raised by searching operations *)

    val failure : {module : string, func : string, msg : string} -> 'a
        (* raise the exception Fail with a standard format message. *)

    val version : {date : string, system : string, version_id : int list}
    val banner : string

  end (* LIB_BASE *)

;

(* lib-base.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *)

structure LibBase : LIB_BASE =
  struct

  (* raised to report unimplemented features *)
    exception Unimplemented of string

  (* raised to report internal errors *)
    exception Impossible of string

  (* raised by searching operations *)
    exception NotFound

  (* raise the exception Fail with a standard format message. *)
    fun failure {module, func, msg} =
          raise (Fail(concat[module, ".", func, ": ", msg]))

    val version = {
            date = "June 1, 1996" : string, 
            system = "SML/NJ Library" : string,
            version_id = [1, 0] : int list
          }

    fun f ([], l) = l
      | f ([x : int], l) = (Int.toString x)::l
      | f (x::r, l) = (Int.toString x) :: "." :: f(r, l)

    val banner = concat (
            #system version :: ", Version " ::
            f (#version_id version, [", ", #date version]))

  end (* LibBase *)

;

(* ord-key-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Abstract linearly ordered keys.
 *
 *)

signature ORD_KEY =
  sig
    type ord_key

    val compare : ord_key * ord_key -> order

  end (* ORD_KEY *)

;

structure K : ORD_KEY =
  struct
    datatype ord_key = C1 | C2 | C3

    fun compare(C1,C2) = LESS
      | compare(C1,C3) = LESS
      | compare(C2,C3) = LESS
      | compare(C3,C2) = GREATER
      | compare(C3,C1) = GREATER
      | compare(C2,C1) = GREATER
      | compare  _     = EQUAL
  end

structure Key = K

;

(* ord-map-sig.sml
 *
 * COPYRIGHT (c) 1996 by AT&T Research.  See COPYRIGHT file for details.
 *
 * Abstract signature of an applicative-style finite maps (dictionaries)
 * structure over ordered monomorphic keys.
 *)

signature ORD_MAP =
  sig

    structure Key : ORD_KEY

    type 'a map

    val empty : 'a map
        (* The empty map *)

    val insert  : 'a map * Key.ord_key * 'a -> 'a map
    val insert' : ((Key.ord_key * 'a) * 'a map) -> 'a map
        (* Insert an item. *)

    val find : 'a map * Key.ord_key -> 'a option
        (* Look for an item, return NONE if the item doesn't exist *)

    val remove : 'a map * Key.ord_key -> 'a map * 'a
        (* Remove an item, returning new map and value removed.
         * Raises LibBase.NotFound if not found.
         *)

    val numItems : 'a map ->  int
        (* Return the number of items in the map *)

    val listItems  : 'a map -> 'a list
    val listItemsi : 'a map -> (Key.ord_key * 'a) list
        (* Return an ordered list of the items (and their keys) in the map.
         *)

    val collate : ('a * 'a -> order) -> ('a map * 'a map) -> order
        (* given an ordering on the map's range, return an ordering
         * on the map.
         *)

    val unionWith  : ('a * 'a -> 'a) -> ('a map * 'a map) -> 'a map
    val unionWithi : (Key.ord_key * 'a * 'a -> 'a) -> ('a map * 'a map) -> 'a map
        (* return a map whose domain is the union of the domains of the two input
         * maps, using the supplied function to define the map on elements that
         * are in both domains.
         *)

    val intersectWith  : ('a * 'b -> 'c) -> ('a map * 'b map) -> 'c map
    val intersectWithi : (Key.ord_key * 'a * 'b -> 'c) -> ('a map * 'b map) -> 'c map
        (* return a map whose domain is the intersection of the domains of the
         * two input maps, using the supplied function to define the range.
         *)

    val app  : ('a -> unit) -> 'a map -> unit
    val appi : ((Key.ord_key * 'a) -> unit) -> 'a map -> unit
        (* Apply a function to the entries of the map in map order. *)

    val map  : ('a -> 'b) -> 'a map -> 'b map
    val mapi : (Key.ord_key * 'a -> 'b) -> 'a map -> 'b map
        (* Create a new map by applying a map function to the
         * name/value pairs in the map.
         *)

    val foldl  : ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldli : (Key.ord_key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
        (* Apply a folding function to the entries of the map
         * in increasing map order.
         *)

    val foldr  : ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldri : (Key.ord_key * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
        (* Apply a folding function to the entries of the map
         * in decreasing map order.
         *)

    val filter  : ('a -> bool) -> 'a map -> 'a map
    val filteri : (Key.ord_key * 'a -> bool) -> 'a map -> 'a map
        (* Filter out those elements of the map that do not satisfy the
         * predicate.  The filtering is done in increasing map order.
         *)

    val mapPartial  : ('a -> 'b option) -> 'a map -> 'b map
    val mapPartiali : (Key.ord_key * 'a -> 'b option) -> 'a map -> 'b map
        (* map a partial function over the elements of a map in increasing
         * map order.
         *)

  end (* ORD_MAP *)

;

(* binary-map-fn.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *)

functor BinaryMapFn (K : ORD_KEY) : ORD_MAP =
  struct

    structure Key = K

    (*
    **  val weight = 3
    **  fun wt i = weight * i
    *)
    fun wt (i : int) = i + i + i

    datatype 'a map
      = E 
      | T of {
          key : K.ord_key, 
          value : 'a, 
          cnt : int, 
          left : 'a map, 
          right : 'a map
        }

    val empty = E

    fun numItems E = 0
      | numItems (T{cnt,...}) = cnt

local
    fun N(k,v,E,E) = T{key=k,value=v,cnt=1,left=E,right=E}
      | N(k,v,E,r as T n) = T{key=k,value=v,cnt=1+(#cnt n),left=E,right=r}
      | N(k,v,l as T n,E) = T{key=k,value=v,cnt=1+(#cnt n),left=l,right=E}
      | N(k,v,l as T n,r as T n') = 
          T{key=k,value=v,cnt=1+(#cnt n)+(#cnt n'),left=l,right=r}

    fun single_L (a,av,x,T{key=b,value=bv,left=y,right=z,...}) = 
          N(b,bv,N(a,av,x,y),z)
      | single_L _ = raise Match
    fun single_R (b,bv,T{key=a,value=av,left=x,right=y,...},z) = 
          N(a,av,x,N(b,bv,y,z))
      | single_R _ = raise Match
    fun double_L (a,av,w,T{key=c,value=cv,left=T{key=b,value=bv,left=x,right=y,...},right=z,...}) =
          N(b,bv,N(a,av,w,x),N(c,cv,y,z))
      | double_L _ = raise Match
    fun double_R (c,cv,T{key=a,value=av,left=w,right=T{key=b,value=bv,left=x,right=y,...},...},z) = 
          N(b,bv,N(a,av,w,x),N(c,cv,y,z))
      | double_R _ = raise Match

    fun T' (k,v,E,E) = T{key=k,value=v,cnt=1,left=E,right=E}
      | T' (k,v,E,r as T{right=E,left=E,...}) =
          T{key=k,value=v,cnt=2,left=E,right=r}
      | T' (k,v,l as T{right=E,left=E,...},E) =
          T{key=k,value=v,cnt=2,left=l,right=E}

      | T' (p as (_,_,E,T{left=T _,right=E,...})) = double_L p
      | T' (p as (_,_,T{left=E,right=T _,...},E)) = double_R p

        (* these cases almost never happen with small weight*)
      | T' (p as (_,_,E,T{left=T{cnt=ln,...},right=T{cnt=rn,...},...})) =
          if ln < rn then single_L p else double_L p
      | T' (p as (_,_,T{left=T{cnt=ln,...},right=T{cnt=rn,...},...},E)) =
          if ln > rn then single_R p else double_R p

      | T' (p as (_,_,E,T{left=E,...})) = single_L p
      | T' (p as (_,_,T{right=E,...},E)) = single_R p

      | T' (p as (k,v,l as T{cnt=ln,left=ll,right=lr,...},
                      r as T{cnt=rn,left=rl,right=rr,...})) =
          if rn >= wt ln then (*right is too big*)
            let val rln = numItems rl
                val rrn = numItems rr
            in
              if rln < rrn then  single_L p  else  double_L p
            end
        
          else if ln >= wt rn then  (*left is too big*)
            let val lln = numItems ll
                val lrn = numItems lr
            in
              if lrn < lln then  single_R p  else  double_R p
            end
    
          else T{key=k,value=v,cnt=ln+rn+1,left=l,right=r}

    local
      fun min (T{left=E,key,value,...}) = (key,value)
        | min (T{left,...}) = min left
        | min _ = raise Match
  
      fun delmin (T{left=E,right,...}) = right
        | delmin (T{key,value,left,right,...}) = T'(key,value,delmin left,right)
        | delmin _ = raise Match
    in
      fun delete' (E,r) = r
        | delete' (l,E) = l
        | delete' (l,r) = let val (mink,minv) = min r in
            T'(mink,minv,l,delmin r)
          end
    end
in
    fun mkDict () = E
    
    fun insert (E,x,v) = T{key=x,value=v,cnt=1,left=E,right=E}
      | insert (T(set as {key,left,right,value,...}),x,v) =
          case K.compare (key,x) of
            GREATER => T'(key,value,insert(left,x,v),right)
          | LESS => T'(key,value,left,insert(right,x,v))
          | _ => T{key=x,value=v,left=left,right=right,cnt= #cnt set}
    fun insert' ((k, x), m) = insert(m, k, x)

    fun find (set, x) = let 
          fun mem E = NONE
            | mem (T(n as {key,left,right,...})) = (case K.compare (x,key)
                 of GREATER => mem right
                  | EQUAL => SOME(#value n)
                  | LESS => mem left
                (* end case *))
          in
            mem set
          end

    fun remove (E,x) = raise LibBase.NotFound
      | remove (set as T{key,left,right,value,...},x) = (
          case K.compare (key,x)
           of GREATER => let
                val (left', v) = remove(left, x)
                in
                  (T'(key, value, left', right), v)
                end
            | LESS => let
                val (right', v) = remove (right, x)
                in
                  (T'(key, value, left, right'), v)
                end
            | _ => (delete'(left,right),value)
          (* end case *))

    fun listItems d = let
          fun d2l (E, l) = l
            | d2l (T{key,value,left,right,...}, l) =
                d2l(left, value::(d2l(right,l)))
          in
            d2l (d,[])
          end

    fun listItemsi d = let
          fun d2l (E, l) = l
            | d2l (T{key,value,left,right,...}, l) =
                d2l(left, (key,value)::(d2l(right,l)))
          in
            d2l (d,[])
          end

    local
      fun next ((t as T{right, ...})::rest) = (t, left(right, rest))
        | next _ = (E, [])
      and left (E, rest) = rest
        | left (t as T{left=l, ...}, rest) = left(l, t::rest)
    in
    fun collate cmpRng (s1, s2) = let
          fun cmp (t1, t2) = (case (next t1, next t2)
                 of ((E, _), (E, _)) => EQUAL
                  | ((E, _), _) => LESS
                  | (_, (E, _)) => GREATER
                  | ((T{key=x1, value=y1, ...}, r1), (T{key=x2, value=y2, ...}, r2)) => (
                      case Key.compare(x1, x2)
                       of EQUAL => (case cmpRng(y1, y2)
                             of EQUAL => cmp (r1, r2)
                              | order => order
                            (* end case *))
                        | order => order
                      (* end case *))
                (* end case *))
          in
            cmp (left(s1, []), left(s2, []))
          end
    end (* local *)

    fun appi f d = let
          fun app' E = ()
            | app' (T{key,value,left,right,...}) = (
                app' left; f(key, value); app' right)
          in
            app' d
          end
    fun app f d = let
          fun app' E = ()
            | app' (T{value,left,right,...}) = (
                app' left; f value; app' right)
          in
            app' d
          end

    fun mapi f d = let
          fun map' E = E
            | map' (T{key,value,left,right,cnt}) = let
                val left' = map' left
                val value' = f(key, value)
                val right' = map' right
                in
                  T{cnt=cnt, key=key, value=value', left = left', right = right'}
                end
          in
            map' d
          end
    fun map f d = mapi (fn (_, x) => f x) d

    fun foldli f init d = let
          fun fold (E, v) = v
            | fold (T{key,value,left,right,...}, v) =
                fold (right, f(key, value, fold(left, v)))
          in
            fold (d, init)
          end
    fun foldl f init d = foldli (fn (_, v, accum) => f (v, accum)) init d

    fun foldri f init d = let
          fun fold (E,v) = v
            | fold (T{key,value,left,right,...},v) =
                fold (left, f(key, value, fold(right, v)))
          in
            fold (d, init)
          end
    fun foldr f init d = foldri (fn (_, v, accum) => f (v, accum)) init d

(** To be implemented **
    val filter  : ('a -> bool) -> 'a map -> 'a map
    val filteri : (Key.ord_key * 'a -> bool) -> 'a map -> 'a map
**)

    end (* local *)

(* the following are generic implementations of the unionWith and intersectWith
 * operetions.  These should be specialized for the internal representations
 * at some point.
 *)
    fun unionWith f (m1, m2) = let
          fun ins  f (key, x, m) = (case find(m, key)
                 of NONE => insert(m, key, x)
                  | (SOME x') => insert(m, key, f(x, x'))
                (* end case *))
          in
            if (numItems m1 > numItems m2)
              then foldli (ins (fn (a, b) => f (b, a))) m1 m2
              else foldli (ins f) m2 m1
          end
    fun unionWithi f (m1, m2) = let
          fun ins f (key, x, m) = (case find(m, key)
                 of NONE => insert(m, key, x)
                  | (SOME x') => insert(m, key, f(key, x, x'))
                (* end case *))
          in
            if (numItems m1 > numItems m2)
              then foldli (ins (fn (k, a, b) => f (k, b, a))) m1 m2
              else foldli (ins f) m2 m1
          end

    fun intersectWith f (m1, m2) = let
        (* iterate over the elements of m1, checking for membership in m2 *)
          fun intersect f (m1, m2) = let
                fun ins (key, x, m) = (case find(m2, key)
                       of NONE => m
                        | (SOME x') => insert(m, key, f(x, x'))
                      (* end case *))
                in
                  foldli ins empty m1
                end
          in
            if (numItems m1 > numItems m2)
              then intersect f (m1, m2)
              else intersect (fn (a, b) => f(b, a)) (m2, m1)
          end
    fun intersectWithi f (m1, m2) = let
        (* iterate over the elements of m1, checking for membership in m2 *)
          fun intersect f (m1, m2) = let
                fun ins (key, x, m) = (case find(m2, key)
                       of NONE => m
                        | (SOME x') => insert(m, key, f(key, x, x'))
                      (* end case *))
                in
                  foldli ins empty m1
                end
          in
            if (numItems m1 > numItems m2)
              then intersect f (m1, m2)
              else intersect (fn (k, a, b) => f(k, b, a)) (m2, m1)
          end

  (* this is a generic implementation of filter.  It should
   * be specialized to the data-structure at some point.
   *)
    fun filter predFn m = let
          fun f (key, item, m) = if predFn item
                then insert(m, key, item)
                else m
          in
            foldli f empty m
          end
    fun filteri predFn m = let
          fun f (key, item, m) = if predFn(key, item)
                then insert(m, key, item)
                else m
          in
            foldli f empty m
          end

  (* this is a generic implementation of mapPartial.  It should
   * be specialized to the data-structure at some point.
   *)
    fun mapPartial f m = let
          fun g (key, item, m) = (case f item
                 of NONE => m
                  | (SOME item') => insert(m, key, item')
                (* end case *))
          in
            foldli g empty m
          end
    fun mapPartiali f m = let
          fun g (key, item, m) = (case f(key, item)
                 of NONE => m
                  | (SOME item') => insert(m, key, item')
                (* end case *))
          in
            foldli g empty m
          end

  end (* functor BinaryMapFn *)

;


structure K : ORD_KEY =
  struct
    datatype ord_key = C1 | C2 | C3

    fun compare(C1,C2) = LESS
      | compare(C1,C3) = LESS
      | compare(C2,C3) = LESS
      | compare(C3,C2) = GREATER
      | compare(C3,C1) = GREATER
      | compare(C2,C1) = GREATER
      | compare  _     = EQUAL
  end

structure Test1 = BinaryMapFn(K)
structure Test2 = BinaryMapFn(K) : ORD_MAP
structure Test3 = BinaryMapFn(K) :> ORD_MAP

signature K_MAP = ORD_MAP where type Key.ord_key = K.ord_key
structure Test4 = BinaryMapFn(K) :> K_MAP
structure Test5 = BinaryMapFn(K) :> ORD_MAP where type Key.ord_key = K.ord_key

;
