(*
 * Authors:
 *   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
 *
 * Copyright:
 *   Thorsten Brunklaus, 2000
 *
 * Last Change:
 *   $Date$ by $Author$
 *   $Revision$
 *
 *)

signature FD_COMPONENT =
    sig
	signature FD =
	    sig
		type fd
		type bool

		datatype domain_element =
		    SINGLE of int
		  | RANGE  of int * int

		type domain = domain_element vector

		datatype propagator =
		    LESS
		  | LESSEQ
		  | EQUAL
		  | NOTEQUAL
		  | GREATER
		  | GREATEREQ

		(* Allocation Functions *)
		val fd : domain -> fd
		val fdvector : domain * int -> fd vector
		val decl : unit -> fd
		val bool : unit -> bool
		val boolvector : int -> bool

		(* Conversion *)
		val toBool : fd -> bool option
		val fromBool : bool -> fd
		val toInt : fd -> int option
		val fromInt : int -> fd

		(* Standards Sums *)
		val sum : fd vector * propagator * fd -> unit
		val sumC : int vector * fd vector * propagator * fd -> unit
		val sumAC : int vector * fd vector * propagator * fd -> unit
		val sumCN : int vector * fd vector vector * propagator * fd -> unit
		val sumACN : int vector * fd vector vector * propagator * fd -> unit
		val sumD : fd vector * propagator * fd -> unit
		val sumCD : int vector * fd vector * propagator * fd -> unit

		(* Standard Propagators; Interval propagation *)
		val plus : fd * fd * fd -> unit (* X + Y =: Z *)
		val minus : fd * fd * fd -> unit (* X - Y =: Z *)
		val times : fd * fd * fd -> unit (* X * Y =: Z *)
		val power : fd * int * fd -> unit (* pow(X, I) =: Z *)
		val divI : fd * int * fd -> unit (* X divI I =: Z *)
		val modI : fd * int * fd -> unit (* X modI I =: Z *)

		(* Standard Propagators; Domain propagation *)
		val plusD : fd * fd * fd -> unit (* X + Y =: Z *)
		val minusD : fd * fd * fd -> unit (* X - Y =: Z *)
		val timesD : fd * fd * fd -> unit (* X * Y =: Z *)
		val divD : fd * int * fd -> unit (* X divD I =: Z *)
		val modD : fd * int * fd -> unit (* X modD I =: Z *)
		    
		val min : fd * fd * fd -> unit (* min(X, Y) =: Z *)
		val max : fd * fd * fd -> unit (* max(X, Y) =: Z *)
		    
		val equal : fd * fd -> unit (* X =: Y *)
		val notequal : fd * fd -> unit (* X \=: Y *)
		val distance : fd * fd * propagator * fd -> unit
		val less : fd * fd -> unit (* X <: Y *)
		val lessEq : fd * fd -> unit (* X <=: Y *)
		val greater : fd * fd -> unit (* X >: Y *)
		val greaterEq : fd * fd -> unit (* X >=: Y *)
		val disjoint : fd * int * fd * int -> unit
		val disjointC : fd * int * fd * int * bool -> unit
		val tasksOverlap : fd * int * fd * int * bool -> unit

		(* Non-Linear Propagators *)
		val distinct : fd vector -> unit
		val distinctOffset : fd vector * int vector -> unit
		val distinct2 : fd vector * int vector * fd vector * int vector -> unit
		val atMost : fd * fd vector * int -> unit
		val atLeast : fd * fd vector * int -> unit
		val exactly : fd * fd vector * int -> unit
		val element : fd * int vector * fd -> unit

		(* 0/1 Propagators *)
		val conj : bool * bool * bool -> unit
		val disj : bool * bool * bool -> unit
		val exor : bool * bool * bool -> unit
		val nega : bool * bool -> unit
		val impl : bool * bool * bool -> unit
		val equi : bool * bool * bool -> unit
		    
		(* Reified Constraints *)
		structure Reified :
		    sig
			(* Reified Variable is returned *)
			val int : domain * bool -> fd
			(* Reified Vector of Variables is returned *)
			val dom : domain * int * bool -> fd vector
			(* Same as in Oz *)
			val card : int * bool vector * int * bool -> unit
			val distance : fd * fd * propagator * fd * bool -> unit
			val sum : fd vector * propagator * fd * bool -> unit
			val sumC : int vector * fd vector * propagator * fd * bool -> unit
			val sumAC : int vector * fd vector * propagator * fd * bool -> unit
			val sumCN : int vector * fd vector vector * propagator * fd * bool -> unit
			val sumACN : int vector * fd vector vector * propagator * fd * bool -> unit
		    end

		(* Reflection *)
		structure Reflect :
		    sig
			val min : fd -> int
			val max : fd -> int
			val mid : fd -> int
			val nextLarger : fd * int -> int
			val nextSmaller : fd * int -> int
			val size : fd -> int
			val dom : fd -> domain
			val domList : fd -> int list
			val nbSusps : fd -> int
		    end

		(* Watching *)
		structure Watch :
		    sig
			val min : fd * int -> bool
			val max : fd * int -> bool
			val size : fd * int -> bool
		    end

		(* Distribution *)
		datatype dist_mode =
		    NAIVE
		  | FIRSTFAIL
		  | SPLIT

		val distribute : dist_mode * fd vector -> unit
	    end

	structure FD : FD
    end
