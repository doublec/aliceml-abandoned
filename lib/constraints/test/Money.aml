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

import structure FD     from "x-alice:/lib/constraints/FD.ozf"
import structure Space  from "x-alice:/lib/constraints/Space.ozf"
import structure Search from "x-alice:/lib/constraints/Search.ozf"
import structure Tools  from "x-alice:/lib/Tools.ozf"

structure Money =
    struct
	open FD

	fun money () =
	    let
		val digits as #[S, E, N, D, M, O, R, Y] = fdvector(#[RANGE(0,9)], 8)
		val send                                = decl ()
		val more                                = decl ()
		val money                               = decl ()
		val zero                                = fromInt 0
	    in
		(sumC(#[1000, 100, 10, 1], #[S, E, N, D], EQUAL, send);
		 sumC(#[1000, 100, 10, 1], #[M, O, R, E], EQUAL, more);
		 sumC(#[10000, 1000, 100, 10, 1], #[M, O, N, E, Y], EQUAL, money);
		 notequal(S, zero);
		 notequal(M, zero);
		 distinct(digits);
		 plus(send, more, money);
		 distribute(FIRSTFAIL, digits);
		 {S, E, N, D, M, O, R, Y})
	    end

	(* Inspect the solution *)
	val sol = Tools.inspect (Search.searchAll money)
    end
