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

import structure FD from "x-alice:/lib/constraints/FD.ozf"
import structure Space from "x-alice:/lib/constraints/Space.ozf"
import structure Explorer from "x-alice:/lib/constraints/Explorer.ozf"
import structure Search from "x-alice:/lib/constraints/Search.ozf"
import structure Tools from "x-alice:/lib/Tools.ozf"

structure Photo =
    struct
	open FD

	datatype person =
	    ALICE
	  | BERT
	  | CHRIS
	  | DEB
	  | EVAN

	val numPersons = 5

	fun personIndex ALICE = 0
	  | personIndex BERT  = 1
	  | personIndex CHRIS = 2
	  | personIndex DEB   = 3
	  | personIndex EVAN  = 4

	val prefs = [(ALICE, CHRIS), (BERT, EVAN),
		     (CHRIS, DEB), (CHRIS, EVAN),
		     (DEB, ALICE), (DEB, EVAN),
		     (EVAN, BERT)]

	fun photo () =
	    let
		val pos = fdvector(#[RANGE(1, numPersons)], numPersons)
		val ful = map
		    (fn (an, bn) =>
		     let
			 val c1     = bool ()
			 val c2     = bool ()
			 val result = bool ()
			 val zero   = fromInt 0
			 val one    = fromInt 1
			 val posA   = Vector.sub(pos, personIndex(an))
			 val posB   = Vector.sub(pos, personIndex(bn))
		     in
			 (Reified.sumC(#[1, 1, ~1], #[one, posA, posB], EQUAL, zero, c1);
			  Reified.sumC(#[1, ~1], #[posA, posB], EQUAL, one, c2);
			  Reified.sum(#[fromBool(c1), fromBool(c2)],
				      EQUAL, one, result);
			  fromBool(result))
		     end) prefs
		val sat = fd #[RANGE(0, length prefs)]
	    in
		(distinct pos;
		 sum(Vector.fromList ful, EQUAL, sat);
		 distribute(NAIVE, pos);
		 (pos, ful, sat))
	    end

	fun order((_, _, a), (_, _, b)) = lessEq(a, b);

	(* Inspect the Solution(s) *)
	(* val sol = Tools.inspect (Search.searchBest(PhotoSkript, Order)) *)
	(* This is for Visual Search *)
	val sol = Explorer.exploreBest (photo, order)
    end
