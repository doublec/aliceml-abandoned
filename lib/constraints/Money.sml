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

open FD;

fun Money () =
    let
	val vars = fdvector(#[RANGE(0,9)], 8)
    in
	(case vars of
	     #[S, E, N, D, M, O, R, Y] =>
	     let
		 val Send  = decl ()
		 val More  = decl ()
		 val Money = decl ()
	     in
		 (distinct(vars);
		  notequal(S, fd #[SINGLE(0)]);
		  notequal(M, fd #[SINGLE(0)]);
		  sumC(#[1000, 100, 10, 1],
		       #[S, E, N, D],
		       EQUAL,
		       Send);
		  sumC(#[1000, 100, 10, 1],
		       #[M, O, R, E],
		       EQUAL,
		       More);
		  sumC(#[10000, 1000, 100, 10, 1],
		       #[M, O, N, E, Y],
		       EQUAL,
		       Money);
		  plus(Send, More, Money);
		  distribute(FIRSTFAIL, vars);
		  vars)
	     end)
    end

val sol = Search.searchAll Money
