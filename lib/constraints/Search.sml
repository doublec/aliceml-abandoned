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

structure Search :> SEARCH =
    struct
	type order = 'a * 'a -> unit
	    
	local
	    fun doSearchOne s =
		(case Space.ask s of
		     FAILED          => NONE
		   | SUCCEEDED       => SOME(s)
		   | ALTERNATIVES(n) =>
			 let
			     val c = Space.clone s
			 in
			     (Space.commit(s, SINGLE(1));
			      (case doSearchOne s of
				   NONE => (Space.commit(c, RANGE(2, n));
					    doSearchOne c)
				 | s    => s)
			 end)
	in
	    fun searchOne p =
		     (case doSearchOne (Space.space p) of
			  NONE    => NONE
			| SOME(s) => SOME(Space.merge s))
	end

	local
	    fun doSearchAll s =
		     (case Space.ask s of
			  FAILED          => nil
			| SUCCEEDED       => [s]
			| ALTERNATIVES(n) =>
			      let
				  val c = Space.clone s
			      in
				  (Space.commit(s, SINGLE(1));
				   Space.commit(c, RANGE(2, n));
				   doSearchAll(s) @ doSearchAll(c))
			      end)
	in
	    fun searchAll p = List.map Space.merge (doSearchAll (Space.space p))
	end
		     
	fun searchBest(p, o) =
		     let
			 fun constrain(s, bs) =
			     let
				 val or = Space.merge (Space.clone bs)
			     in
				 Space.inject(s, fn nr => o(or, nr))
			     end
			 fun doSearchBest(s, bs) =
			     (case Space.ask s of
				  FAILED          => bs
				| SUCCEEDED       => s
				| ALTERNATIVES(n) =>
				      let
					  val c = Space.clone s
				      in
					  (Space.commit(s, SINGLE(1));
					   Space.commit(c, RANGE(2, n));
					   let
					       val nbs = doSearchBest(s, bs)
					   in
					       (case Space.eq(bs, nbs) of
						    false => (constrain(c, nbs);
							      doSearchBest(c, nbs))
						  | true  => doSearchBest(c, nbs))
					   end)
				      end)
			 val s  = Space.space p
			 val bs = doSearchBest(s, s)
		     in
			 (case Space.ask bs of
			      SUCCEEDED => SOME(bs)
			    | _         => NONE)
		     end
    end
