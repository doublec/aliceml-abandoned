(*
 * Authors:
 *   Andreas Rossberg <rossberg@ps.uni-sb.de>
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Andreas Rossberg, 2000
 *   Leif Kornstaedt, 2001
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

functor ComposePhases(
	structure Phase1:  PHASE
	structure Phase2:  PHASE where I = Phase1.O
	structure Context: CONTEXT
	val context1: Context.t -> Phase1.C.t
	val context2: Context.t -> Phase2.C.t
    ) : PHASE =
  struct
    structure I = Phase1.I
    structure O = Phase2.O
    structure C = Context

    fun translate context (desc, rep) =
	Phase2.translate(context2 context)
	(desc, Phase1.translate(context1 context) (desc, rep))
  end

functor ComposePhases'(structure Phase1: PHASE
		       structure Phase2: PHASE where I = Phase1.O): PHASE =
    let
	structure C: CONTEXT =
	    struct
		type t = Phase1.C.t * Phase2.C.t

		fun new () = (Phase1.C.new (), Phase2.C.new ())
		fun clone (context1, context2) =
		    (Phase1.C.clone context1, Phase2.C.clone context2)
	    end
    in
	ComposePhases(structure Phase1 = Phase1
		      structure Phase2 = Phase2
		      structure Context = C
		      fun context1 (context1, _) = context1
		      fun context2 (_, context2) = context2)
    end
