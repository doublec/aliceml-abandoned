(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 1999
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure Debug :> DEBUG =
    struct
	structure Intermediate = IntermediateGrammar
	open Intermediate

	fun seqToString sep toString (x::xs) =
	    List.foldl (fn (x, s) => toString x ^ sep ^ s) (toString x) xs
	  | seqToString _ _ nil = ""

	fun listToString toString xs = seqToString ", " toString xs

	fun setToString toString xs = "{" ^ listToString toString xs ^ "}"

	fun posToString pos =
	    seqToString "." (fn x => x) (List.rev ("e"::pos))

	fun labToString (Lab (_, s)) = s

	fun idToString (Id (_, stamp, InId)) =
	    "$" ^ Stamp.toString stamp
	  | idToString (Id (_, stamp, ExId s)) =
	    s ^ "$" ^ Stamp.toString stamp

	fun longidToString (LongId (_, longid, lab)) =
	    longidToString longid ^ "." ^ labToString lab
	  | longidToString (ShortId (_, id)) = idToString id

	fun mappingToString mapping =
	    "{" ^
	    listToString (fn (pos, id) =>
			  posToString pos ^ " = " ^ idToString id) mapping ^
	    "}"

	fun substToString subst =
	    "{" ^
	    listToString (fn (id, id') =>
			  idToString id ^ " -> " ^ idToString id') subst ^ "}"

	fun litToString (IntLit i) = LargeInt.toString i
	  | litToString _ = "<lit>"

	fun patToString (WildPat _) = "_"
	  | patToString (LitPat (_, lit)) = litToString lit
	  | patToString (VarPat (_, id)) = idToString id
	  | patToString (ConPat (_, longid, NONE, _)) = longidToString longid
	  | patToString (ConPat (_, longid, SOME pat, _)) =
	    "(" ^ longidToString longid ^ " " ^ patToString pat ^ ")"
	  | patToString (RefPat (_, pat)) = "(ref " ^ patToString pat ^ ")"
	  | patToString (TupPat (_, pats)) =
	    "(" ^ listToString patToString pats ^ ")"
	  | patToString (RowPat (_, patFields, hasDots)) =
	    "{" ^
	    listToString (fn Field (_, lab, pat) =>
			  labToString lab ^ ": " ^ patToString pat) patFields ^
	    (if hasDots then ", ..." else "") ^ "}"
	  | patToString (VecPat (_, pats)) =
	    "#[" ^ listToString patToString pats ^ "]"
	  | patToString (AsPat (_, pat1, pat2)) =
	    "(" ^ patToString pat1 ^ " as " ^ patToString pat2 ^ ")"
	  | patToString (AltPat (_, pats)) =
	    "(" ^ seqToString " | " patToString pats ^ ")"
	  | patToString (NegPat (_, pat)) = "(non " ^ patToString pat ^ ")"
	  | patToString (GuardPat (_, pat, _)) =
	    "(" ^ patToString pat ^ " when <exp>)"
	  | patToString (WithPat (_, pat, _)) =
	    "(" ^ patToString pat ^ " with <decs>)"
    end
