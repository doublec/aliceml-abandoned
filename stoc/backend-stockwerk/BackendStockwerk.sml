(*
 * Author:
 *   Leif Kornstaedt <kornstae@ps.uni-sb.de>
 *
 * Copyright:
 *   Leif Kornstaedt, 2000
 *
 * Last change:
 *   $Date$ by $Author$
 *   $Revision$
 *)

structure BackendStockwerk: PHASE =
    struct
	structure C = EmptyContext
	structure I = FlatGrammar
	structure O = Pickle

	open I

	fun translateLit (IntLit i) = O.Int i
	  | translateLit (WordLit w) = O.Word w
	  | translateLit (CharLit c) = O.Char c
	  | translateLit (StringLit s) = O.String s
	  | translateLit (RealLit r) = O.Real r

	fun translateStms (ValDec (_, IdDef id, exp)::stms) =
	    translateExp (translateId id, exp, translateStms stms)
	  | translateStms (ValDec (_, Wildcard,
				   (LitExp (_, _) |
				    PrimExp (_, _) |
				    NewExp _ |
				    VarExp (_, _) |
				    TagExp (_, _, _, _) |
				    ConExp (_, _, _) |
				    RefExp _ |
				    TupExp (_, _) |
				    ProdExp (_, _) |
				    SelExp (_, _, _) |
				    VecExp (_, _) |
				    FunExp (_, _, _, _, _) |
				    TagAppExp (_, _, _, _) |
				    ConAppExp (_, StaticCon _, _) |
				    RefAppExp (_, _)))::stms) =
	    translateStms stms


	and translateExp (id, LitExp (_, lit), instrs) =
	    O.PutConst (id, translateLit lit, instrs)
	  | translateExp (id, PrimExp (_, name), instrs) =
	    O.PutConst (id, O.Prim name, instrs)
	  | translateExp (id, NewExp _, instrs) = O.PutNew (id, instrs)
	  | translateExp (id, VarExp (_, id'), instrs) =
	    O.PutVar (id, translateId id', instrs)
	  | translateExp (id, TagExp (_, _, tag, _), instrs) =
	    O.PutConst (id, O.Int tag, instrs)
	  | translateExp (id, ConExp (_, Con id', _), instrs) =
	    O.PutVar (id, translateId id', instrs)
	  | translateExp (id, ConExp (_, StaticCon s, _), instrs) =
	    O.PutConst (id, O.Constructor s, instrs)
	  | translateExp (id, TupExp (_, ids), instrs) =
	    O.PutTup (id, Vector.map translateId ids, instrs)


	fun translate () (desc, component) = ()
    end
