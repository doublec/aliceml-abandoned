(* datatype and translation functions for normalization of rules *)

structure NormalForm =
struct
    structure A = AbsSyn

    type nonterm = string * int
    type term = string * int
    datatype symbol = NT of nonterm | T of term

    type lhs = term
    type rhs = (string * symbol) list
	
    datatype rule =
	RULE of {lhs  :lhs, 
		 rhs  :rhs,
		 prec :term option}
    type grammar = rule list


    val nonterms = ref 0   (* erstmal zaehlen, may increase during normalization *)
    val terms = 0

    fun mkNewNamefunction s m =
	let val r = ref m 
	in 
	    fn () => (r:=(!r+1); s^"__"^Int.toString(!r-1))^"__"
	end

    val newName = mkNewNamefunction "jackeXXXXXXXX" 0









    fun normalizeBnf (A.Skip) = (A.Skip,[])
      | normalizeBnf (A.Symbol s) = (A.As(s,A.Symbol s),[])
      | normalizeBnf (A.As (id,A.Symbol s)) = (A.As(id,A.Symbol s), [])
      | normalizeBnf (A.As (id, bnf)) = 
	let val (b,l) = normalizeBnf bnf
	in (A.As (id, b),l) 
	end
      | normalizeBnf (A.Seq l) =
	let val l = map (fn b => normalizeBnf b) l
	    val (l',l'') = ListPair.unzip l
	    val _ = print ("laenge l'': "^(Int.toString (List.length l'')))
	in (A.Seq l', List.concat l'') 
	end
      | normalizeBnf (A.Alt l) = 
	let val X = newName ()
	    val l = map (fn b => normalizeBnf b) l
	    val (l',l'') = ListPair.unzip l
	    val _ = print ("laenge l'': "^(Int.toString (List.length l'')))
	in (A.Symbol X,  List.concat ((map (fn b => (X,NONE,b)) l') ::l'')) 
	end
      | normalizeBnf (A.Prec (bnf,id)) = 
	let val X = newName ()
	    val (b,l) = normalizeBnf bnf
	in (A.Symbol X, (X,NONE,b)::l) 
	end
      | normalizeBnf (A.Transform (bnf,code)) = 
	let val code = foldr (fn (s,b)=> s^" "^b) "" code
	    val X = newName ()
	    val (b,l) = normalizeBnf bnf
	in (A.Transform (A.As (X, A.Symbol X), [X]), (X,NONE,A.Transform (b,[code])) ::l) 
	end
    
    fun normalizePt (A.RuleDec []) = []
      | normalizePt (A.RuleDec ((X,t,bnf)::l)) =
	let val (b,l') = normalizeBnf bnf
	in (A.RuleDec ((X,t,b)::l')) ::(normalizePt (A.RuleDec l))
	end
      | normalizePt p = [p]

    fun normalize l = foldr (fn (d,p) => (normalizePt d) @ p) [] l 
end
