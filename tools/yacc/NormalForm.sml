(* datatype and translation functions for normalization of rules *)

structure NormalForm =
struct
    structure A = AbsSyn

    fun mkNewNamefunction s m =
	let val r = ref m 
	in 
	    fn () => (r:=(!r+1); s^"__"^Int.toString(!r-1))^"__"
	end

    val newRulename = mkNewNamefunction "rule" 954
    val newIdname = mkNewNamefunction "id" 387

    (* not used... last tokendec => datatype + eop *)
(*    local 
	val eop = "EOP"
	fun countTokenDecs p = 
	    List.length(List.filter (fn (A.TokenDec l) => true | _ => false) p)
	fun remove n [] = []
	  | remove n (a::l) = if n<1 then a::l
			      else case a of
				  (A.TokenDec _) => remove (n-1) l
				| _ => a::(remove n l)
    in
	fun normTokenDec p =
	    let val tokenDecs = countTokenDecs p
		val extended = List.map 
		(fn (A.TokenDec l) => A.TokenDec ((eop,NONE)::l) | x => x) p
	    in
		remove (tokenDecs -1) extended
	    end
    end
*)
    local fun split l1 l2 [] = (l1,l2)
	    | split l1 l2 (x::xs) = split l2 (x::l1) xs
	  fun merge l1 [] = l1
	    | merge [] l2 = l2
	    | merge (x::xs :string list) (y::ys) = 
	      if x<y then x::(merge xs (y::ys))
	      else y::(merge (x::xs) ys)
    in 
	fun sort [] = []
	  | sort [x] = [x]
	  | sort l =
	    let val (l1,l2) = split [] [] l
	    in
		merge (sort l1) (sort l2)
	    end
    end 

    fun remove (x::y::l) = if x=y then remove (y::l) else x::remove (y::l)
      | remove l = l

    fun names (A.As (s,_)) = [s]
      | names (A.Seq l) = List.concat (List.map names l)
      | names A.Skip = ["()"]
      | names _ = []

    fun tupling l =
	let fun seq [] = [A.ATEXP ""]
	      | seq [n] = [A.ATEXP n]
	      | seq (n::ns) = A.ATEXP n :: A.ATEXP ", " :: (seq ns)
	in A.PAREXP (A.EXP (seq l)) end
    
    (* introduce explicit names for toplevel expressions where necessary *)
    (* deal with multiple occurrences: introduce fresh names, 
     ie. ones not usable in code, where necessary *)
    val explnames = 
	(List.map (fn (A.As (s,_)) => s | _ => "")) 
	o (List.filter (fn A.As _ => true | _ => false)) 

    fun multTokens seq =
	let val l = List.filter (fn A.Symbol _ => true | _ => false) seq
	    val sorted = sort (List.map (fn (A.Symbol s) => s) l)
	    fun mult (s::s'::r) = if s=s' then s::(mult r) else mult r
	      | mult _ = []
	in mult sorted  end

	    (* TO DO *) 
    fun naming bnf =
	let fun naming u (A.As (a,bnf)) = A.As (a,bnf)
	      | naming u (A.Symbol s) = 
	    if List.exists (fn s' => s=s') u 
		then A.As (newIdname(), A.Symbol s)
	    else A.As (s,A.Symbol s)
	      | naming u (A.Seq l) = 
	    let val explnames = explnames l
		val multnames = multTokens l
	    in 
		A.Seq (map (naming (explnames@multnames)) l)
	    end
	      | naming u A.Skip = A.As (newIdname(),A.Skip)
	      | naming u bnf = A.As (newIdname(), bnf) 
	in naming [] bnf
	end

    fun nameTopLevel (A.Prec (bnf,s)) = A.Prec (nameTopLevel bnf,s)
      | nameTopLevel (A.Transform (bnf,c)) = A.Transform (naming bnf,c)
      | nameTopLevel bnf = naming bnf

    (* remove unnecessary sequencing *)
    fun flattenSeq [] = []
      | flattenSeq ((A.Seq l)::l') =
	let val l = flattenSeq l
	    val l' = flattenSeq l'
	in l@l' end
      | flattenSeq (x::l) = (flatten x)::(flattenSeq l)
    and flatten (A.Seq l) = A.Seq (flattenSeq l)
      | flatten (A.Skip) = A.Skip
      | flatten (A.As (s,bnf)) = A.As (s, flatten bnf)
      | flatten (A.Prec (bnf,s)) = A.Prec (flatten bnf,s)
      | flatten (A.Transform (bnf,c)) = A.Transform (flatten bnf,c)
      | flatten (A.Alt l) = A.Alt (map flatten l)
      | flatten (A.Symbol s) = A.Symbol s

    (* remove unnecessary alternatives *)
    fun flattenAlt [] = []
      | flattenAlt ((A.Alt l)::l') =
	let val l = flattenAlt l
	    val l' = flattenAlt l'
	in l@l' end
      | flattenAlt (x::l) = (flatten1 x)::(flattenAlt l)
    and flatten1 (A.Seq l) = A.Seq (map flatten1 l)
      | flatten1 (A.Skip) = A.Skip
      | flatten1 (A.As (s,bnf)) = A.As (s, flatten1 bnf)
      | flatten1 (A.Prec (bnf,s)) = A.Prec (flatten1 bnf,s)
      | flatten1 (A.Transform (bnf,c)) = A.Transform (flatten1 bnf,c)
      | flatten1 (A.Alt l) = A.Alt (flattenAlt l)
      | flatten1 (A.Symbol s) = A.Symbol s


    (* introduce toplevel transformations on named expressions *)
    fun toSeq (A.Seq l) = A.Seq l
      | toSeq b = A.Seq [b]

    fun semAction (A.Transform (t,c)) = A.Transform (toSeq t,c)
      | semAction (A.Prec (A.Transform (bnf,c),s)) = 
	A.Prec (A.Transform (toSeq bnf,c), s)
      | semAction (A.Prec (bnf,s)) = 
		A.Prec (A.Transform (toSeq bnf,
				     (NONE, A.EXP [tupling(names bnf)])),s)
      | semAction bnf = A.Transform (toSeq bnf,
				     (NONE, A.EXP [tupling(names bnf)]))
 
    (* normalization *)
    fun norm (A.Prec (bnf,s)) = 
	let val (bnf',rs) = norm bnf
	in 
	    (A.Prec (bnf',s),rs)
	end
      | norm (A.Transform (bnf,c)) = 
	let val (bnf',rs) = norm bnf
	in 
	    (A.Transform (bnf',c),rs)
	end
      | norm (A.Seq bnf) = 
	let val (a,rs) = normalizeSubexps bnf
	in 
	    (A.Seq a, rs)
	end
    and normalize (name,ty,bnf) = 
	let val (bnf',rs) = norm (semAction (nameTopLevel ((flatten (flatten1 bnf)))))
	    in 
		(name,ty,bnf')::rs
	end
    and  normalizeSubexps [] = ([],[])
      | normalizeSubexps ((x as (A.As (s,A.Symbol s')))::xs) = 
	let val (a,rs) = normalizeSubexps xs
	in
	    (x::a,rs)
	end
      | normalizeSubexps ((A.As (s,A.Skip))::xs) =
	let val (a,rs) =normalizeSubexps xs
	    val r = newRulename()
	    val r' = (r,NONE,A.Transform(A.Seq[],(NONE, A.EXP [A.ATEXP "()"])))
	in
	    (A.As (s,A.Symbol r)::a, r'::rs)
	end
      | normalizeSubexps ((A.As (s,A.Alt l))::xs) = 
	let val (a,rs) = normalizeSubexps xs
	    val r = newRulename ()
	    val r' = List.concat (List.map normalize 
				  (List.map (fn bnf => (r,NONE,bnf)) l))
	in 
	    ((A.As (s,A.Symbol r))::a, r'@rs)
	end
      | normalizeSubexps ((A.As (s,bnf))::xs) = 
	let val (a,rs) = normalizeSubexps xs
	    val r = newRulename ()
	    val r' = normalize (r,NONE,bnf)
	in 
	    ((A.As (s,A.Symbol r))::a, r'@rs)
	end

    fun toNormalForm p =
	let (* val p' = normTokenDec p *) val p' = p
	    val h = List.concat o (List.map normalize)
	in 
	    List.map (fn A.RuleDec l => A.RuleDec (h l) | x => x) p'
	end     
end
