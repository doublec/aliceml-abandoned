structure MergeSort =
    struct
	fun sort (l as (pos, labs)) =
	    let
		val k = length pos
	    in
		case k of
		    0 => l
		  | 1 => l
		  | _ => let
			     val h = k div 2
			     val vorne = sort (List.take (pos, h), List.take (labs, h))
			     val hinten = sort (List.drop (pos, h), List.drop (labs, h))
			 in
			     merge(vorne,hinten)
			 end
	    end

	and
	    merge (oh as (r::rs,x::xs),why as (s::ss,y::ys)) =
	    if LargeInt.>(r,s) then
		(fn (a,b) => (r::a, x::b)) (merge((rs, xs),why))
	    else (fn (a,b) => (s::a, y::b)) (merge(oh,(ss,ys)))
	  | merge ((nil,_), ys) = ys
	  | merge ((_,nil), ys) = ys
	  | merge (xs, _) = xs
    end
