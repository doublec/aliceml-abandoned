(* nested jacke declarations, allowed ? *)

let parser p1 = r1
in let parser p2 : int = r2
   in " ... using parser p2 ... " end
end
