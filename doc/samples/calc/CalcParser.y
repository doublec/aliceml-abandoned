(* Calc -- A alicelex / aliceyacc sample 
 *
 *  Authors: Benedikt Grundmann (bgrund@ps.uni-sb.de) 
 *
 *  $Revision$
 *
 *  Last updated: $Date$ by $Author$
 * 
 *)

structure CalcParser =
struct
    token NUMBER of LargeInt.int | PLUS | MINUS | TIMES | DIVIDE | LPAR | RPAR | NEGATE

    nonassoc NEGATE
    assocl TIMES DIVIDE
    assocl PLUS MINUS
    
    type pos = int * int
    
    exception ParseError of pos * pos

    fun parseError (lPos, rPos) =  raise ParseError (lPos, rPos)

    rule exp  =
        NUMBER
      | MINUS, exp                      => (~exp) prec NEGATE
      | LPAR, exp, RPAR                 => (exp)
      | n1 as exp, addop, n2 as exp     => (addop (n1, n2)) prec PLUS
      | n1 as exp, mulop, n2 as exp     => (mulop (n1, n2)) prec TIMES

    and addop =
        PLUS    => (LargeInt.+)
      | MINUS   => (LargeInt.-)
    
    and mulop =
        TIMES   => (LargeInt.*)
      | DIVIDE  => (LargeInt.div)

    parser eval = exp

end
