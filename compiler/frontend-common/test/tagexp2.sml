__prebound P

signature LR_TABLE =
    sig
        datatype ('a,'b) pairlist =
            EMPTY
          | PAIR of ('a * 'b * ('a,'b) pairlist)
        datatype term = T of P.int
    end

signature TOKEN =
    sig
        structure LrTable: LR_TABLE
        datatype ('a, 'b) token = TOKEN of LrTable.term * ('a * 'b * 'b)
    end

functor LrVals(Token: TOKEN) =
    struct
        structure LrTable = Token.LrTable
        structure Token = Token

        local
            open LrTable
        in
            val x = PAIR (T 1, 0, EMPTY)
        end
    end
(*
structure LrTable: LR_TABLE =
    struct
        datatype ('a,'b) pairlist =
            EMPTY
          | PAIR of ('a * 'b * ('a,'b) pairlist)
        datatype term = T of P.int
    end

structure Token: TOKEN =
    struct
        structure LrTable = LrTable
        datatype ('a,'b) token = TOKEN of LrTable.term * ('a * 'b * 'b)
    end

structure LrVals = LrVals(Token)
*)
