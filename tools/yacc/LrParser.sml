(* adapted from ML-Yacc Parser Generator 
   (c) 1989 Andrew W. Appel, David R. Tarditi 
*)
signature LR_PARSER_ENG =
    sig
	structure LrTable : LR_TABLE
	structure Token : TOKEN

	sharing LrTable = Token.LrTable

	exception ParseError

	val parse : {table : LrTable.table,
		     lexer : unit -> ('_b,'_c) Token.token,
		     arg: 'arg,
		     saction : int * '_c *
		               (LrTable.state * ('_b * '_c * '_c)) list * 
			       'arg ->
				LrTable.nonterm *
				     ('_b * '_c * '_c) *
				     ((LrTable.state *('_b * '_c * '_c)) list),
				     void : '_b
		     } -> '_b
    end


structure LrParserEng :> LR_PARSER_ENG =
 struct
     val print = fn s => TextIO.print s
     val println = fn s => (print s; print "\n")
     structure LrTable = LrTable
     structure Token : TOKEN =
	struct
	    structure LrTable = LrTable
	    datatype ('a,'b) token = TOKEN of LrTable.term * ('a * 'b * 'b)
	    val sameToken = fn (TOKEN (t,_),TOKEN(t',_)) => t=t'
	end
     

     open LrTable 
     open Token

     val DEBUG = true
     exception ParseError

      type ('a,'b) elem = (state * ('a * 'b * 'b))
      type ('a,'b) stack = ('a,'b) elem list

      val showState = fn (STATE s) => ("STATE " ^ (Int.toString s))
      val showTerminal = fn (T i) => ("T " ^ (Int.toString i))

      fun printStack(stack: ('a,'b) elem list, n: int) =
         case stack
           of (state, _) :: rest =>
                 (print("          " ^ (Int.toString n) ^ ": ");
                  println(showState state);
                  printStack(rest, n+1)
                 )
            | nil => ()

      val parse = fn {arg : 'a,
		      table : LrTable.table,
		      lexer : unit -> ('_b,'_c) token,
		      saction : int * '_c * ('_b,'_c) stack * 'a ->
				nonterm * ('_b * '_c * '_c) * ('_b,'_c) stack,
		      void : '_b} =>
 let fun prAction(stack as (state, _) :: _, 
		  next as (TOKEN (term,_)), action) =
             (println "Parse: state stack:";
              printStack(stack, 0);
              print("       state="
                         ^ showState state	
                         ^ " next="
                         ^ showTerminal term
                         ^ " action="
                        );
              case action
                of SHIFT s => println ("SHIFT " ^ showState s)
                 | REDUCE i => println ("REDUCE " ^ (Int.toString i))
                 | ERROR => println "ERROR"
		 | ACCEPT => println "ACCEPT";
              action)
        | prAction (_,_,action) = action

      val action = LrTable.action table
      val goto = LrTable.goto table

      fun get f =  f ()

      fun parseStep(next as (TOKEN (terminal, value as (_,leftPos,_))),
		    stack as (state,_) :: _ : ('_b ,'_c) stack) =
         case (if DEBUG then prAction(stack, next,action(state, terminal))
               else action(state, terminal))
              of SHIFT s => parseStep(get lexer, (s,value) :: stack)
               | REDUCE i =>
		    let val (nonterm,value,stack as (state,_) :: _ ) =
					 saction(i,leftPos,stack,arg)
		    in parseStep(next,(goto(state,nonterm),value)::stack)
		    end
               | ERROR => let val (_,leftPos,rightPos) = value
		          in (*ErrorMsg.error leftPos "syntax error\n";*)
			     raise ParseError
			  end
  	       | ACCEPT => let val (_,(topvalue,_,_)) :: _ = stack
			       (*val (token,restLexer) = next*)
			   in topvalue
			   end
      val next as (TOKEN (terminal,(_,leftPos,_))) = get lexer
   in parseStep(next,[(initialState table,(void,leftPos,leftPos))])
   end
end;

