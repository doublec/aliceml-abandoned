%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2002
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   PrimitiveTable(values)
   Scheduler(object)
export
   interpreter: Me
define
   fun {Construct Args}
      case Args of arg(X) then X
      [] args(...) then {Adjoin Args tuple}
      end
   end

   fun {Deref X}
      case X of transient(TransientState) then
	 case {Access TransientState} of ref(Y) then {Deref Y}
	 else X
	 end
      else X
      end
   end

   fun {IsCyclic X TransientState}
      case {Deref X} of transient(TransientState2) then
	 TransientState2 == TransientState
      else false
      end
   end

   Me =
   byneedInterpreter(
      run:
	 fun {$ Args byneedFrame(_ Transient=transient(TransientState))|Rest}
	    case {Access TransientState} of future(Ts) then X in
	       X = {Construct Args}
	       if {IsCyclic X TransientState} then
		  {Assign TransientState
		   cancelled(PrimitiveTable.values.'Hole.Cyclic')}
		  exception(nil PrimitiveTable.values.'Hole.Cyclic' Rest)
	       else
		  for T in Ts do {Scheduler.object wakeup(T)} end
		  {Assign TransientState ref(X)}
		  continue(arg(Transient) Rest)
	       end
	    end
	 end
      handle:
	 fun {$ _ Exn byneedFrame(_ Transient=transient(TransientState))|Rest}
	    %--** exception wrapping?
	    case {Access TransientState} of future(Ts) then
	       for T in Ts do {Scheduler.object wakeup(T)} end
	       {Assign TransientState cancelled(Exn)}
	       continue(arg(Transient) Rest)
	    end
	 end
/*--** this may be useful for debugging (seeing a full stack trace):
	 fun {$ Debug Exn Frame|Rest}
	    exception(Frame|Debug Exn Rest)
	 end
*/
      toString: fun {$ _} 'Byneed' end)
end
