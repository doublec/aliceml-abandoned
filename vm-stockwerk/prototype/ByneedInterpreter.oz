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
   Scheduler(object)
export
   interpreter: Me
define
   fun {Construct Args}
      case Args of arg(X) then X
      [] args(...) then {Adjoin args tuple}
      end
   end

   fun {ByneedInterpreterRun Args TaskStack}
      case TaskStack
      of byneedFrame(_ Transient=transient(TransientState))|Rest then
	 case {Access TransientState} of future(Ts) then
	    %--** cyclic?
	    for T in Ts do {Scheduler.object enqueue(T)} end
	    {Assign TransientState ref({Construct Args})}
	    continue(arg(Transient) Rest)
	 end
      end
   end

   fun {ByneedInterpreterHandle _ Exn TaskStack}
      %--** exception wrapping?
      case TaskStack
      of byneedFrame(_ Transient=transient(TransientState))|Rest then
	 case {Access TransientState} of future(Ts) then
	    for T in Ts do {Scheduler.object enqueue(T)} end
	    {Assign TransientState cancelled(Exn)}
	    continue(arg(Transient) Rest)
	 end
      end
   end

   fun {ByneedInterpreterPushCall _ _}
      {Exception.raiseError byneedInterpreterPushCall} unit
   end

   Me = byneedInterpreter(run: ByneedInterpreterRun
			  handle: ByneedInterpreterHandle
			  pushCall: ByneedInterpreterPushCall)
end
