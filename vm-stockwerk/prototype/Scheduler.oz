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
   System(showError)
   Application(exit)
   Primitives(table)
   AbstractCodeInterpreter
export
   Scheduler
define
   class Scheduler
      attr QueueHd: unit QueueTl: unit
      meth init() Empty in
	 QueueHd <- Empty
	 QueueTl <- Empty
      end
      meth newThread(Closure)
	 case Closure of closure(function(_ NL IdDefArgs BodyInstr) G) then
	    %--** parameterize over interpreter
	    L = {NewArray 0 NL - 1 unit}
	    TaskStack = [frame(AbstractCodeInterpreter
			       IdDefArgs BodyInstr G L)]
	 in
	    Scheduler, Enqueue('thread'(tuple TaskStack))
	 end
      end
      meth Enqueue(T) Hd Rest in
	 Hd = (@QueueHd <- Rest)
	 Hd = T|Rest
      end
      meth run() Hd = @QueueHd in
	 if {IsFree Hd} then
	    skip   %--** wait for I/O
	 elsecase Hd of 'thread'(Args TaskStack)|Tr then
	    QueueHd <- Tr
	    Scheduler, Run(Args TaskStack)
	    Scheduler, run()
	 end
      end
      meth Run(Args TaskStack)
	 case TaskStack of Frame|_ then Interpreter in
	    Interpreter = Frame.1
	    Scheduler, Result({Interpreter.run Args TaskStack})
	 end
      end
      meth Handle(Debug Exn TaskStack)
	 case TaskStack of Frame|_ then Interpreter in
	    Interpreter = Frame.1
	    Scheduler, Result({Interpreter.handle Debug Exn TaskStack})
	 end
      end
      meth Result(Res)
	 case Res of continue(Args TaskStack) then
	    Scheduler, Run(Args TaskStack)
	 [] preempt(Args TaskStack) then
	    Scheduler, Enqueue('thread'(Args TaskStack))
	 [] exception(Debug Exn TaskStack) then
	    Scheduler, Handle(Debug Exn TaskStack)
	 [] terminate then skip
	 end
      end
   end
end
