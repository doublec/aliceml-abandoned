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
export
   Object
define
   class Scheduler
      attr QueueHd: unit QueueTl: unit CurrentThread: unit
      meth init() Empty in
	 QueueHd <- Empty
	 QueueTl <- Empty
      end
      meth newThread(Closure Args ?Res) TaskStack in
	 case Closure of closure(Function ...) then
	    TaskStack = {Function.1.pushCall Closure nil}
	    Scheduler, Enqueue('thread'(args: Args
					stack: TaskStack
					result: Res))
	 end
      end
      meth Enqueue(T) Tl Rest in
	 Tl = (QueueTl <- Rest)
	 Tl = T|Rest
      end
      meth run() Hd = @QueueHd in
	 if {IsFree Hd} then
	    skip   %--** wait for I/O
	 elsecase Hd of (T='thread'(args: Args stack: TaskStack ...))|Tr then
	    QueueHd <- Tr
	    CurrentThread <- T
	    Scheduler, Run(Args TaskStack)
	    Scheduler, run()
	 end
      end
      meth Run(Args TaskStack)
	 case TaskStack of Frame|_ then Interpreter in
	    Interpreter = Frame.1
	    Scheduler, Result({Interpreter.run Args TaskStack})
	 [] nil then
	    @CurrentThread.result = case Args of arg(X) then X
				    [] args(...) then {Adjoin Args tuple}
				    end
	 end
      end
      meth Handle(Debug Exn TaskStack)
	 case TaskStack of Frame|_ then Interpreter in
	    Interpreter = Frame.1
	    Scheduler, Result({Interpreter.handle Debug Exn TaskStack})
	 [] nil then
	    %--** display the stack
	    {System.showError 'uncaught exception'}
	    {Application.exit 1}
	 end
      end
      meth Result(Res)
	 %--** add a `request' result
	 case Res of continue(Args TaskStack) then
	    Scheduler, Run(Args TaskStack)
	 [] preempt(Args TaskStack) then
	    Scheduler, Enqueue({Adjoin @CurrentThread
				'thread'(args: Args stack: TaskStack)})
	 [] exception(Debug Exn TaskStack) then
	    Scheduler, Handle(Debug Exn TaskStack)
	 end
      end
   end

   Object = {New Scheduler init()}
end
