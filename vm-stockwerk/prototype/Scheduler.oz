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
   PrimitiveTable(table)
   ByneedInterpreter(interpreter)
export
   Object
define
   %--** add another thread that checks for preemption

   class Thread
      attr Args TaskStack Res
      meth init(args: A stack: T result: R)
	 Args <- A
	 TaskStack <- T
	 Res <- R
      end
      meth getArgs($)
	 @Args
      end
      meth getTaskStack($)
	 @TaskStack
      end
      meth setArgsAndTaskStack(A T)
	 Args <- A
	 TaskStack <- T
      end
      meth bindResult(X)
	 @Res = X
      end
   end

   class Scheduler
      attr QueueHd: unit QueueTl: unit CurrentThread: unit
      meth init() Empty in
	 QueueHd <- Empty
	 QueueTl <- Empty
      end
      meth newThread(Closure Args ?Res <= _ taskStack: TaskStack0 <= nil)
	 case Closure of closure(Function ...) then TaskStack in
	    TaskStack = {Function.1.pushCall Closure TaskStack0}
	    Scheduler, enqueue({New Thread init(args: Args
						stack: TaskStack
						result: Res)})
	 end
      end
      meth enqueue(T) Tl Rest in
	 Tl = (QueueTl <- Rest)
	 Tl = T|Rest
      end
      meth getCurrentThread($)
	 @CurrentThread
      end
      meth run() Hd = @QueueHd in
	 if {IsFree Hd} then
	    skip   %--** wait for I/O
	 elsecase Hd of T|Tr then
	    QueueHd <- Tr
	    CurrentThread <- T
	    Scheduler, Run({T getArgs($)} {T getTaskStack($)})
	    Scheduler, run()
	 end
      end
      meth Run(Args TaskStack)
	 case TaskStack of Frame|_ then Interpreter in
	    Interpreter = Frame.1
	    Scheduler, Result({Interpreter.run Args TaskStack})
	 [] nil then
	    {@CurrentThread bindResult(case Args of arg(X) then X
				       [] args(...) then {Adjoin Args tuple}
				       end)}
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
	 case Res of continue(Args TaskStack) then
	    Scheduler, Run(Args TaskStack)
	 [] preempt(Args TaskStack) then
	    {@CurrentThread setArgsAndTaskStack(Args TaskStack)}
	    Scheduler, enqueue(@CurrentThread)
	 [] exception(Debug Exn TaskStack) then
	    Scheduler, Handle(Debug Exn TaskStack)
	 [] request(Transient=transient(TransientState) Args TaskStack) then
	    case {Access TransientState} of hole(_) then
	       %--** currently an Alice-specific exception:
	       Scheduler, Handle(nil PrimitiveTable.table.'Hole.Hole'
				 TaskStack)
	    [] future(Ts) then
	       {@CurrentThread setArgsAndTaskStack(Args TaskStack)}
	       {Assign TransientState future(@CurrentThread|Ts)}
	    [] byneed(Closure) then
	       {@CurrentThread setArgsAndTaskStack(Args TaskStack)}
	       {Assign TransientState future([@CurrentThread])}
	       Scheduler, Byneed(Transient Closure)
	    [] cancelled(Exn) then
	       Scheduler, Handle(nil Exn TaskStack)
	    end
	 end
      end
      meth Byneed(Transient Closure) TaskStack in
	 %--** when can this be done in the current thread?
	 TaskStack = [byneedFrame(ByneedInterpreter.interpreter Transient)]
	 Scheduler, newThread(Closure args() taskStack: TaskStack)
      end
   end

   Object = {New Scheduler init()}
end
