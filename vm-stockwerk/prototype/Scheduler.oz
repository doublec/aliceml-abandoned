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
   PrimitiveTable(values)
   ByneedInterpreter(interpreter)
export
   Object
define
   %--** add another thread that checks for preemption

   class Thread
      attr Args TaskStack Res Suspended State
      meth init(args: A stack: T result: R)
	 Args <- A
	 TaskStack <- T
	 Res <- R
	 Suspended <- false
	 State <- runnable
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
	 State <- terminated
      end
      meth setSuspend(B)
	 case @State of terminated then skip
	 else Suspended <- B
	 end
      end
      meth isSuspended($)
	 @Suspended
      end
      meth setState(S)
	 State <- S
      end
      meth getState($)
	 @State
      end
   end

   fun {MyMember Ts T}
      if {IsFree Ts} then false
      elsecase Ts of !T|_ then true
      elseof _|Tr then {MyMember Tr T}
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
	    Scheduler, Enqueue({New Thread init(args: Args
						stack: TaskStack
						result: Res)})
	 end
      end
      meth wakeup(T)
	 {T setState(runnable)}
	 Scheduler, Enqueue(T)
      end
      meth condEnqueue(T)
	 case {T getState($)} of runnable andthen {Not {MyMember @QueueHd T}}
	 then Scheduler, Enqueue(T)
	 else skip
	 end
      end
      meth Enqueue(T) Tl Rest in
	 {T setState(runnable)}
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
	    if {Not {T isSuspended($)}} then
	       CurrentThread <- T
	       Scheduler, Run({T getArgs($)} {T getTaskStack($)})
	    end
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
	    {System.showError
	     'uncaught exception: '#{Value.toVirtualString Exn 5 5}}
	    {Application.exit 1}
	 end
      end
      meth Result(Res)
	 case Res of continue(Args TaskStack) then
	    Scheduler, Run(Args TaskStack)
	 [] preempt(Args TaskStack) then
	    {@CurrentThread setArgsAndTaskStack(Args TaskStack)}
	    Scheduler, Enqueue(@CurrentThread)
	 [] exception(Debug Exn TaskStack) then
	    Scheduler, Handle(Debug Exn TaskStack)
	 [] request(Transient=transient(TransientState) Args TaskStack) then
	    case {Access TransientState} of hole(_) then
	       %--** currently an Alice-specific exception:
	       Scheduler, Handle(nil PrimitiveTable.values.'Hole.Hole'
				 TaskStack)
	    [] future(Ts) then
	       {@CurrentThread setArgsAndTaskStack(Args TaskStack)}
	       {@CurrentThread setState(blocked)}
	       {Assign TransientState future(@CurrentThread|Ts)}
	    [] byneed(Closure) then
	       {@CurrentThread setArgsAndTaskStack(Args TaskStack)}
	       {@CurrentThread setState(blocked)}
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
