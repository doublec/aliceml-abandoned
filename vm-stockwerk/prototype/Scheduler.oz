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
   Property(condGet)
   System(showError)
   Application(exit)
   Debug(dumpTaskStack: DumpTaskStack)
   PrimitiveTable(values)
   ByneedInterpreter(interpreter)
require
   Helper(pushCallInterpreter: PushCallInterpreter)
export
   Object
define
   %--** add another thread that checks for preemption

   INITIAL_SIGNAL_SOURCES_SIZE = 8

   class Thread
      attr Args: unit TaskStack: unit Res: unit Suspended: unit State: unit
      meth init(args: A stack: T result: R <= _)
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
	 %--** is the result ever needed anymore?
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
      meth block(Transient)
	 State <- blocked(Transient)
      end
      meth unregister()
	 case @State of blocked(transient(TransientState)) then
	    case {Access TransientState} of future(Ts) then
	       {Assign TransientState
		future({Filter Ts fun {$ T} T \= self end})}
	    end
	 end
      end
      meth wakeup()
	 State <- runnable
      end
      meth getState($)
	 {Label @State}
      end
   end

   fun {QueueMember Ts T}
      if {IsFree Ts} then false
      elsecase Ts of !T|_ then true
      elseof _|Tr then {QueueMember Tr T}
      end
   end

   class Scheduler
      attr
	 QueueHd: unit QueueTl: unit CurrentThread: unit
	 SignalSources: unit PendingSignals: unit
      meth init() Empty in
	 QueueHd <- Empty
	 QueueTl <- Empty
	 SignalSources <- {Array.new 0 INITIAL_SIGNAL_SOURCES_SIZE - 1 unit}
	 PendingSignals <- nil
      end
      meth newThread(closure: Closure <= unit
		     args: Args <= args()
		     taskStack: TaskStack0 <= nil) TaskStack in
	 TaskStack = case Closure of unit then TaskStack0
		     else pushCall(PushCallInterpreter Closure)|TaskStack0
		     end
	 Scheduler, Enqueue({New Thread init(args: Args stack: TaskStack)})
      end
      meth wakeup(T)
	 {T wakeup()}
	 Scheduler, Enqueue(T)
      end
      meth condEnqueue(T)
	 case {T getState($)} of runnable
	    andthen {Not {QueueMember @QueueHd T}}
	 then Scheduler, Enqueue(T)
	 else skip
	 end
      end
      meth Enqueue(T) Tl Rest in
	 Tl = (QueueTl <- Rest)
	 Tl = T|Rest
      end
      meth getCurrentThread($)
	 @CurrentThread
      end
      meth run() Hd = @QueueHd in
	 case @PendingSignals of Id|Rest then
	    PendingSignals <- Rest
	    case @SignalSources.Id of transient(TransientState) then
	       case {Access TransientState} of future(Ts) then
		  for T in Ts do
		     Scheduler, wakeup(T)
		  end
	       end
	       {Assign TransientState ref(tuple())}
	    end
	    @SignalSources.Id := unit
	 elseif {IsFree Hd} then
	    skip   %--** wait for I/O or asynchronous signals
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
	    case {Property.condGet 'alice.atExn' unit} of unit then
	       {System.showError
		'uncaught exception: '#{Value.toVirtualString Exn 5 5}}
	       {DumpTaskStack {Reverse Debug}}
	       {Application.exit 1}
	    elseof Closure then
	       Scheduler, Run(args() {Closure.1.1.pushCall Closure TaskStack})
	    end
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
	       {Assign TransientState future(@CurrentThread|Ts)}
	       {@CurrentThread block(Transient)}
	    [] byneed(Closure) then
	       {@CurrentThread setArgsAndTaskStack(Args TaskStack)}
	       {Assign TransientState future([@CurrentThread])}
	       {@CurrentThread block(Transient)}
	       Scheduler, Byneed(Transient Closure)
	    [] cancelled(Exn) then
	       Scheduler, Handle(nil Exn TaskStack)
	    end
	 end
      end
      meth Byneed(Transient Closure) TaskStack in
	 %--** when can this be done in the current thread?
	 TaskStack = [byneedFrame(ByneedInterpreter.interpreter Transient)]
	 Scheduler, newThread(closure: Closure taskStack: TaskStack)
      end
      meth registerSignalSource(?Id ?Transient) A in
	 A = @SignalSources
	 for I in 0..{Array.high A} break: Break do
	    case A.I of unit then
	       Id = I
	       A.Id := Transient
	       {Break}
	    end
	 end
	 if {IsFree Id} then NewA in   % enlargen array
	    Id = {Array.high A} + 1
	    NewA = {Array.new 0 (Id * 3 div 2 - 1) unit}
	    for I in 0..{Array.high A} do
	       NewA.I := A.I
	    end
	    SignalSources <- NewA
	    NewA.Id := Transient
	 end
	 Transient = transient({NewCell future(nil)})
      end
      meth emitSignal(Id)
	 PendingSignals <- Id|@PendingSignals
      end
   end

   Object = {New Scheduler init()}
end
