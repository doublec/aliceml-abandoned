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
   Helper(deref: Deref pushCallInterpreter: PushCallInterpreter)
export
   Object
define
   %--** add another thread that checks for preemption

   INITIAL_SIGNAL_SOURCES_SIZE = 8

   class Thread
      attr Args: unit TaskStack: unit Suspended: unit State: unit
      meth init(args: A stack: T)
	 Args <- A
	 TaskStack <- T
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
      meth setTerminated()
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

   class SignalManager
      attr PendingSignalsHd: unit PendingSignalsTl: unit SignalSources: unit
      meth init() Empty in
	 PendingSignalsHd <- Empty
	 PendingSignalsTl <- Empty
	 SignalSources <- {Array.new 0 INITIAL_SIGNAL_SOURCES_SIZE - 1 unit}
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
      meth emitSignal(Id) NewTl in
	 @PendingSignalsTl = Id|NewTl
	 PendingSignalsTl <- NewTl
      end
      meth waitForSignal(?HasSources) A in
	 A = @SignalSources
	 for I in 0..{Array.high A} break: Break do
	    case A.I of unit then skip
	    else
	       HasSources = true
	       {Break}
	    end
	 end
	 if {IsFree HasSources} then
	    HasSources = false
	 else
	    {Wait @PendingSignalsHd}
	 end
      end
      meth processPendingSignals()
	 if {IsFree @PendingSignalsHd} then skip
	 elsecase @PendingSignalsHd of Id|Rest then
	    PendingSignalsHd <- Rest
	    case @SignalSources.Id of transient(TransientState) then
	       case {Access TransientState} of future(Ts) then
		  for T in Ts do
		     {self wakeup(T)}
		  end
	       end
	       {Assign TransientState ref(tuple())}
	    end
	    @SignalSources.Id := unit
	    SignalManager, processPendingSignals()
	 end
      end
   end

   class Scheduler from SignalManager
      attr QueueHd: unit QueueTl: unit CurrentThread: unit
      meth init() Empty in
	 SignalManager, init()
	 QueueHd <- Empty
	 QueueTl <- Empty
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
	 SignalManager, processPendingSignals()
	 if {IsFree Hd} then
	    if SignalManager, waitForSignal($) then
	       Scheduler, run()
	    end
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
	    {@CurrentThread setTerminated()}
	 end
      end
      meth Handle(Debug Exn TaskStack)
	 case TaskStack of Frame|_ then Interpreter in
	    Interpreter = Frame.1
	    Scheduler, Result({Interpreter.handle Debug Exn TaskStack})
	 [] nil then
	    case {Property.condGet 'alice.atExn' unit} of unit then Exn2 in
	       Exn2 = case {Deref Exn} of Exn=con(Con ...) then
			 {AdjoinAt Exn 1 {Deref Con}}
		      elseof Exn then Exn
		      end
	       {System.showError
		'uncaught exception: '#{Value.toVirtualString Exn2 5 5}}
	       {DumpTaskStack {Reverse Debug}}
	       {Application.exit 1}
	    elseof Closure then
	       Scheduler, Run(args()
			      pushCall(PushCallInterpreter Closure)|TaskStack)
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
   end

   Object = {New Scheduler init()}
end
