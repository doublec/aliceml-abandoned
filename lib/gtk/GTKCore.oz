%%%
%%% Authors:
%%%   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Thorsten Brunklaus, 2000
%%%
%%% Last Change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   Native at 'GTK.so{native}'
export
   'GTKCore$' : GTKCore
define
   Object
   VaArg
   Dispatcher

   local
      Table = {Dictionary.new}
   in
      fun {PointerToObject Pointer}
	 Object = {ForeignPointer.toInt Pointer}
      in
	 {Dictionary.put Table Object Pointer}
	 Object
      end
      fun {ObjectToPointer Object}
	 {Dictionary.get Table Object}
      end
      proc {RemoveObject Object}
	 {Dictionary.remove Table Object}
      end
   end

   fun {SignalConnect Object Name Handler}
      SignalId = {Dispatcher registerHandler(Handler $)}
      AtomName = {String.toAtom {ByteString.toString Name}}
   in
      {Native.signalConnectSml {ObjectToPointer Object} AtomName SignalId _}
      SignalId
   end
   fun {SignalDisconnect Object SignalId}
      {Dispatcher unregisterHandler(SignalId)}
      {Native.signalConnectSml {ObjectToPointer Object} SignalId}
      unit
   end
   fun {SignalHandlerBlock Object SignalId}
      {Native.signalBlock {ObjectToPointer Object} SignalId}
      unit
   end
   fun {SignalHandlerUnblock Object SignalId}
      {Native.signalUnblock {ObjectToPointer Object} SignalId}
      unit
   end
   fun {SignalEmit Object Name}
      AtomName = {String.toAtom {ByteString.toString Name}}
   in
      {Native.signalEmitByName {ObjectToPointer Object} AtomName}
      unit
   end

   local
      local
	 PollingIntervall = 50
	 NewSignalId      = {NewName}
	 FillStream       = {NewName}
	 Dispatch         = {NewName}
      in
	 class DispatcherObject
	    attr
	       signalId    %% SignalId Counter
	       handlerDict %% SignalId -> Handler
	       signalPort  %% Signal Port
	       stream      %% Port Stream
	       threadId    %% Thread Id of "Filler Thread"
	    meth create
	       SignalPort = {Port.new @stream}
	    in
	       @signalId    = 0
	       @handlerDict = {Dictionary.new}
	       @signalPort  = SignalPort
	       %% Tell C side about signal port
	       {Native.initializeSignalPortSml SignalPort}
	       %% Fetch Events
	       thread @threadId = {Thread.this} DispatcherObject, FillStream end
	       %% Call Event Handlers
	       thread DispatcherObject, Dispatch end
	    end
	    meth !FillStream
	       {Native.handlePendingEvents}
	       {Time.delay PollingIntervall}
	       DispatcherObject, FillStream
	    end
	    meth !NewSignalId($)
	       signalId <- (@signalId + 1)
	    end
	    meth registerHandler(Handler $)
	       SignalId = DispatcherObject, NewSignalId($)
	    in
	       {Dictionary.put @handlerDict SignalId Handler}
	       SignalId
	    end
	    meth unregisterHandler(SignalId)
	       {Dictionary.remove @handlerDict SignalId}
	    end
	    meth !Dispatch
	       case @stream
	       of Event|Tail then
		  P = {Dictionary.get @handlerDict Event}
	       in
		  _ = case {Procedure.arity P}
		      of 1 then {P}
		      [] 2 then {P unit}
		      end
		  stream <- Tail
		  DispatcherObject, Dispatch
	       end
	    end
	    meth exit
	       {Thread.terminate @threadId}     %% Terminate Event Fetching
	       {Thread.terminate {Thread.this}} %% Terminate Dispatch Thread
	    end
	 end
      end
   in
      %% SML va_arg list -> Oz List
      fun {VaArgListToOzList As}
	 case As
	 of nil  then nil
	 [] A|Ar then (A.1)|{VaArgListToOzList Ar} 
	 end
      end

      fun {Exit _}
	 {Native.exit}
	 {Dispatcher exit}
	 unit
      end
      
      %% Create Interface
      GTKCore = 'GTKCore'('$object'            : Object
			  '$va_arg'            : VaArg
			  pointerToObject      : PointerToObject
			  objectToPointer      : ObjectToPointer
			  removeObject         : RemoveObject
			  vaArgListToOzList    : VaArgListToOzList
			  signalConnect        : SignalConnect
			  signalDisconnect     : SignalDisconnect
			  signalHandlerBlock   : SignalHandlerBlock
			  signalHandlerUnblock : SignalHandlerUnblock
			  signalEmit           : SignalEmit
			  exit                 : Exit)
      
      %% Start dispatcher
      Dispatcher = {New DispatcherObject create}
   end
end
