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
   Property(put)
   Open
   OS(srand rand)
   System(show)
   Signal at 'GtkSignal.so{native}'
   Native at 'x-oz://system/GTK.so{native}'
export
   'GtkCoreOz$' : GtkCoreOz
define
   Dispatcher

   {Property.put 'errors.depth' 1000}
   {Property.put 'errors.width' 1000}
   
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

   fun {SignalConnect Object ObjSignal Handler}
      Id = {Dispatcher registerHandler(Handler $)}
   in
      {Signal.signalConnect {ObjectToPointer Object} ObjSignal Id} Id
    end
   fun {SignalDisconnect Object SignalId}
      {Dispatcher unregisterHandler(SignalId)}
      {Signal.signalConnect {ObjectToPointer Object} SignalId}
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
   fun {AllocateGdkColor Red Blue Green}
      {PointerToObject {Signal.allocateGdkColor Red Blue Green}}
   end
   fun {FreeGdkColor Color}
      {Signal.freeGdkColor {ObjectToPointer Color}}
      unit
   end

   local
      fun {CreateList Xs L}
	 case Xs
	 of X|Xr then {CreateList Xr {Signal.gListAppend L X}}
	 [] nil  then {PointerToObject L}
	 end
      end
   in
      fun {CreateGList Xs}
	 {CreateList Xs {Signal.null}}
      end
   end

   local
      fun {Id X}
	 X
      end
      fun {PTO X}
	 {PointerToObject X}
      end
      fun {ITB X}
	 if X == 1 then true else false end
      end
      
      ExposeFs     = [window#PTO send#ITB area#PTO count#Id]
      MotionFs     = [window#PTO send#ITB time#Id x#Id y#Id pressure#Id xtilt#Id ytilt#Id state#Id
		      is_hint#Id source#Id deveceid#Id x_root#Id y_root#Id]
      ButtonFs     = [window#PTO send#ITB time#Id x#Id y#Id pressure#Id xtilt#Id ytilt#Id state#Id
		      button#Id source#Id deveceid#Id x_root#Id y_root#Id]
      KeyFs        = [window#PTO send#ITB time#Id state#Id keyval#Id length#Id string#Id]
      CrossingFs   = [window#PTO send#ITB subwindow#PTO time#Id x#Id y#Id x_root#Id y_root#Id
		      mode#Id detail#Id focus#ITB state#Id]
      FocusFs      = [window#PTO send#ITB hasFocus#ITB]
      ConfigureFs  = [window#PTO send#ITB x#Id y#Id width#Id height#Id]
      VisibilityFs = [window#PTO send#ITB state#Id]

      fun {MakeEvent Label FeatS Event}
	 GdkEvent = {Record.make Label {Map FeatS fun {$ X#_} X end}}
      in
	 {List.forAllInd FeatS proc {$ I X#F} GdkEvent.X = {F Event.I} end} GdkEvent
      end
   in
      fun {GetGdkEvent GdkEvent}
	 GdkLabel = {Label GdkEvent}
      in
	 case GdkLabel
	 of 'GDK_EXPOSE'            then {MakeEvent GdkLabel ExposeFs GdkEvent}
	 [] 'GDK_MOTION_NOTIFY'     then {MakeEvent GdkLabel MotionFs GdkEvent}
	 [] 'GDK_BUTTON_PRESS'      then {MakeEvent GdkLabel ButtonFs GdkEvent}
	 [] 'GDK_2BUTTON_PRESS'     then {MakeEvent GdkLabel ButtonFs GdkEvent}
	 [] 'GDK_3BUTTON_PRESS'     then {MakeEvent GdkLabel ButtonFs GdkEvent}
	 [] 'GDK_BUTTON_RELEASE'    then {MakeEvent GdkLabel ButtonFs GdkEvent}
	 [] 'GDK_KEY_PRESS'         then {MakeEvent GdkLabel KeyFs GdkEvent}
	 [] 'GDK_KEY_RELEASE'       then {MakeEvent GdkLabel KeyFs GdkEvent}
	 [] 'GDK_ENTER_NOTIFY'      then {MakeEvent GdkLabel CrossingFs GdkEvent}
	 [] 'GDK_LEAVE_NOTIFY'      then {MakeEvent GdkLabel CrossingFs GdkEvent}
	 [] 'GDK_FOCUS_CHANGE'      then {MakeEvent GdkLabel FocusFs GdkEvent}
	 [] 'GDK_CONFIGURE'         then {MakeEvent GdkLabel ConfigureFs GdkEvent}
	 [] 'GDK_VISIBILITY_NOTIFY' then {MakeEvent GdkLabel VisibilityFs GdkEvent}
	 [] 'GDK_NO_EXPOSE'         then {MakeEvent GdkLabel ExposeFs GdkEvent}
	 [] Name                    then Name
	 end
      end
   end
   fun {FreeGdkRectangle Rect}
      {Signal.freeGdkRectangle {ObjectToPointer Rect}}
   end
   
   local
      local
	 NewSignalId = {NewName}
	 FillStream  = {NewName}
	 Dispatch    = {NewName}
      in
	 class DispatcherObject
	    attr
	       signalId    %% SignalId Counter
	       handlerDict %% SignalId -> Handler
	       signalPort  %% Signal Port
	       threadId    %% Thread Id of "Filler Thread"
	    meth create
	       Stream
	       SignalPort = {Port.new Stream}
	    in
	       @signalId    = 0
	       @handlerDict = {Dictionary.new}
	       @signalPort  = SignalPort
	       %% Tell C side about signal port
	       {Signal.initializeSignalPort SignalPort}
	       %% Fetch Events
	       %% Initial Polling Interval is 50ms
	       thread @threadId = {Thread.this} DispatcherObject, FillStream(50) end
	       %% Call Event Handlers
	       thread
		  try DispatcherObject, Dispatch(Stream)
		  catch Ex then
		     {System.show Ex}
		     DispatcherObject, exit
		  end
	       end
	    end
	    meth !FillStream(PollInterval)
	       NewPollInterval = if {Signal.handlePendingEvents}
				 then 10 %% Rapid Event testing for reactivity
				 else {Min (PollInterval + 5) 50}
				 end
	    in
	       {Time.delay NewPollInterval}
	       DispatcherObject, FillStream(NewPollInterval)
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
	    meth !Dispatch(Stream)
	       case Stream
	       of event(Id Data)|Tail then
		  _ = {{Dictionary.get @handlerDict Id} Data}
		  DispatcherObject, Dispatch(Tail)
	       [] _ then skip
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

      %% This is a temorary shell hack
      local
	 class Shell from Open.pipe Open.text
	    meth init(P A)
	       Open.pipe, init(cmd:  {ByteString.toString P}
			       args: {ByteString.toString A})
	    end
	    meth cmd(Cmd)
	       Open.text, putS({ByteString.toString Cmd})
	    end
	    meth show($)
	       case Open.text, getS($)
	       of false      then {self close} 'NONE'
	       elseof Result then 'SOME'({ByteString.make Result})
	       end
	    end
	 end
      in
	 fun {OpenShellFun P A}
	    {New Shell init(P A)}
	 end
	 fun {PutShellFun S C}
	    {S cmd(C)}
	    unit
	 end
	 fun {GetShellFun S}
	    {S show($)}
	 end
      end
      
      %% Create Interface
      GtkCoreOz = 'GtkCoreOz'(pointerToObject      : PointerToObject
			      objectToPointer      : ObjectToPointer
			      removeObject         : RemoveObject
			      vaArgListToOzList    : VaArgListToOzList
			      signalConnect        : SignalConnect
			      signalDisconnect     : SignalDisconnect
			      signalHandlerBlock   : SignalHandlerBlock
			      signalHandlerUnblock : SignalHandlerUnblock
			      signalEmit           : SignalEmit
			      allocateGdkColor     : AllocateGdkColor
			      freeGdkColor         : FreeGdkColor
			      getGdkEvent          : GetGdkEvent
			      freeGdkRectangle     : FreeGdkRectangle
			      createGList          : CreateGList
			      comboGetEntry        : Signal.comboGetEntry
			      comboGetList         : Signal.comboGetList
			      fileSelectionGetOkButton : Signal.fileSelectionGetOkButton
			      fileSelectionGetCancelButton : Signal.fileSelectionGetCancelButton
			      colorSelectionSetColor : Signal.colorSelectionSetColor
			      colorSelectionGetColor : Signal.colorSelectionGetColor
			      widgetSizeRequest : Signal.widgetSizeRequest
			      widgetGetChildRequisition : Signal.widgetGetSizeRequisition
			      widgetGetPointer : Signal.widgetGetPointer
			      buttonBoxGetChildSizeDefault : Signal.buttonBoxGetChildSizeDefault
			      buttonBoxGetChildIpaddingDefault :
				 Signal.buttonBoxGetChildIpaddingDefault
			      buttonBoxGetChildSize : Signal.buttonBoxGetChildSize
			      buttonBoxGetChildIpadding : Signal.buttonBoxGetChildIpadding
			      clistGetText : Signal.clistGetText
			      clistGetPixmap : Signal.clistGetPixmap
			      clistGetPixtest : Signal.clistGetPixtext
			      clistGetSelectionInfo : Signal.clistGetSelectionInfo
			      ctreeNodeGetText : Signal.ctreeNodeGetText
			      ctreeNodeGetPixmap : Signal.ctreeNodeGetPixmap
			      ctreeNodeGetPixtext : Signal.ctreeNodeGetPixtext
			      ctreeGetNodeInfo : Signal.ctreeGetNodeInfo
			      gtkImageGet : Signal.gtkImageGet
			      editableGetChars : Signal.editableGetChars
			      textBackwardDelete : Signal.textBackwardDelete
			      textForwardDelete : Signal.textForwardDelete
			      realToString : fun {$ R}
					    {ByteString.make {Float.toString R}}
					     end
			      srand                : fun {$ I} {OS.srand I} unit end
			      rand                 : fun {$ _} {OS.rand} end
			      openShell : OpenShellFun
			      putShell : PutShellFun
			      getShell : GetShellFun
			      exit                 : Exit)
      
      %% Start dispatcher
      Dispatcher = {New DispatcherObject create}
   end
end
