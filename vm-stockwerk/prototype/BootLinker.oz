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
   OS(getEnv)
   System(showError)
   URL(resolve toVirtualString toAtom)
   Module(manager)
   Property(get put)
   Pickle(load) at 'Pickle.ozf'
   PrimitiveTable(importOzModule)
   Scheduler(object)
require
   Helper(construct: Construct pushCall: PushCall)
export
   Link
define
   NONE = 0
   %SOME = 1

   EVALUATED   = 0
   UNEVALUATED = 1

   local
      LibSystemNatives = ['Config' 'IO' 'OS' 'Unix' 'CommandLine' 'Component'
			  'Debug' 'Socket' 'Rand' 'Reflect']
      LibUtilityNatives = ['MkRefMap' 'Addr']
   in
      Natives = {FoldR LibSystemNatives
		 fun {$ X In} 'lib/system/Unsafe'#X|In end
		 {Map LibUtilityNatives
		  fun {$ X} 'lib/utility/Unsafe'#X end}}
   end

   TraceFlag = {NewCell {OS.getEnv 'ALICE_TRACE_BOOT_LINKER'} \= false}

   proc {Trace V}
      if {Access TraceFlag} then
	 {System.showError V}
      end
   end

   AliceHome = {Property.get 'alice.home'}

   ModuleTable = {NewDictionary}

   {Property.put 'alice.getInitialTable'
    fun {$}
       {List.toTuple vector
	{Map {Dictionary.entries ModuleTable}
	 fun {$ Key#(Sign#Module)}
	    tuple({ByteString.make Key} Sign Module)
	 end}}
    end}

   local
      ModuleManager = {New Module.manager init}
   in
      for Key in Natives do OzModule Module in
	 OzModule = {ModuleManager link(url: AliceHome#Key#'.ozf' $)}
	 Module = {PrimitiveTable.importOzModule OzModule.module}
	 ModuleTable.{VirtualString.toAtom Key} := NONE#Module
      end
   end

   proc {Link Url ?Module} Key in
      Key = {VirtualString.toAtom Url}
      {Scheduler.object newThread(taskStack: [loadFrame(LoadInterpreter Key)])}
      {Scheduler.object run()}
      Module = ModuleTable.Key.2
   end

   LoadInterpreter =
   loadInterpreter(
      run:
	 fun {$ _ TaskStack}
	    case TaskStack of loadFrame(_ Key)|Rest then
	       %--** interpreter
	       {Trace '[boot-linker] loading '#Key}
	       continue(arg({Pickle.load AliceHome#Key#'.stc'})
			linkFrame(LinkInterpreter Key)|Rest)
	    end
	 end
      handle:
	 fun {$ Debug Exn Frame|Rest}
	    exception(Frame|Debug Exn Rest)
	 end
      toString: fun {$ loadFrame(_ Key)} 'Load '#Key end)

   LinkInterpreter =
   linkInterpreter(
      run:
	 fun {$ Args TaskStack}
	    case TaskStack of linkFrame(_ Key)|Rest then
	       {Trace '[boot-linker] linking '#Key}
	       case {Construct Args} of tag(!EVALUATED Sign X) then
		  continue(arg(X) enterFrame(EnterInterpreter Key Sign)|Rest)
	       [] tag(!UNEVALUATED BodyClosure Imports Sign) then
		  ApplyFrame = applyFrame(ApplyInterpreter
					  BodyClosure Imports Key)
		  EnterFrame = enterFrame(EnterInterpreter Key Sign)
	       in
		  continue(args()
			   {Record.foldR Imports
			    fun {$ Import Rest} Key2 in
			       Key2 = {URL.toAtom {URL.resolve Key Import.2}}
			       if {Not {Dictionary.member ModuleTable Key2}}
			       then loadFrame(LoadInterpreter Key2)|Rest
			       else Rest
			       end
			    end ApplyFrame|EnterFrame|Rest})
	       end
	    end
	 end
      handle:
	 fun {$ Debug Exn Frame|Rest}
	    exception(Frame|Debug Exn Rest)
	 end
      toString: fun {$ linkFrame(_ Key)} 'Link '#Key end)

   ApplyInterpreter =
   applyInterpreter(
      run:
	 fun {$ _ TaskStack}
	    case TaskStack of applyFrame(_ BodyClosure Imports Key)|Rest then
	       {Trace '[boot-linker] applying '#Key}
	       N = {Width Imports}
	       Modules = {MakeTuple vector N}
	    in
	       for I in 1..N do Key2 in
		  Key2 = {URL.toAtom {URL.resolve Key Imports.I.2}}
		  Modules.I = ModuleTable.Key2.2
	       end
	       {PushCall arg(Modules) BodyClosure Rest}
	    end
	 end
      handle:
	 fun {$ Debug Exn Frame|Rest}
	    exception(Frame|Debug Exn Rest)
	 end
      toString: fun {$ applyFrame(_ _ _ Key)} 'Apply '#Key end)

   EnterInterpreter =
   enterInterpreter(
      run:
	 fun {$ Args TaskStack}
	    case TaskStack of enterFrame(_ Key Sign)|Rest then Module in
	       {Trace '[boot-linker] entering '#Key}
	       Module = {Construct Args}
	       ModuleTable.Key := Sign#Module
	       continue(arg(Module) Rest)
	    end
	 end
      handle:
	 fun {$ Debug Exn Frame|Rest}
	    exception(Frame|Debug Exn Rest)
	 end
      toString: fun {$ enterFrame(_ Key _)} 'Enter '#Key end)
end
