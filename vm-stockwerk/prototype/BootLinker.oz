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
   Property(get)
   Pickle(load) at 'Pickle.ozf'
   PrimitiveTable(importOzModule)
   Scheduler(object)
export
   Link
   GetInitialTable
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

   Trace = {NewCell {OS.getEnv 'ALICE_TRACE_BOOT_LINKER'} \= false}

   ModuleTable = {NewDictionary}

   local
      ModuleManager = {New Module.manager init}
      AliceHome = {Property.get 'alice.home'}
   in
      for Url in Natives do OzModule Module in
	 OzModule = {ModuleManager link(url: AliceHome#Url#'.ozf' $)}
	 Module = {PrimitiveTable.importOzModule OzModule.module}
	 ModuleTable.{VirtualString.toAtom Url} := NONE#Module
      end
   end

   proc {Link Url ?Module} Key in
      Key = {URL.toAtom Url}
      case {Dictionary.condGet ModuleTable Key unit} of unit then
	 if {Access Trace} then
	    {System.showError '[boot-linker] loading '#Url}
	 end
	 case {Pickle.load Url#'.stc'} of tag(!EVALUATED Sign X) then
	    Module = X
	    ModuleTable.Key := Sign#Module
	 [] tag(!UNEVALUATED BodyClosure Imports Sign) then N Modules in
	    N = {Width Imports}
	    Modules = {MakeTuple vector N}
	    for I in 1..N do Url2 in
	       Url2 = {URL.toVirtualString {URL.resolve Url Imports.I.1}}
	       Modules.I = {Link Url2}
	    end
	    {Scheduler.object newThread(BodyClosure arg(Modules) ?Module)}
	    {Scheduler.object run()}
	    ModuleTable.Key := Sign#Module
	 end
      elseof M then Module = M
      end
   end

   fun {GetInitialTable}
      {List.toTuple vector
       {Map {Dictionary.items ModuleTable}
	fun {$ Key#(Sign#Module)}
	   tuple({ByteString.make Key} Sign Module)
	end}}
   end
end
