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
   URL(resolve toVirtualString toAtom)
   Module(manager)
   Property(get)
   Pickle(load) at 'Pickle.ozf'
   PrimitiveTable(importOzModule)
   Scheduler(object)
export
   Link
define
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

   ModuleTable = {NewDictionary}

   local
      ModuleManager = {New Module.manager init}
      AliceHome = {Property.get 'alice.home'}
   in
      for Url in Natives do OzModule Module in
	 OzModule = {ModuleManager link(url: AliceHome#Url#'.ozf' $)}
	 Module = {PrimitiveTable.importOzModule OzModule.module}
	 ModuleTable.{VirtualString.toAtom Url} := Module
      end
   end

   fun {LinkInterpreterRun _ TaskStack}
      case TaskStack of linkFrame(_ Url)|Rest then Component in
	 %--** return a failed future on link failure
	 {System.showError 'loading '#Url}
	 Component = try {Pickle.load Url}
		     catch system(os(os ...) ...) then {Pickle.load Url#'.stc'}
		     end
	 case Component of tag(!EVALUATED _ X) then continue(arg(X) Rest)
	 [] tag(!UNEVALUATED BodyClosure Imports _) then N Modules in
	    N = {Width Imports}
	    Modules = {MakeTuple vector N}
	    for I in 1..N do Url2 in
	       Url2 = {URL.toVirtualString {URL.resolve Url Imports.I.1}}
	       Modules.I = {Link Url2}
	    end
	    continue(arg(Modules) {BodyClosure.1.1.pushCall BodyClosure Rest})
	 end
      end
   end

   fun {LinkInterpreterPushCall Closure TaskStack}
      case Closure of closure(link(_) Url) then
	 linkFrame(LinkInterpreter Url)|TaskStack
      end
   end

   fun {LinkInterpreterAbstract _}
      {Exception.raiseError linkInterpreterAbstract} unit
   end

   LinkInterpreter =
   linkInterpreter(run: LinkInterpreterRun
		   handle:
		      fun {$ Debug Exn TaskStack}
			 case TaskStack of Frame|Rest then
			    exception(Frame|Debug Exn Rest)
			 end
		      end
		   pushCall: LinkInterpreterPushCall
		   abstract: LinkInterpreterAbstract)

   proc {Link Url ?Module} Key in
      Key = {URL.toAtom Url}
      case {Dictionary.condGet ModuleTable Key unit} of unit then
	 case {Dictionary.condGet ModuleTable {URL.toAtom Url#'.stc'} unit}
	 of unit then Closure in
	    Closure = closure(link(LinkInterpreter) Url)
	    Module = transient({NewCell byneed(Closure)})
	    ModuleTable.Key := Module
	 elseof M then Module = M
	 end
      elseof M then Module = M
      end
   end
end
