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
   OzPickle at 'x-oz://boot/Pickle'
   Pickle at 'Pickle.ozf'
   PrimitiveTable(importOzModule)
   Scheduler(object)
export
   Link
define
   ModuleTable = {NewDictionary}

   fun {LinkInterpreterRun _ TaskStack}
      case TaskStack of linkFrame(_ Url)|Rest then
	 Component = try {Pickle.load Url}
		     catch system(os(os ...) ...) then {Pickle.load Url#'.stc'}
		     end
	 {System.showError 'loading '#Url}
	 %--** failed future on link failure
	 Imports = Component.1
	 BodyClosure = Component.2
	 N = {Width Imports}
	 Modules = {MakeTuple vector N}
      in
	 for I in 1..N do Url2 in
	    Url2 = {URL.toVirtualString {URL.resolve Url Imports.I.2}}
	    Modules.I = {Link Url2}
	 end
	 continue(arg(Modules) {BodyClosure.1.1.pushCall BodyClosure Rest})
      end
   end

   fun {LinkInterpreterPushCall Closure TaskStack}
      case Closure of closure(link(_) Url) then
	 linkFrame(LinkInterpreter Url)|TaskStack
      end
   end

   LinkInterpreter =
   linkInterpreter(run: LinkInterpreterRun
		   handle:
		      fun {$ Debug Exn TaskStack}
			 case TaskStack of Frame|Rest then
			    exception(Frame|Debug Exn Rest)
			 end
		      end
		   pushCall: LinkInterpreterPushCall)

   local
      ModuleManager = {New Module.manager init}
   in
      fun {LinkNative Url}
	 try OzFunctor OzModule in
	    OzFunctor = try {OzPickle.load Url}
			catch _ then%error(dp(generic ...) ...) then
			   {OzPickle.load Url#'.ozf'}
			end
	    OzModule = {ModuleManager apply(OzFunctor $)}
	    {PrimitiveTable.importOzModule OzModule.module}
	 catch error(url(load ...) ...) then unit
	 [] error(dp(generic ...) ...) then unit
	 end
      end
   end

   proc {LinkAlice Url ?Module}
      Closure = closure(link(LinkInterpreter) Url)
   in
      Module = transient({NewCell byneed(Closure)})
   end

   proc {Link Url ?Module} Key in
      Key = {URL.toAtom Url}
      case {Dictionary.condGet ModuleTable Key unit} of unit then
	 case {Dictionary.condGet ModuleTable {URL.toAtom Url#'.stc'} unit}
	 of unit then
	    Module = case {LinkNative Url} of unit then {LinkAlice Url}
		     elseof M then M
		     end
	    ModuleTable.Key := Module
	 elseof M then Module = M
	 end
      elseof M then Module = M
      end
   end
end
