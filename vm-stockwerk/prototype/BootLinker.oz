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
   OzPickle at 'x-oz://boot/Pickle'
   Pickle at 'Pickle.ozf'
   PrimitiveTable(importOzModule)
   Scheduler(object)
export
   Link
define
   ModuleTable = {NewDictionary}

   proc {DoLink Url ?Module}
      Component = try {Pickle.load Url}
		  catch system(os(os ...) ...) then {Pickle.load Url#'.stc'}
		  end
      Imports = Component.1
      N = {Width Imports}
      Modules = {MakeTuple vector N}
   in
      for I in 1..N do Url2 in
	 Url2 = {URL.toVirtualString {URL.resolve Url Imports.I.2}}
	 Modules.I = {Link Url2}
      end
      {System.showError 'applying '#Url#' to '#{Width Modules}}
      {Scheduler.object newThread(Component.2 arg(Modules) ?Module)}
      {Scheduler.object run()}
   end

   proc {Link Url ?Module} Key in
      Key = {URL.toAtom Url}
      case {Dictionary.condGet ModuleTable Key unit} of unit then
	 case {Dictionary.condGet ModuleTable {URL.toAtom Url#'.stc'} unit}
	 of unit then
	    {System.showError 'unpickling '#Url}
	    try
	       Functor = {OzPickle.load Url}
	       case {Module.link [Functor]} of [OzModule] then
		  Module = {PrimitiveTable.importOzModule OzModule}
	       end
	    catch error(url(load ...) ...) then {DoLink Url Module}
	    [] error(dp(generic ...) ...) then {DoLink Url Module}
	    end
	    ModuleTable.Key := Module
	 elseof M then Module = M
	 end
      elseof M then Module = M
      end
   end
end
