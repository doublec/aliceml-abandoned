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
   Pickle at 'Pickle.ozf'
   Scheduler(object)
export
   Link
define
   ModuleTable = {NewDictionary}

   proc {Link Url ?Module}
{System.showError 'unpickling '#Url}
      Component = try {Pickle.load Url}
		  catch system(os(os ...) ...) then {Pickle.load Url#'.stc'}
		  end
      Imports = Component.1
      N = {Width Imports}
      Modules = {MakeTuple vector N}
   in
      for I in 1..N do
	 Modules.I = {Link {URL.toVirtualString {URL.resolve Url Imports.I.2}}}
      end
      {Scheduler.object newThread(Component.2 arg(Modules) ?Module)}
      {Scheduler.object run()}
      ModuleTable.{URL.toAtom Url} := Module
   end
end
