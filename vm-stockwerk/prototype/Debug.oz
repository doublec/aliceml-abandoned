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
   Inspector(inspect)
require
   Helper(construct: Construct)
export
   DumpTaskStack
   InspectInterpreter
define
   proc {DumpTaskStack TaskStack}
      {System.showError 'Stack Trace:'}
      {List.forAllInd TaskStack
       proc {$ I Frame}
	  {System.showError '  #'#I#': '#{Frame.1.toString Frame}}
       end}
   end

   InspectInterpreter =
   inspectInterpreter(
      run:
	 fun {$ Args TaskStack}
	    {Inspector.inspect {Construct Args}}
	    continue(Args TaskStack.2)
	 end
      handle: fun {$ Debug Exn Frame|Rest} exception(Frame|Debug Exn Rest) end
      toString: fun {$ _} 'Inspect' end)
end
