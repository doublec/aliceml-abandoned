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
export
   DumpTaskStack
define
   proc {DumpTaskStack TaskStack}
      {System.showError 'Stack Trace:'}
      {List.forAllInd TaskStack
       proc {$ I Frame}
	  {System.showError '  #'#I#': '#{Frame.1.toString Frame}}
       end}
   end
end
