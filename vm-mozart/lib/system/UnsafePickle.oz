%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2000
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   Pickle(load saveWithCells)
export
   'UnsafePickle$': UnsafePickle_Module
define
   UnsafePickle_Module =
   'UnsafePickle'('loadSign':
		     fun {$ U}
			try
			   case {Pickle.load U}.'export'
			   of sig(unit) then 'NONE'
			   elseof sig(Sig) then 'SOME'(Sig)
			   else 'NONE'
			   end
			catch error(url(load _) ...) then 'NONE'
			end
		     end
		  'replaceSign':
		     fun {$ U Sig Filename} F1 F2 in
			F1 = {Pickle.load U}
			F2 = {Functor.new F1.'import' sig(Sig) F1.'apply'}
			{Pickle.saveWithCells F2 Filename '' 0}
			unit
		     end)
end
