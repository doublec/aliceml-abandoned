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
   UrlComponent('Url$': Url) at 'x-alice:/misc/Url.ozf'
export
   'Pickle$': Pickle_Module
define
   Pickle_Module =
   'Pickle'('loadSign':
	       fun {$ U}
		  try
		     case {Pickle.load {Url.toString U}}.'export'
		     of sig(unit) then 'NONE'
		     elseof sig(Sig) then 'SOME'(Sig)
		     else 'NONE'
		     end
		  catch error(url(load _) ...) then 'NONE'
		  end
	       end
	    'replaceSign':
	       fun {$ U Sig Filename} F1 F2 in
		  F1 = {Pickle.load {Url.toString U}}
		  F2 = {Functor.new F1.'import' sig(Sig) F1.'apply'}
		  {Pickle.saveWithCells F2 Filename '' 0}
		  unit
	       end)
end
