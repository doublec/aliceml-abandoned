%%%
%%% Authors:
%%%   Andreas Rossberg <rossberg@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Andreas Rossberg, 2001
%%%
%%% Last Change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor $
import OS
export 'UnsafeRand$' : UnsafeRand
define
    UnsafeRand = 'UnsafeRand'(rand:       fun {$ unit} {OS.rand} end
			      srand:      fun {$ Seed} {OS.srand Seed} unit end
			      randLimits: fun {$ unit} Min Max in
					      {OS.randLimits Min Max}
					      Min#Max
					  end)
end
