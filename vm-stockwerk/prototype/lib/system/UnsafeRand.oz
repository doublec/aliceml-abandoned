%%%
%%% Authors:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%   Andreas Rossberg <rossberg@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2002
%%%   Andreas Rossberg, 2001
%%%
%%% Last Change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   OS(rand srand randLimits)
export
   module: UnsafeRandComponent
define
   UnsafeRandComponent = tuple(UnsafeRand)

   I_rand       = 1
   I_randLimits = 2
   I_srand      = 3

   UnsafeRand =
   tuple(I_rand: OS.rand#n_v
	 I_srand: fun {$ Seed} {OS.srand Seed} tuple() end#r_v
	 I_randLimits:
	    fun {$} Min Max in
	       {OS.randLimits ?Min ?Max}
	       tuple(Min Max)
	    end#n_v)
end
