%%%
%%% Authors:
%%%   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Thorsten Brunklaus, 2000
%%%
%%% Last Change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   Explorer(object one best all)
   InspectorComponent('Inspector$': Inspector) at 'x-alice:/lib/tools/Inspector'
export
   'UnsafeExplorer$' : UnsafeExplorer
define
   %% Interface Functions
   fun {ExploreOneFun P}
      {Explorer.one proc {$ R}
		       R = case {Procedure.arity P}
			   of 1 then {P}
			   [] 2 then {P unit}
			   end
		    end}
      unit 
   end
   fun {ExploreOneBABFun P O}
      {Explorer.one
       proc {$ R}
	  R = case {Procedure.arity P}
	      of 1 then {P}
	      [] 2 then {P unit}
	      end
       end
       proc {$ A B}
	  _ = case {Procedure.arity O}
	      of 2 then {O A#B}
	      [] 3 then {O A B}
	      end
       end}
      unit
   end
   fun {ExploreAllFun P}
      {Explorer.all
       proc {$ R}
	  R = case {Procedure.arity P}
	      of 1 then {P}
	      [] 2 then {P unit}
	      end
       end}
      unit
   end
   fun {ExploreAllBABFun P O}
      {Explorer.all
       proc {$ R}
	  R = case {Procedure.arity P}
	      of 1 then {P}
	      [] 2 then {P unit}
	      end
       end
       proc {$ A B}
	  _ = case {Procedure.arity O}
	      of 2 then {O A#B}
	      [] 3 then {O A B}
	      end
       end}
      unit
   end
   fun {ExploreBestFun P O}
      {Explorer.best
       proc {$ R}
	  R = case {Procedure.arity P}
	      of 1 then {P}
	      [] 2 then {P unit}
	      end
       end
       proc {$ A B}
	  _ = case {Procedure.arity O}
	      of 2 then {O A#B}
	      [] 3 then {O A B}
	      end
       end}
      unit
   end

   %% Change Default Explorer Bindings: add Inspector
   {Explorer.object delete(information all)}
   {Explorer.object add(information
			proc {$ I X}
			   {Inspector.inspect '#[]'(I X) _}
			end
			label: 'Inspect')}
   
   %% Create Explorer Interface
   UnsafeExplorer = 'UnsafeExplorer'('exploreOne'    : ExploreOneFun
				     'exploreOneBAB' : ExploreOneBABFun
				     'exploreAll'    : ExploreAllFun
				     'exploreAllBAB' : ExploreAllBABFun
				     'exploreBest'   : ExploreBestFun)
end
