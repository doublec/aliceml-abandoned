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
   %% Convert Alice Scripts/Orders to Oz Orders
   fun {MakeScript P}
      proc {$ R}
	 R = case {Procedure.arity P}
	     of 1 then {P}
	     [] 2 then {P unit}
	     end
      end
   end
   fun {MakeOrder O}
      proc {$ A B}
	 _ = case {Procedure.arity O}
	     of 2 then {O A#B}
	     [] 3 then {O A B}
	     end
      end
   end
   %% Interface Functions
   fun {ExploreOneFun P}
      {Explorer.one {MakeScript P}}
      unit 
   end
   fun {ExploreOneBABFun P O}
      {Explorer.one {MakeScript P} {MakeOrder O}}
      unit
   end
   fun {ExploreAllFun P}
      {Explorer.all {MakeScript P}}
      unit
   end
   fun {ExploreAllBABFun P O}
      {Explorer.all {MakeScript P} {MakeOrder O}}
      unit
   end
   fun {ExploreBestFun P O}
      {Explorer.best {MakeScript P} {MakeOrder O}}
      unit
   end

   %% Setup Alice Inspector
   {Inspector.configure nothing _}
   %% Change Default Explorer Bindings
   {Explorer.object option(visual title:'Alice Explorer')}
   
   %% Create Explorer Interface
   UnsafeExplorer = 'UnsafeExplorer'('exploreOne'    : ExploreOneFun
				     'exploreOneBAB' : ExploreOneBABFun
				     'exploreAll'    : ExploreAllFun
				     'exploreAllBAB' : ExploreAllBABFun
				     'exploreBest'   : ExploreBestFun)
end
