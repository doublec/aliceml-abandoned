%%%
%%% Authors:
%%%   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Thorsten Brunklaus, 2002
%%%
%%% Last Change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   Search
export
   'UnsafeSearch$' : UnsafeSearch
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
   fun {MakeOption V}
      case V
      of [X] then 'SOME'(X)
      [] nil then 'NONE'
      end
   end
   fun {MakeBound V}
      case V
      of [X] then 'BOUND_SOME'(X)
      [] nil then 'BOUND_NONE'
      [] cut then 'BOUND_CUT'
      end
   end
   %% Interface Functions
   fun {SearchOneFun P}
      {MakeOption {Search.base.one {MakeScript P}}}
   end
   fun {SearchOneDepthFun P Rcd}
      {MakeOption {Search.one.depth {MakeScript P} Rcd _}}
   end
   fun {SearchOneDepthSFun P Rcd}
      {MakeOption {Search.one.depthS {MakeScript P} Rcd _}}
   end
   fun {SearchOneBoundFun P Bound Rcd}
      {MakeBound {Search.one.bound {MakeScript P} Bound Rcd _}}
   end
   fun {SearchOneBoundSFun P Bound Rcd}
      {MakeBound {Search.one.boundS {MakeScript P} Bound Rcd _}}
   end
   fun {SearchOneIterFun P Rcd}
      {MakeOption {Search.one.iter {MakeScript P} Rcd _}}
   end
   fun {SearchOneIterSFun P Rcd}
      {MakeOption {Search.one.iterS {MakeScript P} Rcd _}}
   end
   fun {SearchOneLDSFun P Rcd}
      {MakeOption {Search.one.lds {MakeScript P} Rcd _}}
   end
   fun {SearchOneLDSSFun P Rcd}
      {MakeOption {Search.one.ldsS {MakeScript P} Rcd _}}
   end
   fun {SearchAllFun P}
      {Search.base.all {MakeScript P}}
   end
   fun {SearchAllDepthFun P Rcd}
      {Search.all {MakeScript P} Rcd _}
   end
   fun {SearchAllDepthSFun P Rcd}
      {Search.allS {MakeScript P} Rcd _}
   end
   fun {SearchBestFun P Order}
      {MakeOption {Search.base.best {MakeScript P} {MakeOrder Order}}}
   end
   fun {SearchBestBABFun P Order Rcd}
      {MakeOption {Search.best.bab {MakeScript P} {MakeOrder Order} Rcd _}}
   end
   fun {SearchBestBABSFun P Order Rcd}
      {MakeOption {Search.best.babS {MakeScript P} {MakeOrder Order} Rcd _}}
   end
   fun {SearchBestRestartFun P Order Rcd}
      {MakeOption {Search.best.restart {MakeScript P} {MakeOrder Order} Rcd _}}
   end
   fun {SearchBestRestartSFun P Order Rcd}
      {MakeOption {Search.best.restartS {MakeScript P} {MakeOrder Order} Rcd _}}
   end
   %% Create Search Interface
   UnsafeSearch = 'UnsafeSearch'('searchOne' : SearchOneFun
				 'searchOneDepth' : SearchOneDepthFun
				 'searchOneDepthS' : SearchOneDepthSFun
				 'searchOneBound' : SearchOneBoundFun
				 'searchOneBoundS' : SearchOneBoundSFun
				 'searchOneIter' : SearchOneIterFun
				 'searchOneIterS' : SearchOneIterSFun
				 'searchOneLDS' : SearchOneLDSFun
				 'searchOneLDSS' : SearchOneLDSSFun
				 'searchAll' : SearchAllFun
				 'searchAllDepth' : SearchAllDepthFun
				 'searchAllDepthS' : SearchAllDepthSFun
				 'searchBest' : SearchBestFun
				 'searchBestBAB' : SearchBestBABFun
				 'searchBestBABS' : SearchBestBABSFun
				 'searchBestRestart' : SearchBestRestartFun
				 'searchBestRestartS' : SearchBestRestartSFun
				)
end
