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
   Space
export
   'Search$' : AliceSearch
define
   %% Type Variables
   OrderType

   %% Interface Functions
   local
      fun {SearchOne S}
	 case {Space.ask S}
	 of failed          then nil
	 [] succeeded       then S
	 [] alternatives(N) then
	    C = {Space.clone S}
	 in
	    {Space.commit S 1}
	    case {SearchOne S}
	    of nil then {Space.commit C 2#N} {SearchOne C}
	    [] S   then S
	    end
	 end
      end
   in
      fun {SearchOneFun P}
	 S = {Space.new proc {$ Root}
			   Root = {P unit}
			end}
      in
	 case {SearchOne S}
	 of nil then 'NONE'
	 [] S   then 'SOME'({Space.merge S})
	 end
      end
   end

   local
      fun {SearchAll S}
	 case {Space.ask S}
	 of failed          then nil
	 [] succeeded       then [S]
	 [] alternatives(N) then
	    C = {Space.clone S}
	 in
	    {Space.commit S 1}
	    {Space.commit C 2#N}
	    {Append {SearchAll S} {SearchAll C}}
	 end
      end
   in
      fun {SearchAllFun P}
	 S = {Space.new proc {$ Root}
			   Root = {P unit}
			end}
      in
	 {Map {SearchAll S} Space.merge}
      end
   end

   fun {SearchBestFun P O}
      S = {Space.new proc {$ Root}
			Root = {P unit}
		     end}
      proc {Constrain S BS}
	 OR = {Space.merge {Space.clone BS}}
      in
	 {Space.inject S proc {$ NR}
			    _ = {O OR NR}
			 end}
      end
      fun {SearchBest S BS}
	 case {Space.ask S}
	 of failed          then BS
	 [] succeeded       then S
	 [] alternatives(N) then
	    C = {Space.clone S}
	 in
	    {Space.commit S 1}
	    {Space.commit C 2#N}
	    case {SearchBest S BS}
	    of NBS then
	       if BS \= NBS then {Constrain C NBS} end
	       {SearchBest C NBS}
	    end
	 end
      end
      BS = {SearchBest S S}
   in
      case {Space.ask BS}
      of succeeded then 'SOME'({Space.merge BS})
      [] _         then 'NONE'
      end
   end
   fun {ExploreOne S}
      unit
   end
   fun {ExploreAll S}
      unit
   end
   fun {ExploreBest S}
      unit
   end
   
   %% Export the Interface
   AliceSearch = 'Search'('$order'      : OrderType
			  'searchOne'   : SearchOneFun
			  'searchAll'   : SearchAllFun
			  'searchBest'  : SearchBestFun
			  'exploreOne'  : ExploreOneFun
			  'exploreAll'  : ExploreAllFun
			  'exploreBest' : ExploreBestFun)
end
