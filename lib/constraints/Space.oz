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
   'Space$' : AliceSpace
define
   %% Type Variables
   SpaceType
   StatusType
   ChoiceType
   
   %% Interface Functions
   fun {SpaceFun P}
      {Space.new proc {$ Root}
		    Root = {P unit}
		 end}
   end
   fun {StatusFun S}
      case {Space.status S}
      of failed          then 'FAILED'
      [] succeeded       then 'SUCCEEDED'
      [] alternatives(N) then 'ALTERNATIVES'(N)
      end
   end
   fun {CloneFun S}
      {Space.clone S}
   end
   fun {CommitFun S C}
      case C
      of 'SINGLE'(N)  then {Space.commit S N}
      [] 'RANGE'(L U) then {Space.commit S L#N}
      end
      unit
   end
   fun {InjectFun S P}
      {Space.inject S proc {$ Root}
			 _ = {P Root}
		      end}
   end
   fun {MergeFun S}
      {Space.merge S}
   end
   fun {EqFun A B}
      A == B
   end

   %% Export Interface
   AliceSpace = 'Space'('$space'  : SpaceType
			'$status' : StatusType
			'$choice' : ChoiceType
			'space'   : SpaceFun
			'clone'   : CloneFun
			'commit'  : CommitFun
			'inject'  : InjectFun
			'merge'   : MergeFun
			'eq'      : EqFun)
end
