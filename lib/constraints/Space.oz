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
   %% To come soon
   
   %% Interface Functions
   fun {SpaceFun P}
      {Space.new proc {$ Root}
		    Root = case {Procedure.arity P}
			   of 1 then {P}
			   [] 2 then {P unit}
			   end
		 end}
   end
   fun {AskFun S}
      case {Space.ask S}
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
      [] 'RANGE'(L U) then {Space.commit S L#U}
      end
      unit
   end
   fun {InjectFun S P}
      {Space.inject S proc {$ Root}
			 _ = {P Root}
		      end}
      unit
   end
   fun {MergeFun S}
      {Space.merge S}
   end
   fun {EqFun A B}
      A == B
   end
   
   %% Export Interface
   AliceSpace = 'Space'('$space'         : _
			'$status'        : _
			'$choice'        : _
			'FAILED'         : 'FAILED'
			'SUCCEEDED'      : 'SUCCEEDED'
			'ALTERNATIVES'   : fun {$ N} 'ALTERNATIVES'(N) end
			'\'FAILED'       : _
			'\'SUCCEEDED'    : _
			'\'ALTERNATIVES' : _
			'SINGLE'         : fun {$ N} 'SINGLE'(N) end
			'RANGE'          : fun {$ L U} 'RANGE'(L U) end
			'\'SINGLE'       : _
			'\'RANGE'        : _
			'space'          : SpaceFun
			'ask'            : AskFun
			'clone'          : CloneFun
			'commit'         : CommitFun
			'inject'         : InjectFun
			'merge'          : MergeFun
			'eq'             : EqFun)
end
