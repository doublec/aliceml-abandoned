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
   'UnsafeSpace$' : UnsafeSpace
define
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
      of merged          then 'MERGED'
      [] failed          then 'FAILED'
      [] succeeded       then 'SUCCEEDED'
      [] alternatives(N) then 'ALTERNATIVES'(N)
      end
   end
   fun {AskVerboseFun S}
      case {Space.askVerbose S}
      of suspended(F)        then 'VERBOSE_SUSPENDED'(F)
      [] merged              then 'VERBOSE_MERGED'
      [] failed              then 'VERBOSE_FAILED'
      [] succeeded(stuck)    then 'VERBOSE_SUCCEEDED_STUCK'
      [] succeeded(entailed) then 'VERBOSE_SUCCEEDED_ENTAILED'
      [] alternatives(N)     then 'VERBOSE_ALTERNATIVES'(N)
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
   fun {KillFun S}
      {Space.kill S}
      unit
   end
   fun {WaitStableFun S}
      {Space.waitStable S}
      unit
   end
   
   %% Export Interface
   UnsafeSpace = 'UnsafeSpace'('space'      : SpaceFun
			       'ask'        : AskFun
			       'askVerbose' : AskVerboseFun
			       'clone'      : CloneFun
			       'commit'     : CommitFun
			       'inject'     : InjectFun
			       'merge'      : MergeFun
			       'kill'       : KillFun
			       'waitStable' : WaitStableFun)
end
