%%%
%%% Authors:
%%%   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Thorsten Brunklaus, 2003
%%%
%%% Last Change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   BootName(newUnique: NewUniqueName) at 'x-oz://boot/Name'
   System(onToplevel)
export
   UnzipVec
   AliceDomainToOzDomain
   OzDomainToAliceDomain
   AlicePropToOzProp
   AliceAssignToOzAssign   
   Tell
   ExnWrapper
define
   proc {UnzipVec V V1 V2}
      I = {Width V}
   in
      V1 = {MakeTuple '#[]' I}
      V2 = {MakeTuple '#[]' I}
      {Record.forAllInd V proc {$ I X#Y}
			     V1.I = X
			     V2.I = Y
			  end}
   end

   local
      fun {MakeOzValue V}
	 case V
	 of 'SINGLE'(X)  then X
	 [] 'RANGE'(L U) then L#U
	 end
      end
      fun {Cons X Y}
	 {MakeOzValue X}|Y
      end
   in
      fun {AliceDomainToOzDomain D}
	 {Record.foldR D Cons nil}
      end
   end

   fun {OzDomainToAliceDomain Ds}
      V = {MakeTuple '#[]' {Length Ds}}
   in
      {List.forAllInd Ds proc {$ I X}
			    V.I = case X
				  of L#U then 'RANGE'(L U)
				  else 'SINGLE'(X)
				  end
			 end}
      V
   end
   
   fun {AlicePropToOzProp P}
      case P
      of 'LESS'      then '<:'
      [] 'LESSEQ'    then '=<:'
      [] 'EQUAL'     then '=:'
      [] 'NOTEQUAL'  then '\\=:'
      [] 'GREATER'   then '>:'
      [] 'GREATEREQ' then '>=:'
      end
   end

   fun {AliceAssignToOzAssign A}
      case A
      of 'MIN' then min
      [] 'MID' then mid
      [] 'MAX' then max
      end
   end

   %% Alice Exception Format
   Tell = {NewUniqueName 'FD.Tell'}

   proc {Raise E}
      {Exception.raiseError alice(Tell(cause: E))}
   end

   %% Catch tell failure exceptions in toplevel space
   fun {ExnWrapper F}
      if {Procedure.is F}
      then
	 proc {$ X R}
	    Args = if {Procedure.arity F} == 2
		   then [X R]
		   else {Append {Record.toList X} [R]}
		   end
	 in
	    if {System.onToplevel}
	    then try {Procedure.apply F Args} catch E then {Raise E} end
	    else {Procedure.apply F Args}
	    end
	 end
      else F
      end
   end
end
