%%%
%%% Author:
%%%   Andreas Rossberg <rossberg@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Andreas Rossberg, 2001-2003
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
export
   'UnsafeCell$': Cell
define
   Stamp = {NewCell 0}
   fun {NewStamp} Old New in
	{Exchange Stamp Old New}
	New = Old + 1
	Old
   end

   % type 'a cell = stamp * 'a ref

   Cell = 'Cell'(
      'cell':      fun sited {$ X} {NewStamp}#{NewCell X} end
      'content' :  fun {$ C} {Access C.2} end
      'replace' :  fun {$ C X} {Assign C.2 X} unit end
      'Map$' :     Map
   )

   fun {Key C} C.1 end

   % type ('a,'b) map = (stamp, ('a cell * 'b) list) dictionary

   fun {Lookup Es C}
      case Es
      of nil then 'NONE'
      [] E|_ andthen E.1 == C then 'SOME'(E.2)
      [] _|Er then {Lookup Er C}
      end
   end

   proc {Delete Es C Es2 X}
      case Es
      of nil then Es2 = unit
      [] E|Er andthen E.1 == C then Es2 = Er X = E.2
      [] E|Er then Es3 in {Delete Er C Es3 X}
      			  Es2 = case Es3
				of unit then unit
				[] _ then E|Es3
				end
      end
   end

   SizeIndex = size
   fun {Size M} {Dictionary.get M SizeIndex} end
   proc {ResetSize M}
      {Dictionary.put M SizeIndex 0}
   end
   proc {IncSize M N}
      {Dictionary.put M SizeIndex {Dictionary.get M SizeIndex} + N}
   end

   fun {Items M}
      N = {Size M}
      {Dictionary.remove M SizeIndex}
      Is = {Dictionary.items M}
      {Dictionary.put M SizeIndex N}
   in
      Is
   end

   fun {Entries M}
      N = {Size M}
      {Dictionary.remove M SizeIndex}
      Es = {Dictionary.entries M}
      {Dictionary.put M SizeIndex N}
   in
      Es
   end

   Map = 'Map'(
      'map':        fun {$ unit}
			M = {Dictionary.new}
		    in
			{ResetSize M}
			M
		    end
      'clone':      Dictionary.clone
      'size':       fun {$ M} {Size M} end
      'isEmpty':    fun {$ M} {Size M} == 0 end
      'deleteAll':  fun {$ M}
			{Dictionary.removeAll M}
			{ResetSize M}
			unit
		    end

      'lookup':     fun {$ M C}
			K = {Key C}
		    in
			if {Dictionary.member M K}
			then {Lookup {Dictionary.get M K} C}
			else 'NONE'
			end
		    end

      'insertWithi':fun {$ F M C X}
			K  = {Key C}
			Es = if {Dictionary.member M K}
			     then Es = {Dictionary.get M K} X0 in
			          case {Delete Es C $ X0}
				  of unit then {IncSize M 1} C#X|Es
				  [] Es2 then case {Procedure.arity F}
					      of 4 then {F C X0 X}
					      [] 2 then {F C#X0#X}
					      end|Es2
				  end
			     else {IncSize M 1} C#X|nil
			     end
		    in
			{Dictionary.put M K Es}
			unit
		    end

      'deleteWith': fun {$ F M C}
			K = {Key C}
		    in
			if {Dictionary.member M K}
			then case {Delete {Dictionary.get M K} C $ _}
			     of unit then {F C _}
			     [] nil then {IncSize M ~1} {Dictionary.remove M K}
			     [] Es then {IncSize M ~1} {Dictionary.put M K Es}
			     end
			else {F C _}
			end
			unit
		    end

      'app':	    fun {$ F M}
			P = case {Procedure.arity F}
			    of 2 then proc {$ _#X} {F X _} end
			    [] _ then proc {$ _#X}
					   {Procedure.apply F
						 {Append {Record.toList X} [_]}}
				      end
			    end
		    in
			{List.forAll {Items M}
				     proc {$ Es} {List.forAll Es P} end}
			unit
		    end
      'appi':	    fun {$ F M}
			P = case {Procedure.arity F}
			    of 3 then proc {$ C#X} {F C X _} end
			    [] 2 then proc {$ C#X} {F C#X _} end
			    end
		    in
			{List.forAll {Items M}
				     proc {$ Es} {List.forAll Es P} end}
			unit
		    end
      'modify':	    fun {$ F M}
			P = case {Procedure.arity F}
			    of 2 then fun {$ C#X} C#{F X} end
			    [] _ then fun {$ C#X} R in
					  {Procedure.apply F
						 {Append {Record.toList X} [R]}}
					  C#R
				      end
			    end
		    in
			{List.forAll {Entries M}
				     proc {$ K#Es}
				         {Dictionary.put M K {List.map Es P}}
				     end}
			unit
		    end
      'modifyi':    fun {$ F M}
			P = case {Procedure.arity F}
			    of 3 then fun {$ C#X} C#{F C X} end
			    [] 2 then fun {$ C#X} C#{F C#X} end
			    end
		    in
			{List.forAll {Entries M}
				     proc {$ K#Es}
				         {Dictionary.put M K {List.map Es P}}
				     end}
			unit
		    end
      'filter':	    fun {$ F M}
			P = case {Procedure.arity F}
			    of 2 then fun {$ _#X} {F X} end
			    [] _ then fun {$ _#X} R in
					  {Procedure.apply F
						 {Append {Record.toList X} [R]}}
					  R
				      end
			    end
		    in
			{List.forAll {Entries M}
				     proc {$ K#Es}
				         {Dictionary.put M K {List.filter Es P}}
				     end}
			unit
		    end
      'filteri':    fun {$ F M}
			P = case {Procedure.arity F}
			    of 3 then fun {$ C#X} {F C X} end
			    [] 2 then fun {$ C#X} {F C#X} end
			    end
		    in
			{List.forAll {Entries M}
				     proc {$ K#Es}
				         {Dictionary.put M K {List.filter Es P}}
				     end}
			unit
		    end
      'fold':	    fun {$ F Y M}
			P = case {Procedure.arity F}
			    of 3 then fun {$ _#X Y} {F X Y} end
			    [] 2 then fun {$ _#X Y} {F X#Y} end
			    end
		    in
			{List.foldR {Items M}
				    fun {$ Es Y} {List.foldR Es P Y} end Y}
		    end
      'foldi':	    fun {$ F Y M}
			P = case {Procedure.arity F}
			    of 4 then fun {$ C#X Y} {F C X Y} end
			    [] 2 then fun {$ C#X Y} {F C#X#Y} end
			    end
		    in
			{List.foldR {Items M}
				    fun {$ Es Y} {List.foldR Es P Y} end Y}
		    end
      'find':	    fun {$ F M}
			P = case {Procedure.arity F}
			    of 2 then fun {$ _#X} {F X} end
			    [] _ then proc {$ _#X R}
					   {Procedure.apply F
						 {Append {Record.toList X} [R]}}
				      end
			    end
		    in
			case {Find2 {Items M} fun {$ Es} {Find Es P} end}
			of 'SOME'(_#Y) then 'SOME'(Y)
			[] 'NONE' then 'NONE'
			end
		    end
      'findi':	    fun {$ F M}
			P = case {Procedure.arity F}
			    of 3 then fun {$ C#X} {F C X} end
			    [] 2 then F
			    end
		    in
			{Find2 {Items M} fun {$ Es} {Find Es P} end}
		    end
   )

   fun {Find Xs P}
      case Xs
      of nil then 'NONE'
      [] X|_ andthen {P X} then 'SOME'(X)
      [] _|Xr then {Find Xr P}
      end
   end

   fun {Find2 Xs F}
      case Xs
      of nil then 'NONE'
      [] X|_ andthen Y = {F X} in Y == 'SOME'(_) then Y
      [] _|Xr then {Find2 Xr F}
      end
   end
end
