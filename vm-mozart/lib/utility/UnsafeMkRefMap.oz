%%%
%%% Author:
%%%   Andreas Rossberg <rossberg@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Andreas Rossberg, 2001
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
export
   'UnsafeDictionary$': Dict
define
   Dict = 'Dictionary'(
      'new':       fun {$ unit} {Dictionary.new} end
      'clone':     Dictionary.clone
      'insert':    fun {$ D K X} {Dictionary.put D K X} unit end
      'delete':    fun {$ D K} {Dictionary.remove D K} unit end
      'deleteAll': fun {$ D} {Dictionary.removeAll D} unit end
/* Variant 1: Exceptions -- veeeeerrrryyy slow
      'lookup':    fun {$ D K}
		       try 'SOME'({Dictionary.get D K})
		       catch system(kernel(dict ...) ...) then 'NONE'
		       end
		   end
*/
/* Variant 2: CondGet -- be careful not to force item
      'lookup':    fun {$ D K}
		       X = {Dictionary.condGet D K NotFound}
		   in
		       if {IsFree X} orelse X == NotFound
		       then 'NONE'
		       else 'SOME'(X)
		       end
		   end
*/
/* Variant 3: Member test first -- fastest and simplest */
      'lookup':    fun {$ D K}
		       if {Dictionary.member D K}
		       then 'SOME'({Dictionary.get D K})
		       else 'NONE'
		       end
		   end
      'member':    Dictionary.member
      'isEmpty':   Dictionary.isEmpty
      'app':	   fun {$ F D}
		       P = case {Procedure.arity F}
			   of 2 then proc {$ X} {F X _} end
			   [] _ then proc {$ X}
					  {Procedure.apply F
						{Append {Record.toList X} [_]}}
				     end
			   end
		   in
		       {List.forAll {Dictionary.items D} P}
		       unit
		   end
      'appi':	   fun {$ F D}
		       P = case {Procedure.arity F}
			   of 3 then proc {$ K X} {F K X _} end
			   [] 2 then proc {$ K X} {F K#X _} end
			   end
		   in
		       {List.forAll {Dictionary.entries D} P}
		       unit
		   end
      'fold':	   fun {$ F Y D}
		       P = case {Procedure.arity F}
			   of 3 then F
			   [] 2 then fun {$ X Y} {F X#Y} end
			   end
		   in
		       {List.foldR {Dictionary.items D} P Y}
		   end
      'foldi':	   fun {$ F Y D}
		       P = case {Procedure.arity F}
			   of 4 then fun {$ K#X Y} {F K X Y} end
			   [] 2 then fun {$ K#X Y} {F K#X#Y} end
			   end
		   in
		       {List.foldR {Dictionary.entries D} P Y}
		   end
   )
end
