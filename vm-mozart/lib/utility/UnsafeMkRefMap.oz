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
   )
end
