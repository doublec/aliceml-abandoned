%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 1999
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   System(show)
export
   Env
define
   BuiltinTable =
   builtinTable('System.show': fun {$ X} {System.show X} '#' end)

   Env = env('false': false
	     'true': true
	     'nil': nil
%--**	     'cons'
%--**	     'ref'
	     'Match': {NewName}
	     'Bind': {NewName}
	     'eq': fun {$ X#Y} X == Y end
%--**	     'assign'
	     'builtin': fun {$ S} BuiltinTable.{VirtualString.toAtom S} end
	     '<': fun {$ X#Y} X < Y end
	     '+': fun {$ X#Y} X + Y end
	     '*': fun {$ X#Y} X * Y end)
end
