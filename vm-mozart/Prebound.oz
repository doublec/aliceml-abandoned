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
export
   Env
define
   Env = env('false': false
	     'true': true
	     'nil': nil
%--**	     'cons'
%--**	     'ref'
	     'Match': {NewName}
	     'Bind': {NewName}
	     'eq': fun {$ X#Y} X == Y end
%--**	     'assign'
	     '<': fun {$ X#Y} X < Y end
	     '+': fun {$ X#Y} X + Y end
	     '*': fun {$ X#Y} X * Y end)
end
