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
   Env = env(~1: name(false)
	     ~2: name(true)
	     ~3: name(nil)
	     ~4: con('::')
	     ~5: con(ref)
	     ~6: con(match)
	     ~7: con(bind)
	     ~8: fn(fun {$ X#Y} X == Y end
		    fun {$ X Y} X == Y end)
	     ~9: fn(fun {$ _#_} raise notImplemented end end   %--**
		    fun {$ _ _} raise notImplemented end end))
end
