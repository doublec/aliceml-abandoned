%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2002
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
export
   module: ReflectComponent
define
   ReflectComponent = tuple(Reflect)

   I_Reflect      = 1
   I_ReflectSig   = 2
   I_Unreflect    = 3
   I_UnreflectSig = 4
   I_cast         = 5

   Reflect =
   tuple(I_cast:
	    fun {$ A} A end#i_v
	 I_Reflect:
	    fun {$ tuple(_ X)} tuple(X) end#r_v
	 I_Unreflect:
	    fun {$ tuple(_ X)} X end#r_v
	 I_ReflectSig:
	    fun {$ tuple(S)} tuple(S) end#r_v
	 I_UnreflectSig:
	    fun {$ tuple(X)} tuple(X) end#r_v)
end
