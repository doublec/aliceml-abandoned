%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%   Andreas Rossberg <rossberg@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2002
%%%   Andreas Rossberg, 2001
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
export
   module: UnsafeMkRefMapComponent
define
   UnsafeMkRefMapComponent = tuple(UnsafeMkRefMap)

   NONE = 0
   SOME = 1

   I_clone     = 1
   I_delete    = 2
   I_deleteAll = 3
   I_insert    = 4
   I_isEmpty   = 5
   I_lookup    = 6
   I_member    = 7
   I_new       = 8

   UnsafeMkRefMap =
   tuple(I_new: fun {$} {Dictionary.new} end#n_v
	 I_clone: Dictionary.clone#r_v
	 I_insert: fun {$ D K X} {Dictionary.put D K X} tuple() end#rri_v
	 I_delete: fun {$ D K} {Dictionary.remove D K} tuple() end#rr_v
	 I_deleteAll: fun {$ D} {Dictionary.removeAll D} tuple() end#r_v
	 I_lookup:
	    fun {$ D K}
	       if {Dictionary.member D K}
	       then tag(SOME {Dictionary.get D K})
	       else NONE
	       end
	    end#rr_v
	 I_member: Dictionary.member#rr_b
	 I_isEmpty: Dictionary.isEmpty#r_b)
end
