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
import
   System(eq printName)
export
   'UnsafeValue$': Value
define
   fun {ProjRecord Value Labels I}
      case Labels.(I + 1) of 'NUM'(I) then Value.I
      [] 'ALPHA'(S) then Value.{VirtualString.toAtom S}
      end
   end

   fun {ProjTuple Value _ I}
      Value.(I + 1)
   end

   fun {OzLabelToAlice X}
      case {Label X} of '|' then '::'
      [] true then 'true'
      [] false then 'false'
      [] A then A
      end
   end

   fun {FindTag Tags Label I}
      case Tags.(I + 1) of 'ALPHA'(S) andthen {VirtualString.toAtom S} == Label
      then I
      else {FindTag Tags Label I + 1}
      end
   end

   Value =
   'Value'('cast': fun {$ A} A end
	   'same': System.eq
	   'proj': ProjRecord
	   'projTuple': ProjTuple
	   'tag':
	      fun {$ Value Tags} {FindTag Tags {OzLabelToAlice Value} 0} end
	   'projTagged': ProjRecord
	   'projTaggedTuple': ProjTuple
	   'con': Label
	   'projConstructed': ProjRecord
	   'projConstructedTuple': ProjTuple
	   'conName':
	      fun {$ Value}
		 'ExId'({ByteString.make {System.printName Value}})
	      end)
end
