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
      [] 'ALPHA'(S) then Value.{String.toAtom S}
      end
   end

   fun {ProjTuple Value _ I}
      Value.(I + 1)
   end

   Value =
   'Value'('cast': fun {$ A} A end
	   'same': System.eq
	   'proj': ProjRecord
	   'projTuple': ProjTuple
	   'tag': fun {$ Value _} 'ALPHA'({ByteString.make {Label Value}}) end
	   'projTagged': ProjRecord
	   'projTaggedTuple': ProjTuple
	   'con': fun {$ _} raise notImplemented end end %--**
	   'projConstructed': ProjRecord
	   'projConstructedTuple': ProjTuple
	   'conName':
	      fun {$ Value}
		 'ExId'({ByteString.make {System.printName Value}})
	      end)
end
