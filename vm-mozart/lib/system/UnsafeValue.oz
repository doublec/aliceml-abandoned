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
   fun {Proj Value Labels I}
      case Labels.(I + 1) of 'NUM'(I) then Value.I
      [] 'ALPHA'(S) then Value.{String.toAtom S}
      end
   end

   Value =
   'Value'('cast': fun {$ A} A end
	   'same': System.eq
	   'proj': Proj
	   'projTuple': fun {$ Value _ I} Value.(I + 1) end
	   'tag': fun {$ Value _} 'ALPHA'({ByteString.make {Label Value}}) end
	   'projTagged': Proj
	   'con': fun {$ _} raise notImplemented end end %--**
	   'projConstructed': Proj
	   'conName':
	      fun {$ Value}
		 'ExId'({ByteString.make {System.printName Value}})
	      end)
end
