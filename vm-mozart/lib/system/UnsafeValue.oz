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
   System(eq printName show)
   Error(printException)
export
   'UnsafeValue$': Value
define
   fun {Handle F E X}
      {System.show F}
      {Error.printException E}
      X
   end

   fun {ProjRecord Value Labels I}
      try
	 case Labels.(I + 1) of 'NUM'(I) then Value.I
	 [] 'ALPHA'(S) then Value.{VirtualString.toAtom S}
	 end
      catch E then {Handle projRecord(Value Labels I) E 0}
      end
   end

   fun {ProjTuple Value N I}
      try
	 Value.(I + 1)
      catch E then {Handle projTuple(Value N I) E 0}
      end
   end

   fun {OzLabelToAlice X}
      try
	 case {Label X} of '|' then '::'
	 [] true then 'true'
	 [] false then 'false'
	 [] A then A
	 end
      catch E then {Handle ozLabelToAlice(X) E a}
      end
   end

   fun {FindTag Tags Label I}
      try
	 case Tags.(I + 1) of 'ALPHA'(S)
	    andthen {VirtualString.toAtom S} == Label
	 then I
	 else {FindTag Tags Label I + 1}
	 end
      catch E then {Handle findTag(Tags Label I) E 0}
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
	      end
	   'projPoly':
	      fun {$ X L}
		 try
		    case L of 'NUM'(I) then X.I
		    [] 'ALPHA'(S) then X.{VirtualString.toAtom S}
		    end
		 catch E then {Handle projPoly(X L) E 0}
		 end
	      end)
end
