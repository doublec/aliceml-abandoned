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
   Property(get)
export
   'UnsafeValue$': UnsafeValue
define
   fun {WaitProperty P}
      try
	 {Property.get P}
      catch error(...) then
	 {WaitProperty P}
      end
   end
   BuiltinTable = {WaitProperty 'alice.builtinTable'}

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

   fun {LabelToOz Label}
      case Label of 'NUM'(I) then I
      [] 'ALPHA'(S) then
	 case {VirtualString.toAtom S} of 'true' then true
	 [] 'false' then false
	 [] '::' then '|'
	 [] A then A
	 end
      end
   end

   UnsafeValue =
   'Value'('cast': fun {$ A} A end
	   'same': System.eq
	   'awaitRequest': fun {$ X} {Value.waitQuiet X} X end
	   'proj': ProjRecord
	   'projTuple': ProjTuple
	   'tag':
	      fun {$ Value Tags} {FindTag Tags {OzLabelToAlice Value} 0} end
	   'projTagged': ProjRecord
	   'projTaggedTuple': ProjTuple
	   'con': Label
	   'projConstructed': ProjRecord
	   'projConstructedTuple': ProjTuple
	   'projPoly':
	      fun {$ X Label}
		 try X.{LabelToOz Label}
		 catch E then {Handle projPoly(X Label) E 0}
		 end
	      end
	   'prod':
	      fun {$ LabelValueVec}
		 case LabelValueVec of '#[]' then unit
		 else
		    {List.toRecord '#'
		     {Record.foldR LabelValueVec
		      fun {$ Label#Value In}
			 {LabelToOz Label}#Value|In
		      end nil}}
		 end
	      end
	   'tuple':
	      fun {$ X}
		 case X of '#[]' then unit
		 else {Adjoin X '#'}
		 end
	      end
	   'tagged':
	      fun {$ Labels I LabelValueVec}
		 {List.toRecord {LabelToOz Labels.(I + 1)}
		  {Record.foldR LabelValueVec
		   fun {$ Label#Value In}
		      {LabelToOz Label}#Value|In
		   end nil}}
	      end
	   'taggedTuple':
	      fun {$ Labels I Values}
		 {Adjoin Values {LabelToOz Labels.(I + 1)}}
	      end
	   'closure':
	      fun {$ Code Values}
		 {Handle closure(Code Values) notImplemented 0}   %--**
	      end
	   'prim':
	      fun {$ Name}
		 BuiltinTable.{String.toAtom {ByteString.toString Name}}
	      end
	   'conName':
	      fun {$ Value}
		 'ExId'({ByteString.make {System.printName Value}})
	      end
	   'inArity':
	      fun {$ F}
		 if {IsDet F} then
		    case {Procedure.arity F} of 2 then ~1
		    [] 1 then ~2
		    elseof Arity then Arity - 1
		    end
		 else ~2
		 end
	      end
	   'outArity': fun {$ _} ~2 end)
end
