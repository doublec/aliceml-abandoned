%%%
%%% Author:
%%%   Thorsten Brunklaus <bruni@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Thorsten Brunklaus, 2001
%%%
%%% Last Change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor $
import
   System(printName)
   Word(toInt) at 'x-oz://boot/Word'
   Inspector('nodes' : TreeNodes)
   HelperComponent('nodes' : Helper) at 'Helper'
export
   atomLayoutObject        : AtomLayoutObject
   freeLayoutObject        : FreeLayoutObject
   freeGrLayoutObject      : FreeGrLayoutObject
   futureLayoutObject      : FutureLayoutObject
   futureGrLayoutObject    : FutureGrLayoutObject
   nameLayoutObject        : NameLayoutObject
   nameGrLayoutObject      : NameGrLayoutObject
   procedureLayoutObject   : ProcedureLayoutObject
   byteStringLayoutObject  : ByteStringLayoutObject
   wordLayoutObject        : WordLayoutObject
   tupleLayoutObject       : TupleLayoutObject
   vectorIndLayoutObject   : VectorIndLayoutObject
   vectorGrIndLayoutObject : VectorGrIndLayoutObject
   listLayoutObject        : ListLayoutObject
   tupleGrLayoutObject     : TupleGrLayoutObject
   listGrLayoutObject      : ListGrLayoutObject
define
   %% Import all needed LayoutObjects
   local
      LayoutObjects = TreeNodes.'layout'
   in
      IntLayoutObject             = LayoutObjects.intLayoutObject
      OzFreeLayoutObject          = LayoutObjects.freeLayoutObject
      OzFreeGrLayoutObject        = LayoutObjects.freeGrLayoutObject
      OzFutureLayoutObject        = LayoutObjects.futureLayoutObject
      OzFutureGrLayoutObject      = LayoutObjects.futureGrLayoutObject
      OzProcedureLayoutObject     = LayoutObjects.procedureLayoutObject
      LabelTupleLayoutObject      = LayoutObjects.labelTupleLayoutObject
      LabelTupleIndLayoutObject   = LayoutObjects.labelTupleIndLayoutObject
      PipeTupleLayoutObject       = LayoutObjects.pipeTupleLayoutObject
      LabelTupleGrLayoutObject    = LayoutObjects.labelTupleGrLayoutObject
      LabelTupleGrIndLayoutObject = LayoutObjects.labelTupleGrIndLayoutObject
      PipeTupleGrLayoutObject     = LayoutObjects.pipeTupleGrLayoutObject
   end

   %%
   %% Simple Alice Objects
   %%
   
   class AtomLayoutObject from IntLayoutObject
      meth createRep(PrintStr LengthStr)
	 Value = @value
      in
	 case Value
	 of '#[]' then type <- vector
	 [] nil   then
	    value <- '[]'
	    type  <- constructor
	 else type <- constructor
	 end
	 {Helper.convert {Atom.toString @value} PrintStr LengthStr}
      end
   end

   class FreeLayoutObject from OzFreeLayoutObject
      meth createRep(PrintStr LengthStr)
	 PrintStr = '_hole'
	 LengthStr = PrintStr
      end
   end
   
   class FreeGrLayoutObject from OzFreeGrLayoutObject
      meth createRep(PrintStr LengthStr)
	 PrintStr = '_hole'
	 LengthStr = PrintStr
      end
   end

   local
      class AliceLayoutObject
	 meth createRep(PrintStr LengthStr)
	    PrintStr = case {self checkFutureType(@value $)}
		       of '<Fut>'    then '_future'
		       [] '<Failed>' then '_failed'
		       [] '<ByNeed>' then '_lazy'
		       end
	    LengthStr = PrintStr
	 end
      end
   in
      class FutureLayoutObject from OzFutureLayoutObject AliceLayoutObject
	 meth createRep(PrintStr LengthStr)
	    AliceLayoutObject, createRep(PrintStr LengthStr)
	 end
      end

      class FutureGrLayoutObject from OzFutureGrLayoutObject AliceLayoutObject
	 meth createRep(PrintStr LengthStr)
	    AliceLayoutObject, createRep(PrintStr LengthStr)
	 end
      end
   end
   
   local
      class AliceLayoutObject
	 meth createRep(PrintStr LengthStr)
	    Value = @value
	 in
	    PrintStr  = case Value
			of false then type <- constructor 'false'
			[] true  then type <- constructor 'true'
			[] unit  then type <- tuple '()'
			elsecase {System.printName Value}
			of ''   then 'unknown' %% Hack Alert
			[] Name then Name
			end
	    LengthStr = PrintStr
	 end
      end
   in
      class NameLayoutObject from IntLayoutObject AliceLayoutObject
	 meth createRep(PrintStr LengthStr)
	    AliceLayoutObject, createRep(PrintStr LengthStr)
	 end
      end

      class NameGrLayoutObject from OzFreeGrLayoutObject AliceLayoutObject
	 meth createRep(PrintStr LengthStr)
	    AliceLayoutObject, createRep(PrintStr LengthStr)
	 end
      end
   end
   
   class ProcedureLayoutObject from OzProcedureLayoutObject
      meth createRep(PrintStr LengthStr)
	 PrintStr  = '_fn'
	 LengthStr = PrintStr
      end
   end

   class ByteStringLayoutObject from IntLayoutObject
      meth createRep(PrintStr LengthStr)
	 {Helper.convertBS @value PrintStr LengthStr}
      end
   end
   
   class WordLayoutObject from IntLayoutObject
      meth createRep(PrintStr LengthStr)
	 Value = @value
      in
	 PrintStr  = '0w'#{Word.toInt Value} %% More to be determined
	 LengthStr = PrintStr
      end
   end

   %%
   %% Container Objects
   %%

   class TupleLayoutObject from LabelTupleLayoutObject
      meth noSep($)
	 true
      end
      meth isVert($)
	 case {Label @value}
	 of 'Promise__' then false
	 [] 'Package__' then false
	 else true
	 end
      end
   end

   class TupleGrLayoutObject from LabelTupleGrLayoutObject
      meth noSep($)
	 true
      end
      meth isVert($)
	 case {Label @value}
	 of 'Promise__' then false
	 [] 'Package__' then false
	 else true
	 end
      end
   end

   class VectorIndLayoutObject from LabelTupleLayoutObject LabelTupleIndLayoutObject
      meth noSep($)
	 true
      end
      meth adjustLayout(LXDim BXDim)
	 case @type
	 of vector then LabelTupleLayoutObject, adjustLayout(LXDim BXDim)
	 [] conval then LabelTupleIndLayoutObject, adjustLayout(LXDim BXDim)
	 end
      end
   end

   class VectorGrIndLayoutObject from LabelTupleGrLayoutObject LabelTupleGrIndLayoutObject
      meth noSep($)
	 true
      end
      meth adjustLayout(LXDim BXDim)
	 case @type
	 of vector then LabelTupleGrLayoutObject, adjustLayout(LXDim BXDim)
	 [] conval then LabelTupleGrIndLayoutObject, adjustLayout(LXDim BXDim)
	 end
      end
   end
   
   class ListLayoutObject from PipeTupleLayoutObject
      meth noSep($)
	 true
      end
   end

   class ListGrLayoutObject from PipeTupleGrLayoutObject
      meth noSep($)
	 true
      end
   end
end
