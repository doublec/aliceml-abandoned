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
   FD
   System(eq show)
   Inspector('nodes' : TreeNodes)
   HelperComponent('nodes' : Helper) at 'Helper'
export
   intCreateObject         : IntCreateObject
   floatCreateObject       : FloatCreateObject
   procedureCreateObject   : ProcedureCreateObject
   nameGrCreateObject      : NameGrCreateObject
   wordCreateObject        : WordCreateObject
   byteStringCreateObject  : ByteStringCreateObject
   cellCreateObject        : CellCreateObject
   cellGrCreateObject      : CellGrCreateObject
   tupleCreateObject       : TupleCreateObject
   fdIntCreateObject       : FDIntCreateObject
   tupleGrCreateObject     : TupleGrCreateObject
   vectorCreateObject      : VectorCreateObject
   vectorGrCreateObject    : VectorGrCreateObject
   listCreateObject        : ListCreateObject
   listGrCreateObject      : ListGrCreateObject
   recordCreateObject      : RecordCreateObject
   recordGrCreateObject    : RecordGrCreateObject
   recordIndCreateObject   : RecordIndCreateObject
   recordGrIndCreateObject : RecordGrIndCreateObject
   freeGrCreateObject      : FreeGrCreateObject
   futureGrCreateObject    : FutureGrCreateObject
   fdIntGrCreateObject     : FDIntGrCreateObject
   fsValGrCreateObject     : FSValGrCreateObject
   fsVarGrCreateObject     : FSVarGrCreateObject
define
   %% Import all needed CreateObjects
   local
      CreateObjects = TreeNodes.'create'
   in
      CreateObject             = CreateObjects.createObject
      OzIntCreateObject        = CreateObjects.intCreateObject
      OzRecordCreateObject     = CreateObjects.recordCreateObject
      PipeTupleCreateObject    = CreateObjects.pipeTupleCreateObject
      LabelTupleCreateObject   = CreateObjects.labelTupleCreateObject
      LabelTupleGrCreateObject = CreateObjects.labelTupleGrCreateObject
      OzRecordGrCreateObject   = CreateObjects.recordGrCreateObject
      PipeTupleGrCreateObject  = CreateObjects.pipeTupleGrCreateObject
      OzFDIntCreateObject      = CreateObjects.fdIntCreateObject
      OzFreeGrCreateObject     = CreateObjects.freeGrCreateObject
      OzFutureGrCreateObject   = CreateObjects.futureGrCreateObject
      OzFDIntGrCreateObject    = CreateObjects.fdIntGrCreateObject
      OzFSValGrCreateObject    = CreateObjects.fsValGrCreateObject
      OzFSVarGrCreateObject    = CreateObjects.fsVarGrCreateObject
   end

   %%
   %% Simple Objects
   %%

   class IntCreateObject from OzIntCreateObject
      meth create(Value Parent Index Visual Depth)
	 @type = number
	 CreateObject, create(Value Parent Index Visual Depth)
      end
   end

   class FloatCreateObject from IntCreateObject
      meth create(Value Parent Index Visual Depth)
	 @type = number
	 CreateObject, create(Value Parent Index Visual Depth)
      end
   end

   class ProcedureCreateObject from IntCreateObject
      meth create(Value Parent Index Visual Depth)
	 @type = function
	 CreateObject, create(Value Parent Index Visual Depth)
      end
   end

   class NameGrCreateObject from OzFreeGrCreateObject
      meth gcr(Entry Value Parent Index Visual Depth)
	 @type    = name
	 @entry   = Entry
	 @contTag = {Visual newTag($)}
	 {self handleMode({Entry getEqualStr($)} Visual)}
	 {Entry awake(self)}
	 CreateObject, create(Value Parent Index Visual Depth)
	 {Visual logVar(self Value false)}
      end
      meth handleMode(RefStr Visual)
	 @mode = {New Helper.marker create('R'#RefStr ' as ' self Visual)}
      end
   end
   
   class WordCreateObject from IntCreateObject
      meth create(Value Parent Index Visual Depth)
	 @type = number
	 CreateObject, create(Value Parent Index Visual Depth)
      end
   end

   class ByteStringCreateObject from IntCreateObject
      meth create(Value Parent Index Visual Depth)
	 @type = text
	 CreateObject, create(Value Parent Index Visual Depth)
      end
   end

   %%
   %% Container Objects
   %%

   class CellCreateObject from LabelTupleCreateObject
      attr
	 savedValue %% Saved value before Transformation
      meth createContainer
	 Visual = @visual
	 Value  = @value
      in
	 @maxWidth   = 1
	 @type       = cell
	 @label      = {New Helper.label create('ref' '(' self Visual)}
	 @brace      = {New Helper.ozAtom create(')' self 0 Visual tuple)}
	 @savedValue = Value
	 value <- ref({Access Value})
	 %% Was ContainerCreateObject before
	 CellCreateObject, adjustWidth({Visual getWidth($)} 1)
      end
      meth adjustWidth(CurWidth I)
	 width <- {Min CurWidth @maxWidth}
	 {self performInsertion(I @value {@visual getStopVar($)})}
      end
   end
   
   class CellGrCreateObject from LabelTupleGrCreateObject
      attr
	 savedValue %% Saved value before Transformation
      meth gcr(Entry Value Parent Index Visual Depth)
	 @entry      = Entry {Entry awake(self)}
	 @savedValue = Value
	 CellGrCreateObject, create(ref({Access Value}) Parent Index Visual Depth)
	 CellGrCreateObject, handleMode({Entry getEqualStr($)} Visual)
      end
      meth createContainer
	 Visual = @visual
      in
	 @maxWidth = 1
	 @type     = cell
	 @label    = {New Helper.label create('ref' '(' self Visual)}
	 @brace    = {New Helper.ozAtom create(')' self 0 Visual tuple)}
	 %% Was ContainerCreateObject before
	 CellGrCreateObject, adjustWidth({Visual getWidth($)} 1)
      end
      meth handleMode(RefStr Visual)
	 @mode = {New Helper.marker create('R'#RefStr ' as 'self Visual)}
      end
      meth adjustWidth(CurWidth I)
	 width <- {Min CurWidth @maxWidth}
	 {self performInsertion(I @value {@visual getStopVar($)})}
      end
   end

   local
      class SMLCreateContainer
	 meth smlCreateContainer
	    Visual = @visual
	    Value  = @value
	 in
	    @type     = tuple
	    @maxWidth = {Width Value}
	    @arity    = {Record.arity Value}
	    @label    = {New Helper.ozAtom create('(' self 0 Visual tuple)}
	    @brace    = {New Helper.ozAtom create(')' self 0 Visual tuple)}
	    OzRecordCreateObject, adjustWidth({Visual getWidth($)} 1)
	 end
      end
   in
      class TupleCreateObject from OzRecordCreateObject SMLCreateContainer
	 attr
	    auxfeat : Helper.tuple %% Aux Separator Class
	 meth createContainer
	    SMLCreateContainer, smlCreateContainer
	 end
      end

      class TupleGrCreateObject from OzRecordGrCreateObject SMLCreateContainer
	 attr
	    auxfeat : Helper.tuple %% Aux Separator Class
	 meth createContainer
	    SMLCreateContainer, smlCreateContainer
	 end
	 meth handleMode(RefStr Visual)
	    @mode = {New Helper.marker create('R'#RefStr ' as 'self Visual)}
	 end
      end
   end

   local
      class SMLCreateContainer
	 meth smlCreateContainer
	    Visual = @visual
	    Value  = @value
	    ValLab = {Label Value}
	 in
	    @maxWidth = {Width Value}
	    @arity    = {Record.arity Value}
	    case ValLab
	    of '#[]' then
	       @type  = vector
	       @label = {New Helper.ozAtom create('#[' self 0 Visual vector)}
	       @brace = {New Helper.ozAtom create(']' self 0 Visual vector)}
	    [] '@Array__' then
	       @type  = array
	       @label = {New Helper.prefix create('array' '{|' self Visual)}
	       @brace = {New Helper.ozAtom create('|}' self 0 Visual tuple)}
	    [] '@Thread__' then
	       @type  = 'thread'
	       @label = {New Helper.prefix create('thread' '{|' self Visual)}
	       @brace = {New Helper.ozAtom create('|}' self 0 Visual tuple)}
	    [] 'Promise__' then
	       @type  = promise
	       @label = {New Helper.prefix create('promise' '{|' self Visual)}
	       %% We need to rewrite the value to its internal represenation
	       %% Moreover, we hide the fact that the promise uses a hole
	       %% instead of a future
	       case Value of 'Promise__'(_ F) then
		  value    <- '@Promise__'(!!F)
		  arity    <- [1]
		  maxWidth <- 1
	       end
	       @brace = {New Helper.ozAtom create('|}' self 0 Visual tuple)}
	    [] '@Promise__' then
	       @type  = promise
	       @label = {New Helper.prefix create('promise' '{|' self Visual)}
	       @brace = {New Helper.ozAtom create('|}' self 0 Visual tuple)}
	    [] 'Package__' then
	       @type  = package
	       @label = {New Helper.prefix create('package' '{|' self Visual)}
	       @brace = {New Helper.ozAtom create('|}' self 0 Visual tuple)}
	       arity    <- nil
	       maxWidth <- 0
	       %% Hack Alert
	       {Dictionary.put @items 1
		{New Helper.ozAtom create('...' self 1 Visual tuple)}}
	    else
	       @type = conval
	       if {System.eq Value.1 unit}
	       then
		  %% Attention: This is NOT OzHelper
		  @label = {New Helper.atom create(ValLab self 0 Visual constructor)}
		  @brace = {New Helper.empty create(self)}
	       else
		  %% Colors to be determined
		  @label = {New Helper.label create(ValLab '(' self Visual)}
		  @brace = {New Helper.ozAtom create(')' self 0 Visual tuple)}
	       end
	    end
	    OzRecordCreateObject, adjustWidth({Visual getWidth($)} 1)
	 end
      end
   in
      class VectorCreateObject from OzRecordCreateObject
	 attr
	    auxfeat : Helper.vector %% Aux Separator Class
	 meth createContainer
	    SMLCreateContainer, smlCreateContainer
	 end
      end

      class VectorGrCreateObject from OzRecordGrCreateObject
	 attr
	    auxfeat : Helper.vector %% Aux Separator Class
	 meth createContainer
	    SMLCreateContainer, smlCreateContainer
	 end
	 meth handleMode(RefStr Visual)
	    @mode = {New Helper.marker create('R'#RefStr ' as ' self Visual)}
	 end
      end
   end
   
   local
      fun {IsUnbound V}
	 {Value.isFree V} orelse {Value.isFuture V}
      end

      class SmlSeparators
	 meth smlRemoveSeparators(I)
	    Items = @items
	    Node  = {Dictionary.get Items I}
	 in
	    if I < @width
	    then
	       {Node changeSep(', ' list)}
	       SmlSeparators, smlRemoveSeparators((I + 1))
	    else {Dictionary.put Items I {Node remove($)}}
	    end
	 end
	 meth smlAddSeparators(I Width)
	    if I =< Width
	    then
	       Items = @items
	       Node  = {Dictionary.get Items I}
	    in
	       if {Node isSep($)}
	       then {Node changeSep('::' constructor)}
	       else {Dictionary.put Items I {New Helper.separator create('::' @visual Node)}}
	       end
	       SmlSeparators, smlAddSeparators((I + 1) Width)
	    end
	 end
      end
   in
      class ListCreateObject from PipeTupleCreateObject SmlSeparators
	 meth performInsertion(I Vs Stop)
	    Visual = @visual
	    Depth  = @depth
	 in
	    if I > @width orelse {IsDet Stop}
	    then
	       PipeTupleCreateObject,
	       finalInsert(I {New Helper.bitmap treeCreate(width self I Visual Vs)})
	    elseif {IsUnbound Vs}
	    then PipeTupleCreateObject, finalInsert(I {Visual treeCreate(Vs self I Depth $)})
	    elsecase Vs
	    of V|Vr then
	       Node = {Visual treeCreate(V self I Depth $)}
	    in
	       {Dictionary.put @items I {New Helper.separator create('::' Visual Node)}}
	       ListCreateObject, performInsertion((I + 1) Vr Stop)
	    [] nil  then
	       width <- (I - 1)
	       case @type
	       of pipetuple then
		  type  <- list
		  label <- {New Helper.ozAtom create('[' self 0 Visual list)}
		  brace <- {New Helper.ozAtom create(']' self 0 Visual list)}
		  SmlSeparators, smlRemoveSeparators(1)
	       else skip
	       end
	    else PipeTupleCreateObject, finalInsert(I {Visual treeCreate(Vs self I Depth $)})
	    end
	 end
	 meth removeSeparators(I)
	    SmlSeparators, smlRemoveSeparators(I)
	 end
	 meth addSeparators(I Width)
	    SmlSeparators, smlAddSeparators(I Width)
	 end
      end

      class ListGrCreateObject from PipeTupleGrCreateObject SmlSeparators
	 meth performInsertion(Vs I RI Width Stop)
	    Visual = @visual
	    Depth  = @depth
	 in
	    if RI > Width orelse {IsDet Stop}
	    then
	       PipeTupleGrCreateObject,
	       finalInsert(I {New Helper.bitmap create(width self I Visual)})
	    elsecase Vs
	    of V|Vr then
	       NewI  = (I + 1)
	       NewRI = (RI + 1)
	       Node NewNode
	    in
	       PipeTupleGrCreateObject, finalInsert(NewI NewNode) %% Order is significant (!)
	       Node = {Visual treeCreate(V self I Depth $)}
	       {Dictionary.put @items I {New Helper.separator create('::' Visual Node)}}
	       NewNode = {Visual listCreate(Vr self NewI NewRI (Depth - 1) Width Stop $)}
	    end
	 end
	 meth removeSeparators(I)
	    SmlSeparators, smlRemoveSeparators(I)
	 end
	 meth addSeparators(I Width)
	    SmlSeparators, smlAddSeparators(I Width)
	 end
	 meth handleMode(RefStr Visual)
	    PrintStr = 'R'#RefStr
	 in
	    @mode = {New Helper.marker create(PrintStr ' as ' self Visual)}|
	    {New Helper.ozAtom create('(' self 0 Visual round)}|
	    {New Helper.ozAtom create(')' self 0 Visual round)}
	 end
      end
   end

   local
      class SMLCreateContainer
	 meth smlCreateContainer
	    Visual = @visual
	    Value  = @value
	 in
	    @type     = record
	    @maxWidth = {Width Value}
	    @arity    = {Record.arity Value}
	    @label    = case {Record.label Value}
			of '#' then {New Helper.ozAtom create('{' self 0 Visual record)}
			[] L   then {New Helper.labelRecord create(L '{' self Visual)}
			end
	    @brace    = {New Helper.ozAtom create('}' self 0 Visual record)}
	    OzRecordCreateObject, adjustWidth({Visual getWidth($)} 1)
	 end
      end
   in
      class RecordCreateObject from OzRecordCreateObject SMLCreateContainer
	 attr
	    auxfeat : Helper.record %% Aux Separator Class
	 meth createContainer
	    SMLCreateContainer, smlCreateContainer
	 end
      end
      
      class RecordIndCreateObject from RecordCreateObject
	 attr
	    auxfeat : Helper.recordInd %% Aux Separator Class
      end

      class RecordGrCreateObject from OzRecordGrCreateObject SMLCreateContainer
	 attr
	    auxfeat : Helper.record %% Aux Separator Class
	 meth createContainer
	    SMLCreateContainer, smlCreateContainer
	 end
	 meth handleMode(RefStr Visual)
	    @mode = {New Helper.marker create('R'#RefStr ' as ' self Visual)}
	 end
      end

      class RecordGrIndCreateObject from RecordGrCreateObject
	 attr
	    auxfeat : Helper.recordInd %% Aux Separator Class
      end
   end

   %%
   %% Logic-, Future- and Constraint-Variants
   %%

   class FDIntCreateObject from OzFDIntCreateObject
      meth createContainer
	 Arity  = @arity
	 Visual = @visual
	 Value  = @value
      in
	 depth <- (@depth - 1)
	 Arity     = {FD.reflect.dom Value}
	 @type     = fdint
	 @maxWidth = {Length Arity}
	 @label    = {New Helper.ozAtom create('{' self 0 Visual internal)}
	 @brace    = {New Helper.ozAtom create('}' self 0 Visual internal)}
	 {Visual logVar(self Value false)}
	 OzRecordCreateObject, adjustWidth({Visual getWidth($)} 1)
      end
   end

   class FreeGrCreateObject from OzFreeGrCreateObject
      meth handleMode(RefStr Visual)
	 @mode = {New Helper.marker create('R'#RefStr ' as ' self Visual)}
      end
   end

   class FutureGrCreateObject from OzFutureGrCreateObject
      meth handleMode(RefStr Visual)
	 @mode = {New Helper.marker create('R'#RefStr ' as ' self Visual)}
      end
   end

   class FDIntGrCreateObject from OzFDIntGrCreateObject
      meth createContainer
	 Arity  = @arity
	 Visual = @visual
	 Value  = @value
      in
	 depth <- (@depth - 1)
	 Arity     = {FD.reflect.dom Value}
	 @type     = fdint
	 @maxWidth = {Length Arity}
	 @label    = {New Helper.labelType create(fd '{' self Visual)}
	 @brace    = {New Helper.ozAtom create('}' self 0 Visual internal)}
	 {Visual logVar(self Value false)}
	 OzRecordCreateObject, adjustWidth({Visual getWidth($)} 1)
      end
      meth handleMode(RefStr Visual)
	 @mode = {New Helper.marker create('R'#RefStr ' as ' self Visual)}
      end
   end

   class FSValGrCreateObject from OzFSValGrCreateObject
      meth handleMode(RefStr Visual)
	 @mode = {New Helper.marker create('R'#RefStr ' as ' self Visual)}
      end
   end

   class FSVarGrCreateObject from OzFSVarGrCreateObject
      meth handleMode(RefStr Visual)
	 @mode = {New Helper.marker create('R'#RefStr ' as ' self Visual)}
      end
   end
end
