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
   Inspector('nodes' : TreeNodes)
export
   Nodes
define
   %% Import all needed Helper
   local
      Helper = TreeNodes.'helper'
   in
      TkQuoteStr       = Helper.tkQuoteStr
      OzAtomNode       = Helper.atom
      OzLabelNode      = Helper.label
      OzSeparatorNode  = Helper.separator
      OzFeatureNode    = Helper.feature
      OzFeatureIndNode = Helper.featureInd
      EmptyNode        = Helper.empty
      BitmapNode       = Helper.bitmap
   end

   %% SML Specific Quoting
   local
      fun {IsCtrlRange V}
	 (V >= 0 andthen V =< 6) orelse (V >= 16 andthen V =< 31) 
      end
      fun {Stretch N V}
	 if N == 0 then V else {Stretch (N - 1) 48|V} end
      end
      fun {CreateString V}
	 Vs = {Int.toString V}
      in
	 {Stretch (3 - {Length Vs}) Vs}
      end
      fun {SMLConvert Vs}
	 case Vs
	 of V|Vr then
	    case {Char.type V}
	    of space then
	       case V
	       of &\n then &\\|&n|{SMLConvert Vr}
	       [] &\f then &\\|&f|{SMLConvert Vr}
	       [] &\r then &\\|&r|{SMLConvert Vr}
	       [] &\t then &\\|&t|{SMLConvert Vr}
	       [] &\v then &\\|&v|{SMLConvert Vr}
	       else V|{SMLConvert Vr}
	       end
	    [] punct then
	       case V
	       of 34 then &\\|34|{SMLConvert Vr} %% "
	       [] 92 then 92|92|{SMLConvert Vr} %% \
	       else V|{SMLConvert Vr}
	       end
	    [] other then
	       case V
	       of &\a then &\\|&a|{SMLConvert Vr}
	       [] &\b then &\\|&b|{SMLConvert Vr}
	       elseif {IsCtrlRange V}
	       then &\\|94|(V + 64)|{SMLConvert Vr}
	       elseif (V >= 127)
	       then &\\|{SMLConvert {Append {CreateString V} Vr}}
	       else V|{SMLConvert Vr}
	       end
	    else V|{SMLConvert Vr}
	    end
	 [] nil then nil
	 end
      end
   in
      proc {ConvertAtom Vs PrintStr LenStr}
	 LenStr   = {SMLConvert Vs}
	 PrintStr = {TkQuoteStr LenStr}
      end
      proc {ConvertByteString V PrintStr LenStr}
	 LenStr   = {VirtualString.toString "\""#{SMLConvert {ByteString.toString V}}#"\""}
	 PrintStr = {TkQuoteStr LenStr}
      end
   end

   class AtomNode from OzAtomNode
      meth create(Value Parent Index Visual Type)
	 @visual = Visual
	 @tag    = {Visual newTag($)}
	 @xDim   = {VirtualString.length {ConvertAtom {Atom.toString Value} @string}}
	 @index  = Index
	 @type   = Type
	 @parent = Parent
      end
   end

   class TypePrefixNode from OzLabelNode
      meth layoutX($)
	 XDim   = @xDim
	 Value  = @value
	 String = if {IsAtom Value}     then {Atom.toString Value}
		  elseif {IsName Value} then
		     case {System.printName Value}
		     of '' then {Atom.toString 'unknown'}
		     [] PN then {VirtualString.toString PN}
		     end
		  else Value
		  end
      in
	 if {IsFree XDim}
	 then XDim = ({VirtualString.length {ConvertAtom String @string}} + 2)
	 end
	 XDim
      end
      meth drawX(X Y $)
	 Visual  = @visual
	 XDim    = @xDim
	 StringX = (XDim - 2)
      in
	 if @dirty
	 then
	    dirty <- false
	    {Visual printXY(X Y @string @tag typeprefix)}
	    {Visual printXY((X + StringX) Y @limStr @secTag tuple)}
	 else {Visual doublePlace(X Y StringX @tag @secTag)}
	 end
	 (X + XDim)
      end
   end
   
   class LabelNode from OzLabelNode
      meth layoutX($)
	 XDim   = @xDim
	 Value  = @value
	 String = if {IsAtom Value}     then {Atom.toString Value}
		  elseif {IsName Value} then
		     case {System.printName Value}
		     of '' then {Atom.toString 'unknown'}
		     [] PN then {VirtualString.toString PN}
		     end
		  else Value
		  end
      in
	 if {IsFree XDim}
	 then XDim = ({VirtualString.length {ConvertAtom String @string}} + 1)
	 end
	 XDim
      end
      meth drawX(X Y $)
	 Visual  = @visual
	 XDim    = @xDim
	 StringX = (XDim - 1)
      in
	 if @dirty
	 then
	    dirty <- false
	    {Visual printXY(X Y @string @tag constructor)}
	    {Visual printXY((X + StringX) Y @limStr @secTag tuple)}
	 else {Visual doublePlace(X Y StringX @tag @secTag)}
	 end
	 (X + XDim)
      end
   end

   class LabelRecordNode from LabelNode
      meth drawX(X Y $)
	 Visual  = @visual
	 XDim    = @xDim
	 StringX = (XDim - 1)
      in
	 if @dirty
	 then
	    dirty <- false
	    {Visual printXY(X Y @string @tag constructor)}
	    {Visual printXY((X + StringX) Y @limStr @secTag record)}
	 else {Visual doublePlace(X Y StringX @tag @secTag)}
	 end
	 (X + XDim)
      end
   end

   class LabelTypeNode from LabelNode
      meth drawX(X Y $)
	 Visual  = @visual
	 XDim    = @xDim
	 StringX = (XDim - 1)
      in
	 if @dirty
	 then
	    dirty <- false
	    {Visual printXY(X Y @string @tag typeindicator)}
	    {Visual printXY((X + StringX) Y @limStr @secTag tuple)}
	 else {Visual doublePlace(X Y StringX @tag @secTag)}
	 end
	 (X + XDim)
      end
   end
   
   class MarkerNode from OzLabelNode
      meth layoutX($)
	 XDim = @xDim
      in
	 if {IsFree XDim}
	 then XDim = ({VirtualString.length
		       {ConvertAtom {VirtualString.toString @value} @string}} + 4)
	 end
	 XDim
      end
      meth drawX(X Y $)
	 Visual  = @visual
	 XDim    = @xDim
	 StringX = (XDim - 4)
      in
	 if @dirty
	 then
	    dirty <- false
	    {Visual printXY(X Y @string @tag variable)}
	    {Visual printXY((X + StringX) Y @limStr @secTag misc)}
	 else {Visual doublePlace(X Y StringX @tag @secTag)}
	 end
	 (X + XDim)
      end
   end
   
   class SeparatorNode from OzSeparatorNode
      attr
	 printType : constructor %% PrintType (constructor or list)
      meth layout
	 Node = @node
      in
	 case {Node layoutY($)}
	 of XDim|YDim then
	    LXDim = ({Node getLastXDim($)} + {VirtualString.length @string})
	 in
	    xDim     <- {Max XDim LXDim}
	    yDim     <- YDim
	    lastXDim <- LXDim
	 end
      end
      meth layoutX($)
	 SeparatorNode, layout @xDim
      end
      meth layoutY($)
	 SeparatorNode, layout @xDim|@yDim
      end
      meth draw(X Y)
	 Visual = @visual
	 Node   = @node
	 NewY   = ({Node drawY(X Y $)} - 1)
	 String = @string
	 NewX   = (X + @lastXDim - {VirtualString.length String})
      in
	 if @dirty
	 then dirty <- false {Visual printXY(NewX NewY String @tag @printType)}
	 else {Visual place(NewX NewY @tag)}
	 end
      end
      meth drawX(X Y $)
	 SeparatorNode, draw(X Y) (X + @xDim)
      end
      meth drawY(X Y $)
	 SeparatorNode, draw(X Y) (Y + @yDim)
      end
      meth searchNode(XA YA X Y $)
	 Node   = @node
	 XSep   = (XA + @lastXDim)
	 SepDim = {VirtualString.length @string} 
      in
	 if ((SepDim == 2 andthen (X == (XSep - 1) orelse X == (XSep - 2)))
	     orelse (SepDim == 1 andthen X == (XSep - 1)))
	    andthen Y == (YA + @yDim - 1)
	 then self
	 elsecase {Node getXYDim($)}
	 of XDim|YDim then
	    YM = (YA + YDim)
	 in
	    if Y >= YA andthen Y < YM andthen X >= XA andthen X < (XA + XDim)
	    then {Node searchNode(XA YA X Y $)}
	    else nil
	    end
	 end
      end
      meth changeSep(SepVal PrintType)
	 if @dirty then skip else dirty <- true {@visual delete(@tag)} end
	 string <- SepVal
	 printType <- PrintType
      end
   end

   class TupleNode from OzSeparatorNode
      meth create(FeaVal Visual Node)
	 @string = if {{Node getParent($)} isLast(Node $)} then '' else ', ' end
	 @visual = Visual
	 @tag    = {Visual newTag($)}
	 @node   = Node
      end
      meth layout
	 Node = @node
      in
	 case {Node layoutY($)}
	 of XDim|YDim then
	    DeltaX = {VirtualString.length @string}
	    LXDim  = ({Node getLastXDim($)} + DeltaX)
	 in
	    xDim     <- {Max XDim LXDim}
	    yDim     <- YDim
	    lastXDim <- LXDim
	 end
      end
      meth layoutX($)
	 TupleNode, layout @xDim
      end
      meth layoutY($)
	 TupleNode, layout @xDim|@yDim
      end
      meth draw(X Y)
	 Visual = @visual
	 Node   = @node
	 NewY   = ({Node drawY(X Y $)} - 1)
	 NewX   = (X + {Node getLastXDim($)})
	 String = @string
      in
	 case String
	 of '' then skip
	 elseif @dirty
	 then dirty <- false {Visual printXY(NewX NewY String @tag tuple)}
	 else {Visual place(NewX NewY @tag)}
	 end
      end
      meth drawX(X Y $)
	 TupleNode, draw(X Y) (X + @xDim)
      end
      meth drawY(X Y $)
	 TupleNode, draw(X Y) (Y + @yDim)
      end
      meth undraw
	 if @dirty
	 then skip
	 else
	    dirty <- true
	    case @string of '' then skip else {@visual delete(@tag)} end
	 end
	 {@node undraw}
      end
      meth searchNode(XA YA X Y $)
	 Node = @node
      in
	 if {VirtualString.length @string} == 1 andthen
	    X == (XA + @lastXDim - 1) andthen Y == (YA + @yDim - 1)
	 then self
	 elsecase {Node getXYDim($)}
	 of XDim|YDim then
	    YM = (YA + YDim)
	 in
	    if Y >= YA andthen Y < YM andthen X >= XA andthen X < (XA + XDim)
	    then {Node searchNode(XA YA X Y $)}
	    else nil
	    end
	 end
      end
      meth remove($)
	 if @dirty orelse @string == '' then skip else {@visual delete(@tag)} end
	 @node
      end
   end

   class VectorNode from TupleNode
      meth draw(X Y)
	 Visual = @visual
	 Node   = @node
	 NewY   = ({Node drawY(X Y $)} - 1)
	 NewX   = (X + {Node getLastXDim($)})
	 String = @string
      in
	 case String
	 of '' then skip
	 elseif @dirty
	 then dirty <- false {Visual printXY(NewX NewY String @tag vector)}
	 else {Visual place(NewX NewY @tag)}
	 end
      end
      meth drawX(X Y $)
	 VectorNode, draw(X Y) (X + @xDim)
      end
      meth drawY(X Y $)
	 VectorNode, draw(X Y) (Y + @yDim)
      end
   end
   
   local
      class FeatureNode from OzFeatureNode
	 meth create(FeaVal Visual Node)
	    String = @string
	 in
	    @visual = Visual
	    @tag    = {Visual newTag($)}
	    @node   = Node
	    @secTag = {Visual newTag($)}
	    if {IsAtom FeaVal}
	    then
	       @sDim = {VirtualString.length {ConvertAtom {Atom.toString FeaVal} String}}
	    else
	       String = if {IsName FeaVal}
			then {System.printName FeaVal}
			else FeaVal
			end
	       @sDim  = {VirtualString.length String}
	    end
	 end
	 meth layout
	    FeaX = (@sDim + 3)
	    Node = @node
	 in
	    case {Node layoutY($)}
	    of XDim|YDim then
	       xDim     <- (FeaX + XDim)
	       yDim     <- YDim
	       lastXDim <- (FeaX + {Node getLastXDim($)}) 
	    end
	 end
	 meth layoutX($)
	    FeatureNode, layout @xDim
	 end
	 meth layoutY($)
	    FeatureNode, layout @xDim|@yDim
	 end
	 meth draw(X Y)
	    Visual = @visual
	    SDim   = @sDim
	 in
	    if @dirty
	    then
	       dirty <- false
	       {Visual printXY(X Y @string @tag reclabel)}
	       {Visual printXY((X + SDim + 1) Y '=' @secTag record)}
	    else {Visual doublePlace(X Y (SDim + 1) @tag @secTag)}
	    end
	    {@node draw((X + SDim + 3) Y)}
	 end
	 meth drawX(X Y $)
	    FeatureNode, draw(X Y) (X + @xDim)
	 end
	 meth drawY(X Y $)
	    FeatureNode, draw(X Y) (Y + @yDim)
	 end
	 meth getParent($)
	    {@node getParent($)}
	 end
	 meth searchNode(XA YA X Y $)
	    {@node searchNode((XA + @sDim + 3) YA X Y $)}
	 end
      end
      class FeatureIndNode from OzFeatureIndNode
	 meth create(FeaVal Visual Node)
	    String = @string
	 in
	    @visual = Visual
	    @tag    = {Visual newTag($)}
	    @node   = Node
	    @secTag = {Visual newTag($)}
	    if {IsAtom FeaVal}
	    then
	       @sDim = {VirtualString.length {ConvertAtom {Atom.toString FeaVal} String}}
	    else
	       String = if {IsName FeaVal}
			then {System.printName FeaVal}
			else FeaVal
			end
	       @sDim  = {VirtualString.length String}
	    end
	 end
	 meth layout
	    FeaX = (@sDim + 3)
	    Node = @node
	 in
	    case {Node layoutY($)}
	    of XDim|YDim then
	       if {{Node getParent($)} getHorzMode($)}
	       then
		  xDim     <- (FeaX + XDim)
		  yDim     <- YDim
		  lastXDim <- (FeaX + {Node getLastXDim($)}) 
	       else
		  RealXDim = {Max (FeaX - 3) XDim}
	       in
		  xDim     <- (3 + RealXDim)
		  yDim     <- (YDim + 1)
		  lastXDim <- (3 + {Node getLastXDim($)})
	       end
	    end
	 end
	 meth layoutX($)
	    FeatureIndNode, layout @xDim
	 end
	 meth layoutY($)
	    FeatureIndNode, layout @xDim|@yDim
	 end
	 meth draw(X Y)
	    Visual = @visual
	    SDim   = @sDim
	    Node   = @node
	 in
	    if @dirty
	    then
	       dirty <- false
	       {Visual printXY(X Y @string @tag reclabel)}
	       {Visual printXY((X + SDim + 1) Y '=' @secTag record)}
	    else {Visual doublePlace(X Y (SDim + 1) @tag @secTag)}
	    end
	    if {{Node getParent($)} getHorzMode($)}
	    then {Node draw((X + SDim + 3) Y)}
	    else {Node draw((X + 3) (Y + 1))}
	    end
	 end
	 meth drawX(X Y $)
	    FeatureIndNode, draw(X Y) (X + @xDim)
	 end
	 meth drawY(X Y $)
	    FeatureIndNode, draw(X Y) (Y + @yDim)
	 end
	 meth getParent($)
	    {@node getParent($)}
	 end
      end
   in
      class RecordNode from TupleNode
	 meth create(FeaVal Visual Node)
	    NewNode = {New FeatureNode create(FeaVal Visual Node)}
	 in
	    @string = if {{Node getParent($)} isLast(Node $)} then '' else ', ' end
	    @visual = Visual
	    @tag    = {Visual newTag($)}
	    @node   = NewNode
	 end
	 meth change(Node)
	    {@node change(Node)}
	 end
	 meth getInnerNode($)
	    {@node getInnerNode($)}
	 end
	 meth draw(X Y)
	    Visual = @visual
	    Node   = @node
	    NewY   = ({Node drawY(X Y $)} - 1)
	    NewX   = (X + {Node getLastXDim($)})
	    String = @string
	 in
	    case String
	    of '' then skip
	    elseif @dirty
	    then dirty <- false {Visual printXY(NewX NewY String @tag record)}
	    else {Visual place(NewX NewY @tag)}
	    end
	 end
	 meth drawX(X Y $)
	    RecordNode, draw(X Y) (X + @xDim)
	 end
	 meth drawY(X Y $)
	    RecordNode, draw(X Y) (Y + @yDim)
	 end
      end

      class RecordIndNode from RecordNode
	 meth create(FeaVal Visual Node)
	    NewNode = {New FeatureIndNode create(FeaVal Visual Node)}
	 in
	    @string = if {{Node getParent($)} isLast(Node $)} then '' else ', ' end
	    @visual = Visual
	    @tag    = {Visual newTag($)}
	    @node   = NewNode
	 end
      end
   end

   %% Create The Export Record
   Nodes = 'nodes'(convert     : ConvertAtom
		   convertBS   : ConvertByteString
		   atom        : AtomNode
		   ozAtom      : OzAtomNode
		   prefix      : TypePrefixNode
		   label       : LabelNode
		   labelType   : LabelTypeNode
		   labelRecord : LabelRecordNode
		   marker      : MarkerNode
		   separator   : SeparatorNode
		   empty       : EmptyNode
		   bitmap      : BitmapNode
		   tuple       : TupleNode
		   vector      : VectorNode
		   record      : RecordNode
		   recordInd   : RecordIndNode)
end
