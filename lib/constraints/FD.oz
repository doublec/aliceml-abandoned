%%%
%%% Authors:
%%%   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Thorsten Brunklaus, 2000
%%%
%%% Last Change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   FD
export
   'FD$' : AliceFD
define
   %% Type Variables
   %% To come soon

   %% Global Helper Functions
   local
      fun {MakeOzValue V}
	 case V
	 of 'SINGLE'(X)  then X
	 [] 'RANGE'(L U) then L#U
	 end
      end
      fun {Cons X Y}
	 {MakeOzValue X}|Y
      end
   in
      fun {AliceDomainToOzDomain D}
	 {Record.foldR D Cons nil}
      end
      local
	 fun {Cons X Y}
	    X|Y
	 end
      in
	 fun {VectorToList V}
	    {Record.foldR V Cons nil}
	 end
      end
      fun {AlicePropToOzProp P}
	 case P
	 of 'LESS'      then '<:'
	 [] 'LESSEQ'    then '=<:'
	 [] 'EQUAL'     then '=:'
	 [] 'NOTEQUAL'  then '\\=:'
	 [] 'GREATER'   then '>:'
	 [] 'GREATEREQ' then '>=:'
	 end
      end
   end
   
   %% Interface Functions
   fun {FDFun D}
      {FD.int {AliceDomainToOzDomain D}}
   end
   fun {FDVectorFun D I}
      V = {MakeTuple '#[]' I}
   in
      {FD.dom {AliceDomainToOzDomain D} V} V
    end
   fun {DeclFun}
      {FD.decl}
   end
   fun {BoolFun}
      {FD.int 0#1} 
   end
   fun {BoolVectorFun I}
      V = {MakeTuple '#[]' I}
   in
      {FD.dom 0#1 V} V
   end

   %% Conversion Functions
   fun {ToBoolFun X}
      case {FD.reflect.dom X}
      of [0#1] then 'SOME'(X)
      [] _     then 'NONE'
      end
   end
   fun {FromBoolFun X}
      X
   end
   fun {ToIntFun X}
      if {IsInt X} then 'SOME'(X) else 'NONE' end
   end
   fun {FromIntFun X}
      X
   end
   
   %% Standard Sums
   fun {SumFun XV P Y}
      {FD.sum XV {AlicePropToOzProp P} Y}
      unit
   end
   fun {SumCFun IV XV P Y}
      {FD.sumC IV XV {AlicePropToOzProp P} Y}
      unit
   end
   fun {SumACFun IV XV P Y}
      {FD.sumAC IV XV {AlicePropToOzProp P} Y}
      unit
   end
   fun {SumCNFun IV XVV P Y}
      {FD.sumCN IV XVV {AlicePropToOzProp P} Y}
      unit
   end
   fun {SumACNFun IV XVV P Y}
      {FD.sumACN IV XVV {AlicePropToOzProp P} Y}
      unit
   end
   fun {SumDFun XV P Y}
      {FD.sumD XV {AlicePropToOzProp P} Y}
      unit
   end
   fun {SumCDFun IV XV P Y}
      {FD.sumCD IV XV {AlicePropToOzProp P} Y}
      unit
   end
   
   %% Propagator Functions with Interval Propagation
   fun {PlusFun X Y Z}
      {FD.plus X Y Z}
      unit
   end
   fun {MinusFun X Y Z}
      {FD.minus X Y Z}
      unit
   end
   fun {TimesFun X Y Z}
      {FD.times X Y Z}
      unit
   end
   fun {PowerFun X I Z}
      {FD.power X I Z}
      unit
   end
   fun {DivIFun X I Z}
      {FD.divI X I Z}
      unit
   end
   fun {ModIFun X I Z}
      {FD.modI X I Z}
      unit
   end

   %% Propagator Functions with Domain Propagation
   fun {PlusDFun X Y Z}
      {FD.plusD X Y Z}
      unit
   end
   fun {MinusDFun X Y Z}
      {FD.minusD X Y Z}
      unit
   end
   fun {TimesDFun X Y Z}
      {FD.timesD X Y Z}
      unit
   end
   fun {DivDFun X I Z}
      {FD.divD X I Z}
      unit
   end
   fun {ModDFun X I Z}
      {FD.modD X I Z}
      unit
   end
   
   %% Other Propagators
   fun {MinFun X Y Z}
      {FD.min X Y Z}
      unit
   end
   fun {MaxFun X Y Z}
      {FD.max X Y Z}
      unit
   end
   fun {EqualFun X Y}
      X =: Y
      unit
   end
   fun {NotEqualFun X Y}
      X \=: Y
      unit
   end
   fun {DistanceFun X Y P Z}
      {FD.distance X Y {AlicePropToOzProp P} Z}
      unit
   end
   fun {LessFun X Y}
      {FD.less X Y}
      unit
   end
   fun {LessEqFun X Y}
      {FD.lesseq X Y}
      unit
   end
   fun {GreaterFun X Y}
      {FD.greater X Y}
      unit
   end
   fun {GreaterEqFun X Y}
      {FD.greatereq X Y}
      unit
   end
   fun {DisjointFun X I1 Y I2}
      {FD.disjoint X I1 Y I2}
      unit
   end
   fun {DisjointCFun X I1 Y I2 Z}
      {FD.disjointC X I1 Y I2 Z}
      unit
   end
   fun {TasksOverlapFun X I1 Y I2 Z}
      {FD.tasksOverlap X I1 Y I2 Z}
      unit
   end

   %% Non-linear Propagators
   fun {DistinctFun XV}
      {FD.distinct {VectorToList XV}}
      unit
   end
   fun {DistinctOffsetFun XV IV}
      {FD.distinctOffset {VectorToList XV} {VectorToList IV}}
      unit
   end
   fun {Distinct2Fun XV1 IV1 XV2 IV2}
      {FD.distinct2 {VectorToList XV1} {VectorToList IV1} {VectorToList XV2} {VectorToList IV2}}
      unit
   end
   fun {AtMostFun X XV I}
      {FD.atMost X {VectorToList XV} I}
      unit
   end
   fun {AtLeastFun X XV I}
      {FD.atLeast X {VectorToList XV} I}
      unit
   end
   fun {ExactlyFun X XV I}
      {FD.exactly X {VectorToList XV} I}
      unit
   end
   fun {ElementFun X IV Y}
      {FD.element X {VectorToList IV} Y}
      unit
   end

   %% 0/1 Propagators
   fun {ConjFun X Y Z}
      {FD.conj X Y Z}
      unit
   end
   fun {DisjFun X Y Z}
      {FD.disj X Y Z}
      unit
   end
   fun {ExorFun X Y Z}
      {FD.exor X Y Z}
      unit
   end
   fun {NegaFun X Y}
      {FD.nega X Y}
      unit
   end
   fun {ImplFun X Y Z}
      {FD.impl X Y Z}
      unit
   end
   fun {EquiFun X Y Z}
      {FD.equi X Y Z}
      unit
   end

   local
      fun {IntFun D B}
	 {FD.reified.int {AliceDomainToOzDomain D} $ B}
      end
      fun {DomFun D I B}
	 V = {MakeTuple '#[]' I}
      in
	 {FD.reified.dom {AliceDomainToOzDomain D} V B}
	 V
      end
      fun {CardFun I1 XV I2 B}
	 {FD.reified.card I1 XV I2 B}
	 unit
      end
      fun {DistanceFun X Y P Z B}
	 {FD.reified.distance X Y {AlicePropToOzProp P} Z B}
	 unit
      end
      fun {SumFun XV P Y B}
	 {FD.reified.sum XV {AlicePropToOzProp P} Y B}
	 unit
      end
      fun {SumCFun IV XV P Y B}
	 {FD.reified.sumC IV XV {AlicePropToOzProp P} Y B}
	 unit
      end
      fun {SumACFun IV XV P Y B}
	 {FD.reified.sumAC IV XV {AlicePropToOzProp P} Y B}
	 unit
      end
      fun {SumCNFun IV XVV P Y B}
	 {FD.reified.sumCN IV XVV {AlicePropToOzProp P} Y B}
	 unit
      end
      fun {SumACNFun IV XVV P Y B}
	 {FD.reified.sumCN IV XVV {AlicePropToOzProp P} Y B}
	 unit
      end
   in
      %% Create Reified Interface
      AliceReified = 'Reified'('int'      : IntFun
			       'dom'      : DomFun
			       'card'     : CardFun
			       'distance' : DistanceFun
			       'sum'      : SumFun
			       'sumC'     : SumCFun
			       'sumAC'    : SumACFun
			       'sumCN'    : SumCNFun
			       'sumACN'   : SumACNFun)
   end

   local
      local
	 fun {OzDomainToAliceDomain Ds}
	    V = {MakeTuple '#[]' {Length Ds}}
	 in
	    {List.forAllInd Ds proc {$ X I}
				  V.I = case X
					of L#U then 'RANGE'(L U)
					else 'SINGLE'(X)
					end
			       end}
	    V
	 end
      in
	 fun {DomainFun X}
	    {OzDomainToAliceDomain {FD.reflect.dom X}}
	 end
      end
   in
      %% Create Reflection Interface
      AliceReflect = 'Reflect'('min'         : FD.reflect.min
			       'max'         : FD.reflect.max
			       'mid'         : FD.reflect.mid
			       'nextLarger'  : FD.reflect.nextLarger
			       'nextSmaller' : FD.reflect.nextSmaller
			       'size'        : FD.reflect.size
			       'dom'         : DomainFun
			       'domList'     : FD.reflect.domList
			       'nbSusps'     : FD.reflect.nbSusps)
   end

   %% Create Watch Interface
   AliceWatch = 'Watch'('min'  : FD.watch.min
			'max'  : FD.watch.max
			'size' : FD.watch.size)

   %% Distribution Interface
   local
      fun {AliceDistToOzDist M}
	 case M
	 of 'FIRSTFAIL' then ff
	 [] 'NAIVE'     then naive
	 end
      end
   in
      fun {DistFun M X}
	 {FD.distribute {AliceDistToOzDist M} X}
	 unit
      end
   end
   
   %% Create FD Interface
   AliceFD = 'FD'('$fd'            : _
		  '$bool'          : _
		  '$domain'        : _
		  '$propagator'    : _
		  '$dist_mode'     : _
		  'SINGLE'         : fun {$ N} 'SINGLE'(N) end
		  'RANGE'          : fun {$ L U} 'RANGE'(L U) end
		  '\'SINGLE'       : _
		  '\'RANGE'        : _
		  'LESS'           : 'LESS'
		  'LESSEQ'         : 'LESSEQ'
		  'EQUAL'          : 'EQUAL'
		  'NOTEQUAL'       : 'NOTEQUAL'
		  'GREATER'        : 'GREATER'
		  'GREATEREQ'      : 'GREATEREQ'
		  '\'LESS'         : _
		  '\'LESSEQ'       : _
		  '\'EQUAL'        : _
		  '\'NOTEQUAL'     : _
		  '\'GREATER'      : _
		  '\'GREATEREQ'    : _
		  'fd'             : FDFun
		  'fdvector'       : FDVectorFun
		  'decl'           : DeclFun
		  'bool'           : BoolFun
		  'boolvector'     : BoolVectorFun
		  'toBool'         : ToBoolFun
		  'fromBool'       : FromBoolFun
		  'toInt'          : ToIntFun
		  'fromInt'        : FromIntFun
		  'sum'            : SumFun
		  'sumC'           : SumCFun
		  'sumAC'          : SumACFun
		  'sumCN'          : SumCNFun
		  'sumACN'         : SumACNFun
		  'sumD'           : SumDFun
		  'sumCD'          : SumCDFun
		  'plus'           : PlusFun
		  'minus'          : MinusFun
		  'times'          : TimesFun
		  'power'          : PowerFun
		  'divI'           : DivIFun
		  'modI'           : ModIFun
		  'plusD'          : PlusDFun
		  'minusD'         : MinusDFun
		  'timesD'         : TimesDFun
		  'divD'           : DivDFun
		  'modD'           : ModDFun
		  'min'            : MinFun
		  'max'            : MaxFun
		  'equal'          : EqualFun
		  'notequal'       : NotEqualFun
		  'distance'       : DistanceFun
		  'less'           : LessFun
		  'lessEq'         : LessEqFun
		  'greater'        : GreaterFun
		  'greaterEq'      : GreaterEqFun
		  'disjoint'       : DisjointFun
		  'disjointC'      : DisjointCFun
		  'tasksOverlap'   : TasksOverlapFun
		  'distinct'       : DistinctFun
		  'distinctOffset' : DistinctOffsetFun
		  'distinct2'      : Distinct2Fun
		  'atMost'         : AtMostFun
		  'atLeast'        : AtLeastFun
		  'exactly'        : ExactlyFun
		  'element'        : ElementFun
		  'conj'           : ConjFun
		  'disj'           : DisjFun
		  'exor'           : ExorFun
		  'nega'           : NegaFun
		  'impl'           : ImplFun
		  'equi'           : EquiFun
		  'Reified$'       : AliceReified
		  'Reflect$'       : AliceReflect
		  'Watch$'         : AliceWatch
		  'NAIVE'          : 'NAIVE'
		  'FIRSTFAIL'      : 'FIRSTFAIL'
		  'SPLIT'          : 'SPLIT'
		  '\'NAIVE'        : _
		  '\'FIRSTFAIL'    : _
		  '\'SPLIT'        : _
		  'distribute'     : DistFun)
end
