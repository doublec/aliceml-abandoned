%%%
%%% Authors:
%%%   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
%%%   Andreas Rossberg <rossberg@ps.uni-sb.de>
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
   'UnsafeFD$' : UnsafeFD
define
   %% Global Helper Functions
   proc {UnzipVec V V1 V2}
      I = {Width V}
   in
      V1 = {MakeTuple '#[]' I}
      V2 = {MakeTuple '#[]' I}
      {Record.forAllInd V proc {$ I X#Y}
			     V1.I = X
			     V2.I = Y
			  end}
   end

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
   
   %% Interface Functions
   fun {FDFun DO}
      case DO
      of 'NONE'    then {FD.decl}
      [] 'SOME'(D) then {FD.int {AliceDomainToOzDomain D}}
      end
   end
   fun {FDVecFun I D}
      V = {MakeTuple '#[]' I}
   in
      {FD.dom {AliceDomainToOzDomain D} V}
      V
   end
   fun {RangeFun L H}
      {FD.int L#H}
   end
   fun {RangeVecFun I D}
      V = {MakeTuple '#[]' I}
   in
      {FD.dom D V}
      V
   end
   fun {BinFun _}
      {FD.int 0#1} 
   end
   fun {BinVecFun I}
      V = {MakeTuple '#[]' I}
   in
      {FD.dom 0#1 V}
      V
   end

   %% Conversion Functions
   fun {ToIntFun X}
      {Wait X}
      X
   end
   fun {FromIntFun X}
      X
   end
   fun {ToFutureFun X}
      X
   end
   fun {IsBinFun X}
      {FD.reflect.max X} =< 1
   end
   
   %% Standard Sums
   fun {SumFun XV P Y}
      {FD.sum XV {AlicePropToOzProp P} Y}
      unit
   end
   fun {SumCFun IXV P Y}
      IV XV
   in
      {UnzipVec IXV IV XV}
      {FD.sumC IV XV {AlicePropToOzProp P} Y}
      unit
   end
   fun {SumACFun IXV P Y}
      IV XV
   in
      {UnzipVec IXV IV XV}
      {FD.sumAC IV XV {AlicePropToOzProp P} Y}
      unit
   end
   fun {SumCNFun IXVV P Y}
      IV XVV
   in
      {UnzipVec IXVV IV XVV}
      {FD.sumCN IV XVV {AlicePropToOzProp P} Y}
      unit
   end
   fun {SumACNFun IXVV P Y}
      IV XVV
   in
      {UnzipVec IXVV IV XVV}
      {FD.sumACN IV XVV {AlicePropToOzProp P} Y}
      unit
   end
   fun {SumDFun XV P Y}
      {FD.sumD XV {AlicePropToOzProp P} Y}
      unit
   end
   fun {SumCDFun IXV P Y}
      IV XV
   in
      {UnzipVec IXV IV XV}
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
      {FD.distinct XV}
      unit
   end
   fun {DistinctOffsetFun XIV}
      XV IV
   in
      {UnzipVec XIV XV IV}
      {FD.distinctOffset XV IV}
      unit
   end
   fun {Distinct2Fun XIXIV}
      XIV1 XIV2
      XV1 IV1 XV2 IV2
   in
      {UnzipVec XIXIV XIV1 XIV2}
      {UnzipVec XIV1 XV1 IV1}
      {UnzipVec XIV2 XV2 IV2}
      {FD.distinct2 XV1 IV1 XV2 IV2}
      unit
   end
   fun {AtMostFun X XV I}
      {FD.atMost X XV I}
      unit
   end
   fun {AtLeastFun X XV I}
      {FD.atLeast X XV I}
      unit
   end
   fun {ExactlyFun X XV I}
      {FD.exactly X XV I}
      unit
   end
   fun {ElementFun X IV Y}
      {FD.element X IV Y}
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

   %% Reified Constraints
   fun {ReifiedFdFun D B}
      {FD.reified.int {AliceDomainToOzDomain D} $ B}
   end
   fun {ReifiedFdVecFun I D B}
      V = {MakeTuple '#[]' I}
   in
      {FD.reified.dom {AliceDomainToOzDomain D} V B}
      V
   end
   fun {ReifiedCardFun I1 XV I2 B}
      {FD.reified.card I1 XV I2 B}
      unit
   end
   fun {ReifiedDistanceFun X Y P Z B}
      {FD.reified.distance X Y {AlicePropToOzProp P} Z B}
      unit
   end
   fun {ReifiedSumFun XV P Y B}
      {FD.reified.sum XV {AlicePropToOzProp P} Y B}
      unit
   end
   fun {ReifiedSumCFun IXV P Y B}
      IV XV
   in
      {UnzipVec IXV IV XV}
      {FD.reified.sumC IV XV {AlicePropToOzProp P} Y B}
      unit
   end
   fun {ReifiedSumACFun IXV P Y B}
      IV XV
   in
      {UnzipVec IXV IV XV}
      {FD.reified.sumAC IV XV {AlicePropToOzProp P} Y B}
      unit
   end
   fun {ReifiedSumCNFun IXVV P Y B}
      IV XVV
   in
      {UnzipVec IXVV IV XVV}
      {FD.reified.sumCN IV XVV {AlicePropToOzProp P} Y B}
      unit
   end
   fun {ReifiedSumACNFun IXVV P Y B}
      IV XVV
   in
      {UnzipVec IXVV IV XVV}
      {FD.reified.sumCN IV XVV {AlicePropToOzProp P} Y B}
      unit
   end

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
      fun {ReflectDomainFun X}
	 {OzDomainToAliceDomain {FD.reflect.dom X}}
      end
   end

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
   
   %% Create Interface
   UnsafeFD = 'UnsafeFD'('fd'                  : FDFun
			 'fdVec'               : FDVecFun
			 'range'               : RangeFun
			 'rangeVec'            : RangeVecFun
			 'bin'                 : BinFun
			 'binVec'              : BinVecFun
			 'toInt'               : ToIntFun
			 'future'              : ToFutureFun
			 'fromInt'             : FromIntFun
			 'isBin'               : IsBinFun
			 'sum'                 : SumFun
			 'sumC'                : SumCFun
			 'sumAC'               : SumACFun
			 'sumCN'               : SumCNFun
			 'sumACN'              : SumACNFun
			 'sumD'                : SumDFun
			 'sumCD'               : SumCDFun
			 'plus'                : PlusFun
			 'minus'               : MinusFun
			 'times'               : TimesFun
			 'power'               : PowerFun
			 'divI'                : DivIFun
			 'modI'                : ModIFun
			 'plusD'               : PlusDFun
			 'minusD'              : MinusDFun
			 'timesD'              : TimesDFun
			 'divD'                : DivDFun
			 'modD'                : ModDFun
			 'min'                 : MinFun
			 'max'                 : MaxFun
			 'equal'               : EqualFun
			 'notequal'            : NotEqualFun
			 'distance'            : DistanceFun
			 'less'                : LessFun
			 'lessEq'              : LessEqFun
			 'greater'             : GreaterFun
			 'greaterEq'           : GreaterEqFun
			 'disjoint'            : DisjointFun
			 'disjointC'           : DisjointCFun
			 'tasksOverlap'        : TasksOverlapFun
			 'distinct'            : DistinctFun
			 'distinctOffset'      : DistinctOffsetFun
			 'distinct2'           : Distinct2Fun
			 'atMost'              : AtMostFun
			 'atLeast'             : AtLeastFun
			 'exactly'             : ExactlyFun
			 'element'             : ElementFun
			 'conj'                : ConjFun
			 'disj'                : DisjFun
			 'exor'                : ExorFun
			 'nega'                : NegaFun
			 'impl'                : ImplFun
			 'equi'                : EquiFun
			 'reified_fd'          : ReifiedFdFun
			 'reified_fdVec'       : ReifiedFdVecFun
			 'reified_card'        : ReifiedCardFun
			 'reified_distance'    : ReifiedDistanceFun
			 'reified_sum'         : ReifiedSumFun
			 'reified_sumC'        : ReifiedSumCFun
			 'reified_sumAC'       : ReifiedSumACFun
			 'reified_sumCN'       : ReifiedSumCNFun
			 'reified_sumACN'      : ReifiedSumACNFun
			 'reflect_min'         : FD.reflect.min
			 'reflect_max'         : FD.reflect.max
			 'reflect_mid'         : FD.reflect.mid
			 'reflect_nextLarger'  : FD.reflect.nextLarger
			 'reflect_nextSmaller' : FD.reflect.nextSmaller
			 'reflect_size'        : FD.reflect.size
			 'reflect_dom'         : ReflectDomainFun
			 'reflect_domList'     : FD.reflect.domList
			 'reflect_nbSusps'     : FD.reflect.nbSusps
			 'watch_min'           : FD.watch.min
			 'watch_max'           : FD.watch.max
			 'watch_size'          : FD.watch.size
			 'distribute'          : DistFun)
end
