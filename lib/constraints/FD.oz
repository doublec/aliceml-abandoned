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
   FDType
   BoolType
   DomainType
   PropagatorType
   DistModeType

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
      fun {ListToVector Ls}
	 V = {MakeTuple '#[]' {Length Ls}}
      in
	 {List.forAllInd Ls proc {$ X I}
			       V.I = X
			    end}
      end
      fun {AlicePropToOzProp P}
	 case P
	 of 'LESS'      then '<:'
	 [] 'LESSEQ'    then '=<:'
	 [] 'EQUAL'     then '=:'
	 [] 'NOTEQUAL'  then '\=:'
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
      V = {MakeList I}
   in
      {FD.dom {AliceDomainToOzDomain D} V}
      {List.toTuple '#[]' V}
   end
   fun {DeclFun}
      {FD.decl}
   end
   fun {BoolFun}
      {FD.int 0#1} 
   end
   fun {BoolVectorFun I}
      V = {MakeList I}
   in
      {FD.dom 0#1 V}
      {List.toTuple '#[]' V}
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
   
   %% Standard Sums
   fun {SumFun XV P Y}
      {FD.sum {VectorToList XV} {AlicePropToOzProp P} Y}
      unit
   end
   fun {SumCFun IV XV P Y}
      {FD.sumC {VectorToList IV} {VectorToList XV} {AlicePropToOzProp P} Y}
      unit
   end
   fun {SumACFun IV XV P Y}
      {FD.sumAC {VectorToList IV} {VectorToList XV} {AlicePropToOzProp P} Y}
      unit
   end
   fun {SumCNFun IV XVV P Y}
      {FD.sumCN {VectorToList IV} {Map {VectorToList XV} VectorToList} {AlicePropToOzProp P} Y}
      unit
   end
   fun {SumACNFun IV XVV P Y}
      {FD.sumACN {VectorToList IV} {Map {VectorToList XV} VectorToList} {AlicePropToOzProp P} Y}
      unit
   end
   fun {SumDFun XV P Y}
      {FD.sumD {VectorToList XV} {AlicePropToOzProp P} Y}
      unit
   end
   fun {SumCDFun IV XV P Y}
      {FD.sumCD {VectorToList IV} {VectorToList XV} {AlicePropToOzProp P} Y}
      unit
   end
   
   %% Propagator Functions
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
   fun {equiFun X Y Z}
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
      fun {SumFun XV P Y B}
	 {FD.reified.sum XV {AlicePropToOzProp P} Y B}
	 unit
      end
      fun {SumCFun IV XV P Y B}
	 {FD.reified.sumC {VectorToList IV} XV {AlicePropToOzProp P} Y B}
	 unit
      end
   in
      %% Create Reified Interface
      AliceReified = 'Reified'('int'  : IntFun
			       'dom'  : DomFun
			       'sum'  : SumFun
			       'sumC' : SumCFun)
   end

   local
      local
	 local
	    fun {MakeInt X}
	       'SINGLE'(X)
	    end
	    fun {MakeRange L U}
	       'RANGE'(L U)
	    end
	    fun {MakeVector L}
	       {MakeTuple '#[]' L}
	    end
	 in
	    fun {OzDomainToAliceDomain Ds}
	       V = {MakeVector {Length Ds}}
	    in
	       {List.forAllInd Ds proc {$ X I}
				     V.I = case X
					   of L#U then {MakeRange L U}
					   else {MakeInt X}
					   end
				  end}
	       V
	    end
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
      end
   end
   
   %% Create FD Interface
   AliceFD = 'FD'('$fd'         : FDType
		  '$bool'       : BoolType
		  '$domain'     : DomainType
		  '$propagator' : PropagatorType
		  '$dist_mode'  : DistModeType
		  'fd'          : FDFun
		  'fdvector'    : FDVectorFun
		  'decl'        : DeclFun
		  'bool'        : BoolFun
		  'boolvector'  : BoolVectorFun
		  'toBool'      : ToBoolFun
		  'fromBool '   : FromBoolFun
		  'sum'         : SumFun
		  'sumC'        : SumCFun
		  'sumAC'       : SumACFun
		  'sumCN'       : SumCNFun
		  'sumACN'      : SumACNFun
		  'sumD'        : SumDFun
		  'sumCD'       : SumCDFun
		  'plus'        : PlusFun
		  'minus'       : MinusFun
		  'times'       : TimesFun
		  'power'       : PowerFun
		  'divI'        : DivIFun
		  'modI'        : ModIFun
		  'min'         : MinFun
		  'max'         : MaxFun
		  'equal'       : EqualFun
		  'notequal'    : NotEqualFun
		  'less'        : LessFun
		  'lessEq'      : LessEqFun
		  'greater'     : GreaterFun
		  'greaterEq'   : GreaterEqFun
		  'conj'        : ConjFun
		  'disj'        : DisjFun
		  'exor'        : ExorFun
		  'nega'        : NegaFun
		  'impl'        : ImplFun
		  'equi'        : EquiFun
		  'Reified'     : AliceReified
		  'Reflect'     : AliceReflect
		  'Watch'       : AliceWatch
		  'distribute'  : DistFun)
end
