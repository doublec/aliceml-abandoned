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
   FS
export
   'FS$' : AliceFS
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
   end
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
   
   %% Interface Functions
   fun {FsFun DsO}
      case DsO
      of 'NONE'        then {FS.var.decl}
      [] 'SOME'(D1#D2) then {FS.var.bounds {AliceDomainToOzDomain D1} {AliceDomainToOzDomain D2}}
      end
   end
   fun {FsVecFun I D1 D2}
      {FS.var.tuple.bounds '#[]' I {AliceDomainToOzDomain D1} {AliceDomainToOzDomain D2}}
   end

   %% Create AliceInt Interface
   local
      fun {MinFun M D}
	 {FS.int.min M D}
	 unit
      end
      fun {MaxFun M D}
	 {FS.int.max M D}
	 unit
      end
      fun {ConvexFun M}
	 {FS.int.convex M}
	 unit
      end
      fun {MatchFun M Dv}
	 {FS.int.match M Dv}
	 unit
      end
      fun {MinNFun M DV}
	 {FS.int.minN M DV}
	 unit
      end
      fun {MaxNFun M DV}
	 {FS.int.maxN M DV}
	 unit
      end
   in
      AliceInt = 'Int'('min'    : MinFun
		       'max'    : MaxFun
		       'convex' : ConvexFun
		       'match'  : MatchFun
		       'minN'   : MinNFun
		       'maxN'   : MaxNFun)
   end

   fun {ComplFun M1 M2}
      {FS.compl M1 M2}
      unit
   end
   fun {ComplInFun M1 M2 M3}
      {FS.complIn M1 M2 M3}
      unit
   end
   fun {InclFun D M}
      {FS.include D M}
      unit
   end
   fun {ExclFun D M}
      {FS.exclude D M}
      unit
   end
   fun {CardFun M D}
      {FS.card M D}
      unit
   end
   fun {CardRangeFun I1 I2 M}
      {FS.cardRange I1 I2 M}
      unit
   end
   
   fun {DifferenceFun M1 M2 M3}
      {FS.diff M1 M2 M3}
      unit
   end
   fun {IntersectFun M1 M2 M3}
      {FS.intersect M1 M2 M3}
      unit
   end
   fun {IntersectNFun MV M}
      {FS.intersectN MV M}
      unit
   end
   fun {UnionFun M1 M2 M3}
      {FS.union M1 M2 M3}
      unit
   end
   fun {UnionNFun MV M}
      {FS.unionN MV M}
      unit
   end
   fun {SubsetFun M1 M2}
      {FS.subset M1 M2}
      unit
   end
   fun {DisjointFun M1 M2}
      {FS.disjoint M1 M2}
      unit
   end
   fun {DisjointNFun MV}
      {FS.disjointN MV}
      unit
   end
   fun {DistinctFun M1 M2}
      {FS.distinct M1 M2}
      unit
   end
   fun {DistinctNFun MV}
      {FS.distinctN MV}
      unit
   end
   fun {PartitionFun MV M}
      {FS.partition MV M}
      unit
   end
   fun {ValueFun S}
      {FS.value.make {AliceDomainToOzDomain S}}
   end
   fun {EmptyValueFun _}
      {FS.value.empty}
   end
   fun {UniversalValueFun _}
      {FS.value.universal}
   end

   %% Create Reified Interface
   local
      fun {IsInFun I M B}
	 {FS.reified.isIn I M B}
	 unit
      end
      fun {AreInFun Is M Ds}
	 {FS.reified.areIn Is M Ds}
	 unit
      end
      fun {InclFun D M B}
	 {FS.reified.include D M B}
	 unit
      end
      fun {EqualFun M1 M2 B}
	 {FS.reified.equal M1 M2 B}
	 unit
      end
      fun {PartitionFun Ms Is M Bs}
	 {FS.reified.partition Ms Is M Bs}
	 unit
      end
   in
      AliceReified = 'Reified'('isIn'      : IsInFun
			       'areIn'     : AreInFun
			       'incl'      : InclFun
			       'equalFun'  : EqualFun
			       'partition' : PartitionFun)
   end
   
   %% Create Reflect Interface
   local
      fun {CardFun X}
	 case {FS.reflect.card X}
	 of L#U then '#[]'('RANGE'(L U))
	 [] I   then '#[]'('SINGLE'(I))
	 end
      end
      fun {LowerBoundFun X}
	 {OzDomainToAliceDomain {FS.reflect.lowerBound X}}
      end
      fun {UnknownFun X}
	 {OzDomainToAliceDomain {FS.reflect.unknown X}}
      end
      fun {UpperBoundFun X}
	 {OzDomainToAliceDomain {FS.reflect.upperBound X}}
      end

   in
      AliceReflect = 'Reflect'('card'             : CardFun
			       'lowerBound'       : LowerBoundFun
			       'unknown'          : UnknownFun
			       'upperBound'       : UpperBoundFun
			       'cardOfLowerBound' : FS.reflect.cardOf.lowerBound
			       'cardOfUnknown'    : FS.reflect.cardOf.unknown
			       'cardOfUpperBound' : FS.reflect.cardOf.upperBound)
   end
   
   %% Create FS Interface
   AliceFS = 'FS'('$fs'            : _
		  'fs'             : FsFun
		  'fsVec'          : FsVecFun
		  'Int$'           : AliceInt
		  'compl'          : ComplFun
		  'complIn'        : ComplInFun
		  'incl'           : InclFun
		  'excl'           : ExclFun
		  'card'           : CardFun
		  'cardRange'      : CardRangeFun
		  'isIn'           : FS.isIn
		  'isEmpty'        : FS.empty
		  'difference'     : DifferenceFun
		  'intersect'      : IntersectFun
		  'intersectN'     : IntersectNFun
		  'union'          : UnionFun
		  'unionN'         : UnionNFun
		  'subset'         : SubsetFun
		  'disjoint'       : DisjointFun
		  'disjointN'      : DisjointNFun
		  'distinct'       : DistinctFun
		  'distinctN'      : DistinctNFun
		  'partition'      : PartitionFun
		  'value'          : ValueFun
		  'emptyValue'     : EmptyValueFun
		  'singletonValue' : FS.value.singl
		  'universalValue' : UniversalValueFun
		  'isValue'        : FS.value.is
		  'Reified$'       : AliceReified
		  'Reflect$'       : AliceReflect)
end
