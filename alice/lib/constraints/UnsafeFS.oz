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
   System(onToplevel)
   FS
   FDCommon('aliceDomainToOzDomain' : AliceDomainToOzDomain
	    'ozDomainToAliceDomain' : OzDomainToAliceDomain
	    'exnWrapper' : ExnWrapper) at 'FDCommon.ozf'
export
   'UnsafeFS$' : UnsafeFS
define
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
      FS.value.empty
   end
   fun {UniversalValueFun _}
      {FS.value.universal}
   end

   %% Integer Functions
   fun {IntMinFun M D}
      {FS.int.min M D}
      unit
   end
   fun {IntMaxFun M D}
      {FS.int.max M D}
      unit
   end
   fun {IntConvexFun M}
      {FS.int.convex M}
      unit
   end
   fun {IntMatchFun M Dv}
      {FS.int.match M Dv}
      unit
   end
   fun {IntMinNFun M DV}
      {FS.int.minN M DV}
      unit
   end
   fun {IntMaxNFun M DV}
      {FS.int.maxN M DV}
      unit
   end

   %% Reified Functions
   fun {ReifiedIsInFun I M B}
      {FS.reified.isIn I M B}
      unit
   end
   fun {ReifiedAreInFun Is M Ds}
      {FS.reified.areIn Is M Ds}
      unit
   end
   fun {ReifiedInclFun D M B}
      {FS.reified.include D M B}
      unit
   end
   fun {ReifiedEqualFun M1 M2 B}
      {FS.reified.equal M1 M2 B}
      unit
   end
   fun {ReifiedPartitionFun Ms Is M Bs}
      {FS.reified.partition Ms Is M Bs}
      unit
   end
   
   %% Reflect Functions
   fun {ReflectCardFun X}
      case {FS.reflect.card X}
      of L#U then '#[]'('RANGE'(L U))
      [] I   then '#[]'('SINGLE'(I))
      end
   end
   fun {ReflectLowerBoundFun X}
      {OzDomainToAliceDomain {FS.reflect.lowerBound X}}
   end
   fun {ReflectUnknownFun X}
      {OzDomainToAliceDomain {FS.reflect.unknown X}}
   end
   fun {ReflectUpperBoundFun X}
      {OzDomainToAliceDomain {FS.reflect.upperBound X}}
   end

   %% Create Interface
   UnsafeFS = {Record.map
	       'UnsafeFS'(
		  'inf'                      : FS.inf
		  'sup'                      : FS.sup
		  'unsafeFS'                 : FsFun
		  'unsafeFSVec'              : FsVecFun
		  'compl'                    : ComplFun
		  'complIn'                  : ComplInFun
		  'incl'                     : InclFun
		  'excl'                     : ExclFun
		  'card'                     : CardFun
		  'cardRange'                : CardRangeFun
		  'isIn'                     : FS.isIn
		  'difference'               : DifferenceFun
		  'intersect'                : IntersectFun
		  'intersectN'               : IntersectNFun
		  'union'                    : UnionFun
		  'unionN'                   : UnionNFun
		  'subset'                   : SubsetFun
		  'disjoint'                 : DisjointFun
		  'disjointN'                : DisjointNFun
		  'distinct'                 : DistinctFun
		  'distinctN'                : DistinctNFun
		  'partition'                : PartitionFun
		  'unsafeValue'              : ValueFun
		  'emptyValue'               : EmptyValueFun
		  'singletonValue'           : FS.value.singl
		  'universalValue'           : UniversalValueFun
		  'isValue'                  : FS.value.is
		  'int_min'                  : IntMinFun
		  'int_max'                  : IntMaxFun
		  'int_convex'               : IntConvexFun
		  'int_match'                : IntMatchFun
		  'int_minN'                 : IntMinNFun
		  'int_maxN'                 : IntMaxNFun
		  'reifeid_isIn'             : ReifiedIsInFun
		  'reifeid_areIn'            : ReifiedAreInFun
		  'reifeid_incl'             : ReifiedInclFun
		  'reifeid_equalFun'         : ReifiedEqualFun
		  'reifeid_partition'        : ReifiedPartitionFun
		  'reflect_card'             : ReflectCardFun
		  'reflect_lowerBound'       : ReflectLowerBoundFun
		  'reflect_unknown'          : ReflectUnknownFun
		  'reflect_upperBound'       : ReflectUpperBoundFun
		  'reflect_cardOfLowerBound' : FS.reflect.cardOf.lowerBound
		  'reflect_cardOfUnknown'    : FS.reflect.cardOf.unknown
		  'reflect_cardOfUpperBound' : FS.reflect.cardOf.upperBound)
	       ExnWrapper}
end
