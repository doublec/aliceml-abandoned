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
   FS
export
   'FS$' : AliceFS
define
   %% Type Variables
   %% To come soon

   %% Interface Functions
   fun {DiffFun M1 M2 M3}
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
   
   %% Create FS Interface
   AliceFS = 'FS'('$fs'        : _
		  'diff'       : DiffFun
		  'intersect'  : IntersectFun
		  'intersectN' : IntersecNFun
		  'union'      : UnionFun
		  'unionN'     : UnionNFun
		  'subset'     : SubsetFun
		  'disjoint'   : DisjointFun
		  'disjointN'  : DisjointNFun
		  'distinct'   : DistinctFun
		  'distinctN'  : DistinctNFun
		  'partition'  : PartitionFun)
end
