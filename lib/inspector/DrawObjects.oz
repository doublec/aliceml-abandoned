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
   Inspector('nodes' : TreeNodes)
   HelperComponent('nodes' : Helper) at 'Helper'
export
   listGrMDrawObject : ListGrMDrawObject
   listGrSDrawObject : ListGrSDrawObject
define
   %% Import all needed DrawObjects
   local
      DrawObjects = TreeNodes.'draw'
   in
      PipeTupleGrMDrawObject = DrawObjects.pipeTupleGrMDrawObject
      PipeTupleGrSDrawObject = DrawObjects.pipeTupleGrSDrawObject
   end

   %%
   %% Container Objects
   %%

   class ListGrMDrawObject from PipeTupleGrMDrawObject end

   class ListGrSDrawObject from PipeTupleGrSDrawObject end
end
