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
%   HelperComponent('nodes' : Helper) at 'Helper'
export
   nameGrDrawObject      : NameGrDrawObject
   vectorIndDrawObject   : VectorIndDrawObject
   vectorGrIndDrawObject : VectorGrIndDrawObject
   listGrMDrawObject     : ListGrMDrawObject
   listGrSDrawObject     : ListGrSDrawObject
define
   %% Import all needed DrawObjects
   local
      DrawObjects = TreeNodes.'draw'
   in
      OzFreeGrDrawObject        = DrawObjects.freeGrDrawObject
      LabelTupleDrawObject      = DrawObjects.labelTupleDrawObject
      LabelTupleIndDrawObject   = DrawObjects.labelTupleIndDrawObject
      LabelTupleGrDrawObject    = DrawObjects.labelTupleGrDrawObject
      LabelTupleGrIndDrawObject = DrawObjects.labelTupleGrIndDrawObject
      PipeTupleGrMDrawObject    = DrawObjects.pipeTupleGrMDrawObject
      PipeTupleGrSDrawObject    = DrawObjects.pipeTupleGrSDrawObject
   end

   %%
   %% Simple Objects
   %%
   
   class NameGrDrawObject from OzFreeGrDrawObject end
   
   %%
   %% Container Objects
   %%

   class VectorIndDrawObject from LabelTupleDrawObject LabelTupleIndDrawObject
      meth drawBody(X Y)
	 case @type
	 of vector then LabelTupleDrawObject, drawBody(X Y)
	 [] conval then LabelTupleIndDrawObject, drawBody(X Y)
	 end
      end
      meth searchBody(XA YA X Y $)
	 case @type
	 of vector then LabelTupleDrawObject, searchBody(XA YA X Y $)
	 [] conval then LabelTupleIndDrawObject, searchBody(XA YA X Y $)
	 end
      end
   end

   class VectorGrIndDrawObject from LabelTupleGrDrawObject LabelTupleGrIndDrawObject
      meth drawBody(X Y)
	 case @type
	 of vector then LabelTupleGrDrawObject, drawBody(X Y)
	 [] conval then LabelTupleGrIndDrawObject, drawBody(X Y)
	 end
      end
      meth searchBody(XA YA X Y $)
	 case @type
	 of vector then LabelTupleGrDrawObject, searchBody(XA YA X Y $)
	 [] conval then LabelTupleGrIndDrawObject, searchBody(XA YA X Y $)
	 end
      end
   end
   
   class ListGrMDrawObject from PipeTupleGrMDrawObject end

   class ListGrSDrawObject from PipeTupleGrSDrawObject end
end
