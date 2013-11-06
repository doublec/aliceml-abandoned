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
   System(eq)
   Inspector('nodes' : TreeNodes)
%   HelperComponent('nodes' : Helper) at 'Helper'
export
   nameGrDrawObject      : NameGrDrawObject
   cellDrawObject        : CellDrawObject
   cellIndDrawObject     : CellIndDrawObject
   cellGrDrawObject      : CellGrDrawObject
   cellGrIndDrawObject   : CellGrIndDrawObject
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

   proc {WatchCell Node Cell Value WidPort PollMS}
      if {System.eq {Access Cell} Value}
      then {Delay PollMS} {WatchCell Node Cell Value WidPort PollMS}
      else {Port.send WidPort updateCell(Node)}
      end
   end

   class CellUpdate
      attr
	 watchThread
      meth watchCell
	 Cell        = @savedValue
	 OldValue    = {Access Cell}
	 WidPort     = {@visual getServer($)}
	 PollMS      = {@visual get(widgetCellPollInterval $)}
	 WatchThread = @watchThread
      in
	 if {IsFree WatchThread}
	 then
	    thread
	       WatchThread = {Thread.this}
	       {WatchCell self Cell OldValue WidPort PollMS}
	    end
	    %% Important: Sync on Thread ID
	    {Wait WatchThread}
	 end
      end
      meth terminate
	 WatchThread = @watchThread
      in
	 if {IsFree WatchThread}
	 then skip
	 else
	    watchThread <- _
	    case {Thread.state WatchThread}
	    of terminated then skip
	    else {Thread.terminate WatchThread}
	    end
	 end
      end
      meth tell($)
	 Dirty = @dirty
      in
	 if Dirty
	 then Dirty
	 else
	    Parent = @parent
	    Index  = @index
	    RI     = {Parent getRootIndex(Index $)}
	 in
	    {Parent replace(Index @savedValue replaceNormal)} RI
	 end
      end
   end
   
   class CellDrawObject from LabelTupleDrawObject CellUpdate
      meth drawBody(X Y)
	 LabelTupleDrawObject, drawBody(X Y)
	 CellUpdate, watchCell
      end
      meth makeDirty
	 CellUpdate, terminate
	 LabelTupleDrawObject, makeDirty
      end
   end

   class CellIndDrawObject from LabelTupleIndDrawObject CellUpdate
      meth drawBody(X Y)
	 LabelTupleIndDrawObject, drawBody(X Y)
	 CellUpdate, watchCell
      end
      meth makeDirty
	 CellUpdate, terminate
	 LabelTupleIndDrawObject, makeDirty
      end
   end

   class CellGrDrawObject from LabelTupleGrDrawObject CellUpdate
      meth drawBody(X Y)
	 LabelTupleGrDrawObject, drawBody(X Y)
	 CellUpdate, watchCell
      end
      meth makeDirty
	 CellUpdate, terminate
	 LabelTupleGrDrawObject, makeDirty
      end
   end

   class CellGrIndDrawObject from LabelTupleGrIndDrawObject CellUpdate
      meth drawBody(X Y)
	 LabelTupleGrIndDrawObject, drawBody(X Y)
	 CellUpdate, watchCell
      end
      meth makeDirty
	 CellUpdate, terminate
	 LabelTupleGrIndDrawObject, makeDirty
      end
   end
   
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
