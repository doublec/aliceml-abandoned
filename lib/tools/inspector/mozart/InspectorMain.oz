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
   Inspector(inspect inspectN configure)
   TreeNodesComponent('nodes' : TreeNodes) at 'TreeNodes'
export
   'Inspector$' : AliceInspector
define
   %% Type Variables
   %% No types defined

   %% Interface Functions
   fun {InspectFun V}
      {Inspector.inspect V}
      unit
   end
   fun {InspectNFun N V}
      {Inspector.inspectN N V}
      unit
   end
   fun {ConfigureFun K V}
      {Inspector.configure K V}
      unit
   end

   %%
   %% Change Default Inspector Bindings
   %%

   %% Set appropriate Name
   {Inspector.configure inspectorLanguage 'Alice'}

   %% Set appropriate Map and Color-Change-Settings
   local
      fun {AliceMapFilter Type}
	 case {String.toAtom Type}
	    %% Atomic Stuff
	 of 'number'     then false %% Int, Real and Word
	 [] 'function'   then false %% Function
	 [] 'text'       then true  %% String
	 [] 'cell'       then true  %% Cell
	    %% Container Stuff
	 [] 'tuple'      then true  %% Tuple
	 [] 'vector'     then true  %% Vector
	 [] 'conval'     then true  %% Constructed Values
	 [] 'record'     then true  %% Record
	 [] 'list'       then true  %% List (also pipetuple)
	    %% Variable Stuff 
	 [] 'future'     then true
	 [] 'fdint'      then true
	 [] 'fset'       then true
	 else false
	 end
      end
      fun {AliceColorFilter Type}
	 case {String.toAtom Type}
	    %% Atomic Stuff
	 of 'number'      then true %% Int, Real and Word
	 [] 'function'    then true %% Function
	 [] 'text'        then true %% String
	 [] 'cell'        then true %% Cell
	    %% Container Stuff
	 [] 'constructor' then true %% Constructor (includes true, false, nil, ::)
	 [] 'tuple'       then true %% Tuple
	 [] 'vector'      then true %% Vector
	 [] 'conval'      then true %% Constructed Value
	 [] 'record'      then true %% Record
	 [] 'reclabel'    then true %% Record Label
	 [] 'list'        then true %% List (also pipetuple)
	    %% Variable Stuff
	 [] 'future'      then true
	    %% Relation Variable Stuff
	 [] 'variable'    then true %% Variable Definition
	 [] 'variableref' then true %% Variable Reference
	    %% Misc Stuff
	 [] 'misc'        then true
	 [] 'round'       then true %% Round Brackets around Lists
	    %% Arrow Stuff
	 [] 'widthbitmap' then true
	 [] 'depthbitmap' then true
	 else false
	 end
      end
   in
      {Inspector.configure inspectorOptionsFilter
       fun {$ Mode Type}
	  case Mode
	  of map   then {AliceMapFilter Type}
	  [] color then {AliceColorFilter Type} 
	  end
       end}
   end
   
   %% Set appropriate NodeSets
   local
      %% Node Translation Tables
      NormalNodes      = [int#int word#word float#float atom#atom name#name procedure#procedure
			  hashtuple#tuple pipetuple#list labeltuple#vector
			  record#record fdint#fdInt cell#cell
			  fset#fsVal fsvar#fsVar free#free future#future
			  byteString#byteString]
      RelationNodes    = [int#int word#word float#float atom#atom name#name procedure#procedure
			  hashtuple#tupleGr pipetuple#listGr
			  labeltuple#vectorGr record#recordGr
			  fdint#fdIntGr cell#cell
			  fset#fsValGr fsvar#fsVarGr free#freeGr future#futureGr
			  byteString#byteString]
      NormalIndNodes   = [int#int word#word float#float atom#atom name#name
			  procedure#procedure hashtuple#tupleInd
			  pipetuple#list labeltuple#vectorInd record#recordInd
			  fdint#fdInt cell#cell
			  fset#fsVal fsvar#fsVar free#free future#future
			  byteString#byteString]
      RelationIndNodes = [int#int word#word float#float atom#atom name#name procedure#procedure
			  hashtuple#tupleGrInd pipetuple#listGr
			  labeltuple#vectorGrInd record#recordGrInd
			  cell#cell fdint#fdIntGr
			  fset#fsValGr fsvar#fsVarGr free#freeGr future#futureGr
			  byteString#byteString]
   in
      {Inspector.configure widgetTreeWidth 50}
      {Inspector.configure widgetTreeDepth 15}
      {Inspector.configure widgetTreeDisplayMode false}
      {Inspector.configure widgetUseNodeSet 2} %% Can be 1 or 2
      {Inspector.configure widgetNodesContainer TreeNodes}
      {Inspector.configure widgetNodeSets ((NormalNodes|RelationNodes)#
					   (NormalIndNodes|RelationIndNodes))}
   end

   %% Specify Default Colors
   local
      local
	 Black  = '#000000'
	 Blue   = '#0000ff'
	 Green  = '#228b22'
	 Red    = '#ff0000'
	 Purple = '#a020f0'
      in
	 ColorDefaults = [
			  number      # Purple
			  text        # Black
			  constructor # Blue
			  tuple       # Black
			  record      # Black
			  reclabel    # Green
			  list        # Black
			  vector      # Black
			  function    # Black
			  misc        # Black
			  %% These are arrows
			  widthbitmap # Red
			  depthbitmap # Red
			  %% Relation Variable Definition and Reference Colors
			  variable    # Red
			  variableref # Red
			  round       # Black
			 ]
      end
   in
      {ForAll ColorDefaults proc {$ Key#Color}
			       {Inspector.configure {VirtualString.toAtom Key#'Color'} Color}
			    end}
   end

   %% Specify Default Menus
   local
      %% Default Width and Depth Lists
      WidthList = [1 5 10 0 ~1 ~5 ~10]
      DepthList = [1 5 10 0 ~1 ~5 ~10]
      
      ContainerMenus = [
			tupleMenu # menu(WidthList
					 DepthList
					 nil
					 ['Reinspect'(reinspect)])
			vectorMenu # menu(WidthList
					  DepthList
					  nil
					  ['Reinspect'(reinspect)])
			convalMenu # menu(WidthList
					  DepthList
					  nil
					  ['Reinspect'(reinspect)])
			listMenu   # menu(WidthList
					  DepthList
					  nil
					  ['Reinspect'(reinspect)])
			recordMenu # menu(WidthList
					  DepthList
					  nil
					  ['Reinspect'(reinspect)])
		       ]
   in
      {ForAll ContainerMenus proc {$ Key#Menu}
				{Inspector.configure Key Menu}
			     end}
   end
   
   %% GUI Specific (Type->Name) Conversion Table
   local
      ConversionTable = [ number       # 'Int, Real and Word'
			  function     # 'Function'
			  text         # 'String'
			  cell         # 'Reference'
			  constructor  # 'Constructor'
			  tuple        # 'Tuple'
			  vector       # 'Vector'
			  conval       # 'Constructed Value'
			  record       # 'Record'
			  reclabel     # 'Record Label'
			  list         # 'List'
			  future       # 'Future'
			  fdint        # 'Finite-Domain-Variable'
			  fset         # 'Finite-Set-Variable'
			  variable     # 'Relation Variable'
			  variableref  # 'Rel-Var Reference'
			  misc         # 'Miscellany'
			  round        # 'Round Brackets'
			  widthbitmap  # 'Width-Limit-Indicator'
			  depthbitmap  # 'Depth-Limit-Indicator'
			]
   in
      {Inspector.configure typeConversion ConversionTable}
   end

   %% Create Inspector Interface
   AliceInspector = 'Inspector'('inspect'      : InspectFun
				'inspectN'     : InspectNFun
				'InspectType$' : InspectFun
				'InspectSig$'  : InspectFun
				'Inspect$'     : fun {$ S X} {InspectFun X} end
				'configure'    : ConfigureFun)
end
