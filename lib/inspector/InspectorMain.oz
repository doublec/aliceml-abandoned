%%%
%%% Authors:
%%%   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Contributor:
%%%   Andreas Rossberg <rossberg@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Thorsten Brunklaus and Leif Kornstaedt, 2000-2001
%%%
%%% Last Change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   Debug(getGlobals) at 'x-oz://boot/Debug'
   System(show)
   Inspector(inspect inspectN configure)
   TreeNodesComponent('nodes' : TreeNodes) at 'TreeNodes'
export
   '$INSPECTOR$': INSPECTOR
   'Inspector$': AliceInspector
define
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   %% Change Default Inspector Bindings
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
      RelationNodes    = [int#int word#word float#float atom#atom name#nameGr procedure#procedure
			  hashtuple#tupleGr pipetuple#listGr
			  labeltuple#vectorGr record#recordGr
			  fdint#fdIntGr cell#cellGr
			  fset#fsValGr fsvar#fsVarGr free#freeGr future#futureGr
			  byteString#byteString]
      NormalIndNodes   = [int#int word#word float#float atom#atom name#name
			  procedure#procedure hashtuple#tuple
			  pipetuple#list labeltuple#vectorInd record#recordInd
			  fdint#fdInt cell#cellInd
			  fset#fsVal fsvar#fsVar free#free future#future
			  byteString#byteString]
      RelationIndNodes = [int#int word#word float#float atom#atom name#nameGr procedure#procedure
			  hashtuple#tupleGr pipetuple#listGr
			  labeltuple#vectorGrInd record#recordGrInd
			  cell#cellGrInd fdint#fdIntGr
			  fset#fsValGr fsvar#fsVarGr free#freeGr future#futureGr
			  byteString#byteString]

      fun {IsAtomic V}
	 case {Value.status V}
	 of det(tuple)  then false
	 [] det(record) then false
	 [] det(cell)   then false
	 [] det(name)   then false
	 [] det(_)      then true
	 else false
	 end
      end
   in
      {Inspector.configure widgetTreeWidth 50}
      {Inspector.configure widgetTreeDepth 15}
      {Inspector.configure widgetTreeDisplayMode true}
      {Inspector.configure widgetUseNodeSet 2} %% Can be 1 or 2
      {Inspector.configure widgetNodesContainer TreeNodes}
      {Inspector.configure widgetNodeSets ((NormalNodes|RelationNodes)#
					   (NormalIndNodes|RelationIndNodes))}
      {Inspector.configure widgetAtomicTest IsAtomic}
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
			  free         # 'Hole'
			]
   in
      {Inspector.configure typeConversion ConversionTable}
   end

   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   %% Create Exported Signature
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   INSPECTOR = 'INSPECTOR'()   %--** rtt not implemented

   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   %% Create Exported Structure
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   ConfigurationError = {NewName}

   %% Configuration Defaults
   local
      Black  = '#000000'
      Blue   = '#0000ff'
      Green  = '#228b22'
      Red    = '#ff0000'
      Purple = '#a020f0'
      Brown  = '#b886b0'

      DefaultWidths = [1 5 10 0 ~1 ~5 ~10]
      DefaultDepths = [1 5 10 0 ~1 ~5 ~10]

      DefaultActions = ['Reinspect'(reinspect)
			'Print'(System.show)]
      FunctionActions = {Append DefaultActions
			 ['Inspect Closure'(InspectClosure)]}
      FutureActions = {Append DefaultActions
		       ['Force Evaluation'(Wait)]}

      proc {InspectClosure P}
	 {Inspector.inspect {Debug.getGlobals P}}
      end

      Colors = colors('NUMBER':           {NewCell Purple}
		      'FUNCTION':         {NewCell Black}
		      'STRING':           {NewCell Black}
		      'HOLE':             {NewCell Brown}
		      'FUTURE':           {NewCell Brown}
		      'CONSTRUCTOR':      {NewCell Blue}
		      'TUPLE':            {NewCell Black}
		      'RECORD':           {NewCell Black}
		      'LIST':             {NewCell Black}
		      'VECTOR':           {NewCell Black}
		      'RECORD_LABEL':     {NewCell Green}
		      'ALIAS_DEFINITION': {NewCell Red}
		      'ALIAS_REFERENCE':  {NewCell Red}
		      'WIDTH_ARROW':      {NewCell Red}
		      'DEPTH_ARROW':      {NewCell Red}
		      'PARENTHESES':      {NewCell Black}
		      'MISC':             {NewCell Black})

      Widths = widths('TUPLE':             {NewCell DefaultWidths}
		      'RECORD':            {NewCell DefaultWidths}
		      'LIST':              {NewCell DefaultWidths}
		      'CONSTRUCTED_VALUE': {NewCell DefaultWidths}
		      'VECTOR':            {NewCell DefaultWidths})

      Depths = depths('TUPLE':             {NewCell DefaultDepths}
		      'RECORD':            {NewCell DefaultDepths}
		      'LIST':              {NewCell DefaultDepths}
		      'CONSTRUCTED_VALUE': {NewCell DefaultDepths}
		      'VECTOR':            {NewCell DefaultDepths})

      Actions = actions('NUMBER':            {NewCell DefaultActions}
			'FUNCTION':          {NewCell FunctionActions}
			'STRING':            {NewCell DefaultActions}
			'HOLE':              {NewCell DefaultActions}
			'FUTURE':            {NewCell FutureActions}
			'CONSTRUCTOR':       {NewCell DefaultActions}
			'REFERENCE':         {NewCell DefaultActions}
			'FD':                {NewCell DefaultActions}
			'FSET':              {NewCell DefaultActions}
			'TUPLE':             {NewCell DefaultActions}
			'RECORD':            {NewCell DefaultActions}
			'LIST':              {NewCell DefaultActions}
			'CONSTRUCTED_VALUE': {NewCell DefaultActions}
			'VECTOR':            {NewCell DefaultActions})

      ColorNames = colorNames('NUMBER':           numberColor
			      'FUNCTION':         functionColor
			      'STRING':           textColor
			      'CONSTRUCTOR':      constructorColor
			      'HOLE':             freeColor
			      'FUTURE':           futureColor
			      'TUPLE':            tupleColor
			      'RECORD':           recordColor
			      'LIST':             listColor
			      'VECTOR':           vectorColor
			      'RECORD_LABEL':     reclabelColor
			      'ALIAS_DEFINITION': variableColor
			      'ALIAS_REFERENCE':  variablerefColor
			      'WIDTH_ARROW':      widthbitmapColor
			      'DEPTH_ARROW':      depthbitmapColor
			      'PARENTHESES':      roundColor
			      'MISC':             miscColor)

      MenuNames = menuNames('NUMBER':            numberMenu
			    'FUNCTION':          functionMenu
			    'STRING':            textMenu
			    'REFERENCE':         cellMenu
			    'CONSTRUCTOR':       constructorMenu
			    'HOLE':              freeMenu
			    'FUTURE':            futureMenu
			    'FD':                fdintMenu
			    'FSET':              fsetMenu
			    'TUPLE':             tupleMenu
			    'RECORD':            recordMenu
			    'LIST':              listMenu
			    'CONSTRUCTED_VALUE': convalMenu
			    'VECTOR':            vectorMenu)

      Hex = hex(0x0: &0 0x1: &1 0x2: &2 0x3: &3
		0x4: &4 0x5: &5 0x6: &6 0x7: &7
		0x8: &8 0x9: &9 0xA: &a 0xB: &b
		0xC: &c 0xD: &d 0xE: &e 0xF: &f)

      proc {ConfigureColor Type}
	 {Inspector.configure ColorNames.Type {Access Colors.Type}}
      end

      proc {ConfigureMenu Type}
	 {Inspector.configure MenuNames.Type
	  menu(if {HasFeature Widths Type}
	       then {Access Widths.Type} else nil end
	       if {HasFeature Depths Type}
	       then {Access Depths.Type} else nil end
	       nil
	       {Access Actions.Type})}
      end

      proc {SetColor Type Color}
	 case Color of 'KEEP_COLOR' then skip
	 [] 'SET_COLOR'(red: Red green: Green blue: Blue) then
	    if Red < 0 orelse Red > 0xFF
	       orelse Green < 0 orelse Green > 0xFF
	       orelse Blue < 0 orelse Blue > 0xFF
	    then {Exception.raiseError alice(ConfigurationError)}
	    end
	    {Assign Colors.Type
	     {VirtualString.toAtom [&#
				    Hex.(Red div 16)
				    Hex.(Red mod 16)
				    Hex.(Green div 16)
				    Hex.(Green mod 16)
				    Hex.(Blue div 16)
				    Hex.(Blue mod 16)]}}
	    {ConfigureColor Type}
	 end
      end

      proc {SetWidth Type Width}
	 case Width of 'KEEP_WIDTHS' then skip
	 [] 'REPLACE_WIDTHS'(Is) then
	    {Assign Widths.Type {Record.toList Is}}
	    {ConfigureMenu Type}
	 [] 'APPEND_WIDTH'(I) then
	    {Assign Widths.Type {Append {Access Widths.Type} [I]}}
	    {ConfigureMenu Type}
	 [] 'REMOVE_WIDTH'(I) then
	    {Assign Widths.Type
	     {Filter {Access Widths.Type} fun {$ J} I \= J end}}
	    {ConfigureMenu Type}
	 end
      end

      proc {SetDepth Type Depth}
	 case Depth of 'KEEP_DEPTHS' then skip
	 [] 'REPLACE_DEPTHS'(Is) then
	    {Assign Depths.Type {Record.toList Is}}
	    {ConfigureMenu Type}
	 [] 'APPEND_DEPTH'(I) then
	    {Assign Depths.Type {Append {Access Depths.Type} [I]}}
	    {ConfigureMenu Type}
	 [] 'REMOVE_DEPTH'(I) then
	    {Assign Depths.Type
	     {Filter {Access Depths.Type} fun {$ J} I \= J end}}
	    {ConfigureMenu Type}
	 end
      end

      proc {SetAction Type Action}
	 case Action of 'KEEP_ACTIONS' then skip
	 [] 'REPLACE_ACTIONS'(NameProcPairs) then
	    {Assign Actions.Type
	     {Map {Record.toList NameProcPairs}
	      fun {$ Name#Proc} A in
		 A = {VirtualString.toAtom Name}
		 A(proc {$ X} _ = {Proc X} end)
	      end}}
	    {ConfigureMenu Type}
	 [] 'APPEND_ACTION'(Name Proc) then A in
	    A = {VirtualString.toAtom Name}
	    {Assign Actions.Type
	     {Append {Access Actions.Type} [A(proc {$ X} _ = {Proc X} end)]}}
	    {ConfigureMenu Type}
	 [] 'REMOVE_ACTION'(Name) then A in
	    A = {VirtualString.toAtom Name}
	    {Assign Actions.Type
	     {Filter {Access Actions.Type} fun {$ X} {Label X} \= A end}}
	    {ConfigureMenu Type}
	 end
      end
   in
      fun {Configure Options}
	 {Record.forAll Options
	  proc {$ Option}
	     Type = {Label Option}
	  in
	     if {HasFeature Colors Type} then
		case {Width Option} of 1 then
		   {SetColor  Type Option.1}
		[] 2 then
		   {SetColor  Type Option.1}
		   {SetAction Type Option.2}
		[] 4 then
		   {SetColor  Type Option.1}
		   {SetWidth  Type Option.2}
		   {SetDepth  Type Option.3}
		   {SetAction Type Option.4}
		end
	     else
		case {Width Option} of 1 then
		   {SetAction Type Option.1}
		[] 3 then
		   {SetWidth  Type Option.1}
		   {SetDepth  Type Option.2}
		   {SetAction Type Option.3}
		end
	     end
	  end}
	 unit
      end

      %% Set Defaults
      {ForAll ['NUMBER' 'FUNCTION' 'STRING' 'HOLE' 'FUTURE' 'CONSTRUCTOR'
	       'TUPLE' 'RECORD' 'LIST' 'VECTOR' 'RECORD_LABEL'
	       'ALIAS_DEFINITION' 'ALIAS_REFERENCE' 'WIDTH_ARROW'
	       'DEPTH_ARROW' 'PARENTHESES' 'MISC']
       ConfigureColor}
      {ForAll ['NUMBER' 'FUNCTION' 'STRING' 'HOLE' 'FUTURE' 'CONSTRUCTOR'
	       'REFERENCE' 'FD' 'FSET' 'TUPLE' 'RECORD' 'LIST'
	       'CONSTRUCTED_VALUE' 'VECTOR']
       ConfigureMenu}
   end

   %% Create Inspector Interface
   AliceInspector =
   'Inspector'(%% Types
	       'value$':  {Value.byNeedFail rttNotImplemented}
	       'color$':  {Value.byNeedFail rttNotImplemented}
	       'width$':  {Value.byNeedFail rttNotImplemented}
	       'depth$':  {Value.byNeedFail rttNotImplemented}
	       'action$': {Value.byNeedFail rttNotImplemented}
	       'option$': {Value.byNeedFail rttNotImplemented}

	       %% Constructors
	       'KEEP_COLOR':           'KEEP_COLOR'
	       '\'KEEP_COLOR':         'KEEP_COLOR'
	       'SET_COLOR':            fun {$ X} 'SET_COLOR'(X) end
	       '\'SET_COLOR':          'SET_COLOR'
	       'ConfigurationError':   ConfigurationError
	       '\'ConfigurationError': ConfigurationError
	       'KEEP_WIDTHS':          'KEEP_WIDTHS'
	       '\'KEEP_WIDTHS':        'KEEP_WIDTHS'
	       'REPLACE_WIDTHS':       fun {$ X} 'REPLACE_WIDTHS'(X) end
	       '\'REPLACE_WIDTHS':     'REPLACE_WIDTHS'
	       'APPEND_WIDTH':         fun {$ X} 'APPEND_WIDTH'(X) end
	       '\'APPEND_WIDTH':       'APPEND_WIDTH'
	       'REMOVE_WIDTH':         fun {$ X} 'REMOVE_WIDTH'(X) end
	       '\'REMOVE_WIDTH':       'REMOVE_WIDTH'
	       'KEEP_DEPTHS':          'KEEP_DEPTHS'
	       '\'KEEP_DEPTHS':        'KEEP_DEPTHS'
	       'REPLACE_DEPTHS':       fun {$ X} 'REPLACE_DEPTHS'(X) end
	       '\'REPLACE_DEPTHS':     'REPLACE_DEPTHS'
	       'APPEND_DEPTH':         fun {$ X} 'APPEND_DEPTH'(X) end
	       '\'APPEND_DEPTH':       'APPEND_DEPTH'
	       'REMOVE_DEPTH':         fun {$ X} 'REMOVE_DEPTH'(X) end
	       '\'REMOVE_DEPTH':       'REMOVE_DEPTH'
	       'KEEP_ACTIONS':         'KEEP_ACTIONS'
	       '\'KEEP_ACTIONS':       'KEEP_ACTIONS'
	       'REPLACE_ACTIONS':      fun {$ X} 'REPLACE_ACTIONS'(X) end
	       '\'REPLACE_ACTIONS':    'REPLACE_ACTIONS'
	       'APPEND_ACTION':        fun {$ X} 'APPEND_ACTION'(X) end
	       '\'APPEND_ACTION':      'APPEND_ACTION'
	       'REMOVE_ACTION':        fun {$ X} 'REMOVE_ACTION'(X) end
	       '\'REMOVE_ACTION':      'REMOVE_ACTION'
	       'NUMBER':               fun {$ X} 'NUMBER'(X) end
	       '\'NUMBER':             'NUMBER'
	       'FUNCTION':             fun {$ X} 'FUNCTION'(X) end
	       '\'FUNCTION':           'FUNCTION'
	       'STRING':               fun {$ X} 'STRING'(X) end
	       '\'STRING':             'STRING'
	       'HOLE':                 fun {$ X} 'HOLE'(X) end
	       '\'HOLE':               'HOLE'
	       'FUTURE':               fun {$ X} 'FUTURE'(X) end
	       '\'FUTURE':             'FUTURE'
	       'CONSTRUCTOR':          fun {$ X} 'CONSTRUCTOR'(X) end
	       '\'CONSTRUCTOR':        'CONSTRUCTOR'
	       'REFERENCE':            fun {$ X} 'REFERENCE'(X) end
	       '\'REFERENCE':          'REFERENCE'
	       'FD':                   fun {$ X} 'FD'(X) end
	       '\'FD':                 'FD'
	       'FSET':                 fun {$ X} 'FSET'(X) end
	       '\'FSET':               'FSET'
	       'TUPLE':                fun {$ X} 'TUPLE'(X) end
	       '\'TUPLE':              'TUPLE'
	       'RECORD':               fun {$ X} 'RECORD'(X) end
	       '\'RECORD':             'RECORD'
	       'LIST':                 fun {$ X} 'LIST'(X) end
	       '\'LIST':               'LIST'
	       'CONSTRUCTED_VALUE':    fun {$ X} 'CONSTRUCTED_VALUE'(X) end
	       '\'CONSTRUCTED_VALUE':  'CONSTRUCTED_VALUE'
	       'VECTOR':               fun {$ X} 'VECTOR'(X) end
	       '\'VECTOR':             'VECTOR'
	       'RECORD_LABEL':         fun {$ X} 'RECORD_LABEL'(X) end
	       '\'RECORD_LABEL':       'RECORD_LABEL'
	       'ALIAS_DEFINITION':     fun {$ X} 'ALIAS_DEFINITION'(X) end
	       '\'ALIAS_DEFINITION':   'ALIAS_DEFINITION'
	       'ALIAS_REFERENCE':      fun {$ X} 'ALIAS_REFERENCE'(X) end
	       '\'ALIAS_REFERENCE':    'ALIAS_REFERENCE'
	       'WIDTH_ARROW':          fun {$ X} 'WIDTH_ARROW'(X) end
	       '\'WIDTH_ARROW':        'WIDTH_ARROW'
	       'DEPTH_ARROW':          fun {$ X} 'DEPTH_ARROW'(X) end
	       '\'DEPTH_ARROW':        'DEPTH_ARROW'
	       'PARENTHESES':          fun {$ X} 'PARENTHESES'(X) end
	       '\'PARENTHESES':        'PARENTHESES'
	       'MISC':                 fun {$ X} 'MISC'(X) end
	       '\'MISC':               'MISC'

	       %% Functions
	       'inspect':
		  fun {$ V}
		     {Inspector.inspect V}
		     unit
		  end
	       'inspectN':
		  fun {$ N V}
		     {Inspector.inspectN N V}
		     unit
		  end
	       'InspectType$':
		  fun {$ X}
		     {Inspector.inspect X.'$t'}
		     unit
		  end
	       'InspectSig$':
		  fun {$ X}
		     {Inspector.inspect
		      {X.'$S$' unit}}
		     unit
		  end
	       'InspectValSig$':
		  fun {$ X}
		     {Inspector.inspect
		      '#'('val':X.x 'sig':{X.'$S$' unit})}
		     unit
		  end
	       'Inspect$':
		  fun {$ X}
		     {Inspector.inspect X.'X$'}
		     unit
		  end
	       'configure': Configure)
end
