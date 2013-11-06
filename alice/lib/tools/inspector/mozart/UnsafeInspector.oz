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
   'UnsafeInspector$': UnsafeInspector
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
\ifndef Mozart_1_2
	 [] 'failed'     then true
\endif
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
\ifndef Mozart_1_2
	 [] 'failed'      then true
\endif
	    %% Relation Variable Stuff
	 [] 'variable'    then true %% Variable Definition
	 [] 'variableref' then true %% Variable Reference
	    %% Misc Stuff
	 [] 'misc'        then true
	 [] 'round'       then true %% Round Brackets around Lists
	    %% Arrow Stuff
	 [] 'widthbitmap' then true
	 [] 'depthbitmap' then true
	    %% Abstract Type Stuff
	 [] 'promise'     then true %% Promise
	 [] 'package'     then true %% Package
	 [] 'typeindicator' then true %% Type indicators (currently fd only)
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
\ifndef Mozart_1_2
			  failed#failed
\endif
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
      {Inspector.configure widgetUseNodeSet 1} %% Can be 1 or 2
      {Inspector.configure widgetNodesContainer TreeNodes}
      {Inspector.configure widgetNodeSets ((NormalNodes|RelationNodes)#
					   (NormalIndNodes|RelationIndNodes))}
      {Inspector.configure widgetAtomicTest IsAtomic}
      {Inspector.configure widgetCellPollInterval 500}
   end

   %% GUI Specific (Type->Name) Conversion Table
   local
      ConversionTable = [ number        # 'Int, Real and Word'
			  function      # 'Function'
			  text          # 'String'
			  cell          # 'Reference'
			  constructor   # 'Constructor'
			  typeindicator # 'Type Indicator'
			  package       # 'Package'
			  promise       # 'Promise'
			  tuple         # 'Tuple'
			  vector        # 'Vector'
			  conval        # 'Constructed Value'
			  record        # 'Record'
			  reclabel      # 'Record Label'
			  list          # 'List'
			  future        # 'Future'
\ifndef Mozart_1_2
			  failed        # 'Failed Future'
\endif
			  fdint         # 'Finite-Domain-Variable'
			  fset          # 'Finite-Set-Variable'
			  variable      # 'Co-Reference Definition'
			  variableref   # 'Co-Reference Usage'
			  misc          # 'Miscellany'
			  round         # 'Round Brackets'
			  widthbitmap   # 'Width-Limit-Indicator'
			  depthbitmap   # 'Depth-Limit-Indicator'
			  free          # 'Hole'
			]
   in
      {Inspector.configure typeConversion ConversionTable}
   end

   %% Introduce abstract types
   local
      fun {ExpandArray V W D}
	 AW = {Array.high V}
	 AT = {Tuple.make '@Array__' (AW + 1)}
      in
	 {For 0 AW 1 proc {$ I} AT.(I+1) = {Array.get V I} end} AT
      end
   in
      {Inspector.configure arrayMenu
       menu(nil nil [auto('Expand'(ExpandArray))] nil)} 
   end

   local
      fun {ExpandThread V W D}
	 State = case {Thread.state V}
		 of runnable   then 'RUNNABLE'
		 [] blocked    then 'BLOCKED'
		 [] terminated then 'TERMINATED'
		 end
      in
	 '@Thread__'(State)
      end
   in
      {Inspector.configure threadMenu
       menu(nil nil [auto('Expand'(ExpandThread))] nil)}
   end

   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   %% Create Exported Structure
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   ConfigurationError = {NewName}

   %% Configuration Defaults
   local
      White  = '#ffffff'
      Black  = '#000000'
      Blue   = '#0000ff'
      Green  = '#228b22'
      Red    = '#ff0000'
      Purple = '#a020f0'
      Brown  = '#b886b0'

      DefaultWidths = [1 10 25 100 0 ~1 ~10 ~25 100]
      DefaultDepths = [~1 0 1 2 3 4 5 10 20]

      DefaultActions = ['Reinspect'(reinspect)
			'Print'(System.show)]
      FunctionActions = {Append DefaultActions
			 ['Inspect Closure'(InspectClosure)]}
      FutureActions = {Append DefaultActions
		       ['Force Evaluation'(proc {$ X} try {Wait X} catch _ then skip end end)]}

      proc {InspectClosure P}
	 {Inspector.inspect {Debug.getGlobals P}}
      end

      Colors = colors('NUMBER':           {NewCell Purple}
		      'FUNCTION':         {NewCell Black}
		      'STRING':           {NewCell Black}
		      'HOLE':             {NewCell Red}
		      'FUTURE':           {NewCell Red}
		      'PROMISE':          {NewCell Brown}
		      'PACKAGE':          {NewCell Brown}
		      'CONSTRUCTOR':      {NewCell Blue}
		      'TYPEINDICATOR':    {NewCell Brown}
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

      {Inspector.configure typeprefixColor Brown}
      {Inspector.configure proxyColor White}

      Widths = widths('TUPLE':             {NewCell DefaultWidths}
		      'RECORD':            {NewCell DefaultWidths}
		      'LIST':              {NewCell DefaultWidths}
		      'REFERENCE':         {NewCell DefaultWidths}
		      'CONSTRUCTED_VALUE': {NewCell DefaultWidths}
		      'VECTOR':            {NewCell DefaultWidths})

      Depths = depths('TUPLE':             {NewCell DefaultDepths}
		      'RECORD':            {NewCell DefaultDepths}
		      'LIST':              {NewCell DefaultDepths}
		      'REFERENCE':         {NewCell DefaultDepths}
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
			      'TYPEINDICATOR':    typeindicatorColor
			      'PROMISE':          promiseColor
			      'PACKAGE':          packageColor
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
	     if Type == 'CELL_POLL_INTERVAL'
	     then {Inspector.configure widgetCellPollInterval Option.1}
	     elseif {HasFeature Colors Type} then
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
	       'DEPTH_ARROW' 'PARENTHESES' 'MISC' 'TYPEINDICATOR' 'PACKAGE' 'PROMISE']
       ConfigureColor}
      {ForAll ['NUMBER' 'FUNCTION' 'STRING' 'HOLE' 'FUTURE' 'CONSTRUCTOR'
	       'REFERENCE' 'FD' 'FSET' 'TUPLE' 'RECORD' 'LIST'
	       'CONSTRUCTED_VALUE' 'VECTOR']
       ConfigureMenu}
   end

   %% Create Inspect Function
   fun {AliceInspect V}
      {Inspector.inspect V}
      unit
   end

   %% Create Inspector Interface
   UnsafeInspector = 'UnsafeInspector'(
			%% Exception
			'ConfigurationError' : ConfigurationError
			'\'ConfigurationError' : ConfigurationError
			%% Functions
			'inspect': AliceInspect
			'inspectN':
			   fun {$ N V}
			      {Inspector.inspectN N V}
			      unit
			   end
			'Inspect$':
			   fun {$ X}
			      {Inspector.inspect X.'X$'}
			      unit
			   end
			'InspectType$':
			   fun {$ X}
			      {Inspector.inspect X.'$t'}
			      unit
			   end
			'InspectSig$':
			   fun {$ X}
			      {Inspector.inspect X.'$S$'}
			      unit
			   end
			'configure': Configure)
end
