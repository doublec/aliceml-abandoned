%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 1999-2002
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

\ifdef Mozart_1_2
\define OLD_BYNEED
\endif

functor
import
   Application(getArgs exit)
   Property(get put)
   System(printError onToplevel)
   Error(registerFormatter printException)
   OS(getEnv)
   Module(manager)
   DefaultURL(functorExt ozScheme nameToUrl)
   URL(toString)
   Pickle(load)
   Prebound(builtinTable: BuiltinTable
	    raiseAliceException: RaiseAliceException
	    unwrapAliceException: UnwrapAliceException)
define
   {Error.registerFormatter alice
    fun {$ E} T in
       T = 'Alice exception'
       case E of alice(E Coord) then
	  error(kind: T
		items: [hint(l: 'Exception' m: oz(E))
			hint(l: 'Raised at' m: Coord)])
       [] alice(failed F I J) then
	  error(kind: T
		msg: 'Evaluated failed expression'
		items: [hint(l: 'At' m: pos(F I J))])
       [] alice(InnerE ...) then
	  error(kind: T
		items: (hint(l: 'Exception' m: oz(InnerE))|
			{List.mapInd {Record.toList E}.2
			 fun {$ I X} hint(l: 'Debug '#I m: oz(X)) end}))
       else
	  error(kind: T
		items: [line(oz(E))])
       end
    end}

   {Property.put 'alice.builtinTable' BuiltinTable}
   {Property.put 'alice.raiseException' RaiseAliceException}
   {Property.put 'alice.unwrapException' UnwrapAliceException}

   {Property.put 'alice.atExnActions'
    {NewCell [fun {$ E}
		 {Error.printException E}
		 if {System.onToplevel} then
		    {Application.exit 1}
		 end
		 unit
	      end]}}

   {Property.put 'errors.handler'
    proc {$ E}
       case E of system(kernel(terminate) ...) then
	  skip
       [] error(alice(Exn ...) debug:Debug) then
	  {ForAll {Access {Property.get 'alice.atExnActions'}}
	   proc {$ P}
	      try {P Exn Debug} catch _ then skip end
	   end}
       else
	  {Error.printException E}
	  if {System.onToplevel} then
	     {Application.exit 1}
	  end
       end
    end}

   Trace = case {OS.getEnv 'ALICE_TRACE_BOOT_LINKER'} of false then
	      proc {$ _ _} skip end
	   else
	      proc {$ Prefix Key}
		 {System.printError '[boot-linker] '#Prefix#' '#Key#'\n'}
	      end
	   end

   ComponentTable = {NewDictionary}

   AliceHome = case {OS.getEnv 'ALICE_HOME'} of false then
		  {System.printError 'alicerun: ALICE_HOME not set\n'}
		  {Application.exit 1}
		  unit
	       elseof S then S#'/'
	       end

   ModuleManager = {New Module.manager init()}

   fun {Localize Key}
      {VirtualString.toAtom AliceHome#Key#'.ozf'}
   end

   local
      fun {ParentDir Base Offset}
	 if Offset == 0 then Offset
	 elsecase {ByteString.get Base Offset - 1} of &/ then Offset - 1
	 else {ParentDir Base Offset - 1}
	 end
      end

      fun {ResolveSub Base Rel Offset} Offset2 in
	 Offset2 = {ParentDir Base Offset}
	 case Rel of &.|&.|&/|Rest then {ResolveSub Base Rest Offset2}
	 elseif Offset2 == 0 then Rel
	 else {ByteString.slice Base 0 Offset2}#'/'#Rel
	 end
      end
   in
      fun {Resolve Base Rel}
	 {VirtualString.toAtom
	  case {VirtualString.toString Rel} of &x|&-|&o|&z|&:|_ then Rel
	  elseof Rel2 then Base2 in
	     Base2 = {ByteString.make Base}
	     {ResolveSub Base2 Rel2 {ByteString.width Base2}}
	  end}
      end
   end

   proc {Link Key ?Mod} Inf in
      {Trace loading Key}
      {Dictionary.put ComponentTable Key Inf#Mod}
      case {Atom.toString Key} of &x|&-|&o|&z|&:|_ then
	 Mod = {ModuleManager link(url: Key $)}
	 Inf = 'NONE'
      else F Imports in
	 F = {Pickle.load {Localize Key}}
	 {Trace linking Key}
	 Imports = {Record.mapInd F.'import'
		    fun {$ ModName Desc} Key2 in
		       Key2 = case {CondSelect Desc 'from' unit} of unit then
				 {VirtualString.toAtom
				  {URL.toString
				   {DefaultURL.nameToUrl ModName}}}
			      elseof S then {Resolve Key S}
			      end
		       if {Dictionary.member ComponentTable Key2} then
			  {Dictionary.get ComponentTable Key2}.2
		       else {Link Key2}
		       end
		    end}
\ifdef OLD_BYNEED
	 Mod = {Value.byNeed
\else
	 Mod = {Value.byNeedFuture
\endif
		fun {$} X in
		   {Trace applying Key}
		   X = {F.apply {Adjoin Imports 'IMPORT'}}
		   {Trace evaluated Key}
		   X
		end}
	 Inf = case F.'export' of sig(Sig) andthen Sig \= unit then 'SOME'(Sig)
	       else 'NONE'
	       end
      end
   end

   case {Application.getArgs plain} of Name|Rest then
      {Property.put 'alice.rootUrl' Name}
      {Property.put 'alice.initialComponentTable' ComponentTable}
      {Property.put 'ozd.args' Rest}
      {Property.put 'errors.depth' 20}
      {Property.put 'errors.width' 10}
      {{Link 'lib/system/Boot'}.boot {ByteString.make Name} _}
   [] nil then
      {System.printError 'Usage: alicerun <name> <args> ...\n'}
      {Application.exit 2}
   end
end
