%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2002
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

%%
%% pickle    ::= int | chunk | block | tuple | closure | transform
%% int       ::= POSINT <uint> | NEGINT <uint>
%% chunk     ::= CHUNK size <byte>*size
%% size      ::= <uint>
%% block     ::= BLOCK label size field*size
%% tuple     ::= TUPLE size field*size
%% closure   ::= CLOSURE size field*size
%% label     ::= <uint>
%% field     ::= pickle | reference
%% reference ::= REF id
%% id        ::= <uint>
%% transform ::= TRANSFORM (chunk|reference) field
%%

functor
import
   Open(file)
   System(eq)
   PrimitiveTable(table)
   AbstractCodeInterpreter(interpreter)
export
   Unpack
   Load
   module: PickleComponent
define
   %% Tags:
   POSINT    = 0
   NEGINT    = 1
   CHUNK     = 2
   BLOCK     = 3
   TUPLE     = 4
   CLOSURE   = 5
   REF       = 6
   TRANSFORM = 7

   %% Block Labels:   %--** Alice-specific
   ARRAY        = 0
   CELL         = 1
   CONSTRUCTOR  = 2
   CON_VAL      = 3
   GLOBAL_STAMP = 4
   VECTOR       = 5
   LabelOffset  = 6

   fun {ApplyTransform F X}
      %--** implement using byneeds
      case F of 'Alice.primitive' then
	 case X of tag(0 Name) then
	    PrimitiveTable.table.{VirtualString.toAtom Name}
	 end
      [] 'Alice.function' then
	 case X of tag(0 tuple(F L C) NG NL IdDefArgs Instr)
	 then function(AbstractCodeInterpreter.interpreter
		       {VirtualString.toAtom F}#L#C NG NL IdDefArgs Instr)
	 end
      end
   end

   class PickleParser
      %--** implement as an interpreter
      attr BS: unit Index: 0 Dict: unit Counter: unit
      meth init(V $)
	 BS <- {ByteString.make V}
	 Index <- 0
	 Dict <- {NewDictionary}
	 Counter <- 0
	 PickleParser, ParsePickle($)
      end
      meth Next($) I in
	 I = @Index
	 Index <- I + 1
	 {ByteString.get @BS I}
      end
      meth Remember(X) N in
	 N = @Counter
	 @Dict.N := X
	 Counter <- N + 1
      end
      meth Lookup(N $)
	 @Dict.N
      end
      meth ParsePickle($)
	 case PickleParser, Next($)
	 of !POSINT    then PickleParser, ParseUInt($)
	 [] !NEGINT    then ~(PickleParser, ParseUInt($) + 1)
	 [] !CHUNK     then PickleParser, ParseChunk($)
	 [] !BLOCK     then PickleParser, ParseBlock($)
	 [] !TUPLE     then PickleParser, ParseTuple($)
	 [] !CLOSURE   then PickleParser, ParseClosure($)
	 [] !TRANSFORM then PickleParser, ParseTransform($)
	 [] !REF       then PickleParser, ParseReference($)
	 end
      end
      meth ParseUInt($) B in
	 PickleParser, Next(?B)
	 if B >= 0x80 then PickleParser, ParseUIntSub(B - 0x80 0x80 $)
	 else B
	 end
      end
      meth ParseUIntSub(X N $) B in
	 PickleParser, Next(?B)
	 if B >= 0x80 then
	    PickleParser, ParseUIntSub(X + (B - 0x80) * N N * 0x80 $)
	 else X + B * N
	 end
      end
      meth ParseChunk(?Chunk) Size in
	 PickleParser, ParseUInt(?Size)
	 Chunk = {ByteString.make for I in 1..Size collect: C do
				     {C PickleParser, Next($)}
				  end}
	 PickleParser, Remember(Chunk)
      end
      meth ParseBlock(?T) Label Size in
	 PickleParser, Remember(T)
	 PickleParser, ParseUInt(?Label)
	 PickleParser, ParseUInt(?Size)
	 case Label
	 of !ARRAY then fail   %--**
	 [] !CELL then fail   %--**
	 [] !CONSTRUCTOR then fail   %--**
	 [] !CON_VAL then fail   %--**
	 [] !GLOBAL_STAMP then fail   %--**
	 [] !VECTOR then RealSize in
	    PickleParser, ParsePickle(?RealSize)
	    T = {MakeTuple vector RealSize}
	    for I in 1..RealSize do
	       PickleParser, ParsePickle(?T.I)
	    end
%--** the following fails when debugging:
%	    for I in RealSize+1..Size-1 do
%	       PickleParser, ParsePickle(_)
%	    end
	 else
	    T = {MakeTuple tag Size + 1}
	    T.1 = Label - LabelOffset
	    for I in 2..Size + 1 do
	       PickleParser, ParsePickle(?T.I)
	    end
	 end
      end
      meth ParseTuple(?T) Size in
	 PickleParser, Remember(T)
	 PickleParser, ParseUInt(?Size)
	 T = {MakeTuple tuple Size}
	 for I in 1..Size do
	    PickleParser, ParsePickle(?T.I)
	 end
      end
      meth ParseClosure(?Closure) Size in
	 PickleParser, Remember(Closure)
	 PickleParser, ParseUInt(?Size)
	 Closure = {MakeTuple closure Size}
	 for I in 1..Size do
	    PickleParser, ParsePickle(?Closure.I)
	 end
      end
      meth ParseTransform(?Transform) F X in
	 PickleParser, Remember(Transform)
	 PickleParser, ParsePickle(?F)
	 PickleParser, ParsePickle(?X)
	 Transform = {ApplyTransform {VirtualString.toAtom F} X}
      end
      meth ParseReference($)
	 PickleParser, Lookup(PickleParser, ParseUInt($) $)
      end
   end

   fun {Unpack V}
      {New PickleParser init(V $) _}
   end

   proc {ReadFile File ?S} F in
      F = {New Open.file init(name: File flags: [read])}
      {F read(list: ?S size: all)}
      {F close()}
   end

   fun {Load File}
      {New PickleParser init({ReadFile File} $) _}
   end

   fun {Deref X}
      case X of transient(TransientState) then
	 case {Access TransientState} of ref(Y) then {Deref Y}
	 else X
	 end
      else X
      end
   end

   class OutputStreamBase
      meth putUInt(I)
	 if I >= 0x80 then
	    {self putByte(I mod 0x80 + 0x80)}
	    OutputStreamBase, putUInt(I div 0x80)
	 else
	    {self putByte(I)}
	 end
      end
      meth putByteString(S)
	 for I in 0..{ByteString.length S} - 1 do
	    {self putByte({ByteString.get S I})}
	 end
      end
   end

   class FileOutputStream from Open.file OutputStreamBase
      meth init(Name Header <= '')
	 Open.file, init(name: Name flags: [write create truncate])
	 OutputStreamBase, putByteString({ByteString.make Header})
      end
      meth putByte(C)
	 Open.file, write(vs: [C])
      end
      meth putByteString(S)
	 Open.file, write(vs: S)
      end
   end

   class StringOutputStream from OutputStreamBase
      attr Hd: unit Tl: unit
      meth init() Empty in
	 Hd <- Empty
	 Tl <- Empty
      end
      meth putByte(C) NewTl in
	 @Tl = C|NewTl
	 Tl <- NewTl
      end
      meth close(?S)
	 @Tl = nil
	 S = {ByteString.make @Hd}
      end
   end

   fun {FindRef X Ys}
      case Ys of Y#Id|Yr then
	 if {System.eq X Y} then Id else {FindRef X Yr} end
      [] nil then unit
      end
   end

   fun {PicklingInterpreterRun Args=args(Id OutputStream Seen) TaskStack}
      %--** test for resources
      case TaskStack of pickling(_ X0)|Rest then X T in
	 X = {Deref X0}
	 T = {Value.type X}
	 case T of int then
	    if X >= 0 then
	       {OutputStream putByte(POSINT)}
	       {OutputStream putUInt(X)}
	    else
	       {OutputStream putByte(NEGINT)}
	       {OutputStream putUInt(~(X + 1))}
	    end
	    continue(Args Rest)
	 elsecase {FindRef X Seen} of unit then
	    case T of byteString then
	       {OutputStream putByte(CHUNK)}
	       {OutputStream putUInt({ByteString.length X})}
	       {OutputStream putByteString(X)}
	       continue(args(Id + 1 OutputStream X#Id|Seen) Rest)
	    [] tuple then
	       case {Label X} of transient then request(X Args TaskStack)
	       [] tuple then
		  {OutputStream putByte(TUPLE)}
		  {OutputStream putUInt({Width X})}
		  continue(args(Id + 1 OutputStream X#Id|Seen)
			   {Record.foldR X
			    fun {$ Y Rest}
			       pickling(PicklingInterpreter Y)|Rest
			    end Rest})
	       [] tag then
		  {OutputStream putByte(BLOCK)}
		  {OutputStream putUInt(LabelOffset + X.1)}
		  {OutputStream putUInt({Width X} - 1)}
		  continue(args(Id + 1 OutputStream X#Id|Seen)
			   {ForThread {Width X} 2 ~1
			    fun {$ Rest I}
			       pickling(PicklingInterpreter X.I)|Rest
			    end Rest})
	       [] con then
		  {OutputStream putByte(BLOCK)}
		  {OutputStream putUInt(CON_VAL)}
		  {OutputStream putUInt({Width X})}
		  continue(args(Id + 1 OutputStream X#Id|Seen)
			   {Record.foldR X
			    fun {$ Y Rest}
			       pickling(PicklingInterpreter Y)|Rest
			    end Rest})
	       [] vector then
		  {OutputStream putByte(BLOCK)}
		  {OutputStream putUInt(VECTOR)}
		  {OutputStream putUInt({Width X})}
		  continue(args(Id + 1 OutputStream X#Id|Seen)
			   {Record.foldR X
			    fun {$ Y Rest}
			       pickling(PicklingInterpreter Y)|Rest
			    end Rest})
	       [] closure then
		  {OutputStream putByte(BLOCK)}
		  {OutputStream putUInt(CLOSURE)}
		  {OutputStream putUInt({Width X})}
		  continue(args(Id + 1 OutputStream X#Id|Seen)
			   {Record.foldR X
			    fun {$ Y Rest}
			       pickling(PicklingInterpreter Y)|Rest
			    end Rest})
	       [] function then
		  continue(Args
			   pickling(PicklingInterpreter {X.1.abstract X})|Rest)
	       [] transform(X Y) then
		  {OutputStream putByte(TRANSFORM)}
		  continue(args(Id + 1 OutputStream X#Id|Seen)
			   pickling(PicklingInterpreter X)|
			   pickling(PicklingInterpreter Y)|Rest)
	       end
	    [] array then
	       {OutputStream putByte(BLOCK)}
	       {OutputStream putUInt(ARRAY)}
	       {OutputStream putUInt({Array.high X} + 1)}
	       continue(args(Id + 1 OutputStream X#Id|Seen)
			{ForThread {Array.high X} 0 ~1
			 fun {$ Rest I}
			    pickling(PicklingInterpreter X.I)|Rest
			 end Rest})
	    [] cell then
	       {OutputStream putByte(BLOCK)}
	       {OutputStream putUInt(CELL)}
	       {OutputStream putUInt(1)}
	       continue(args(Id + 1 OutputStream X#Id|Seen)
			pickling(PicklingInterpreter {Access X})|Rest)
	    end
	 elseof Id then
	    {OutputStream putByte(REF)}
	    {OutputStream putUInt(Id)}
	    continue(args(Id OutputStream Seen) Rest)
	 end
      end
   end

   PicklingInterpreter =
   picklingInterpreter(run: PicklingInterpreterRun
		       handle:
			  fun {$ Debug Exn TaskStack}
			     case TaskStack of Frame|Rest then
				exception(Frame|Debug Exn Rest)
			     end
			  end)

   fun {PicklePackInterpreterRun args(_ OutputStream _) TaskStack}
      case TaskStack of picklePack(_)|Rest then
	 continue(arg({OutputStream close($)}) Rest)
      end
   end

   PicklePackInterpreter =
   picklePackInterpreter(run: PicklePackInterpreterRun
			 handle:
			    fun {$ Debug Exn TaskStack}
			       case TaskStack of Frame|Rest then
				  exception(Frame|Debug Exn Rest)
			       end
			    end)

   fun {Pack X TaskStack}
      continue(args(0 {New StringOutputStream init()} nil)
	       pickling(PicklingInterpreter X)|
	       picklePack(PicklePackInterpreter)|TaskStack)
   end

   fun {PickleSaveInterpreterRun args(_ OutputStream _) TaskStack}
      case TaskStack of pickleSave(_)|Rest then
	 {OutputStream close()}
	 continue(args() Rest)
      end
   end

   PickleSaveInterpreter =
   pickleSaveInterpreter(run: PickleSaveInterpreterRun
			 handle:
			    fun {$ Debug Exn TaskStack}
			       case TaskStack of Frame|Rest then
				  exception(Frame|Debug Exn Rest)
			       end
			    end)

   fun {Save X Filename TaskStack}
      continue(args(0 {New FileOutputStream init(Filename)} nil)
	       pickling(PicklingInterpreter X)|
	       pickleSave(PickleSaveInterpreter)|TaskStack)
   end

   PickleComponent = tuple(Pickle)

   I_pack = 1
   I_save = 2

   Pickle =
   tuple(I_pack: Pack#i_t
	 I_save: Save#ir_t)
end
