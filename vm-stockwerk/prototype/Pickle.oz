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
   BootName(newUnique: NewUniqueName) at 'x-oz://boot/Name'
   Open(file)
   System(eq)
   PrimitiveTable(values functions)
   AbstractCodeInterpreter(makeConcreteCode)
require
   Helper(deref: Deref)
export
   Unpack
   Load
   Pack
   Save
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
   CON_VAL      = 2
   VECTOR       = 3
   LabelOffset  = 4

   IoException = {NewUniqueName 'IO.Io'}
   CorruptException = {NewUniqueName 'Component.Corrupt'}

   I_cause    = 2
   I_function = 3
   I_name     = 4

   %%-------------------------------------------------------------------
   %% Unpickling
   %%-------------------------------------------------------------------

   fun {ApplyTransform F X}
      %--** registration of transformers
      %--** implement using byneeds
      case F of 'Alice.primitive.value' then
	 case X of tag(0 Name) then
	    PrimitiveTable.values.{VirtualString.toAtom Name}
	 end
      [] 'Alice.primitive.function' then
	 case X of tag(0 Name) then
	    PrimitiveTable.functions.{VirtualString.toAtom Name}
	 end
      [] 'Alice.function' then
	 {AbstractCodeInterpreter.makeConcreteCode X}
      end
   end

   class InputStreamBase
      attr BufferHd: unit BufferTl: unit BufferPtr: unit
      meth init() Empty in
	 BufferHd <- Empty
	 BufferTl <- Empty
	 BufferPtr <- Empty
      end
      meth getByte($)
	 if {IsFree @BufferPtr} then eob
	 elsecase @BufferPtr of C|Cr then
	    BufferPtr <- Cr
	    C
	 end
      end
      meth getBytes(N $)
	 if N == 0 then nil
	 elsecase InputStreamBase, getByte($) of eob then eob
	 elseof B then
	    case InputStreamBase, getBytes(N - 1 $) of eob then eob
	    elseof Bs then B|Bs
	    end
	 end
      end
      meth getUInt($)
	 InputStreamBase, GetUInt(0 1 $)
      end
      meth GetUInt(X N $)
	 case InputStreamBase, getByte($) of eob then eob
	 elseof B then
	    if B >= 0x80 then
	       InputStreamBase, GetUInt(X + (B - 0x80) * N N * 0x80 $)
	    else X + B * N
	    end
	 end
      end
      meth commit()
	 BufferHd <- @BufferPtr
      end
      meth appendToBuffer(Cs) NewTl in
	 @BufferTl = {Append Cs NewTl}
	 BufferTl <- NewTl
	 BufferPtr <- @BufferHd
      end
   end

   class FileInputStream from InputStreamBase
      attr File
      meth init(Name)
	 InputStreamBase, init()
	 File <- {New Open.file init(name: Name flags: [read])}
      end
      meth fillBuffer(Args TaskStack $)
	 %--** blocking
	 case {@File read(list: $)} of nil then
	    exception(nil CorruptException TaskStack.2)
	 elseof S then
	    InputStreamBase, appendToBuffer(S)
	    continue(Args TaskStack.2)
	 end
      end
      meth close()
	 {@File close()}
      end
   end

   class StringInputStream from InputStreamBase
      attr String
      meth init(S)
	 String <- S
      end
      meth fillBuffer(Args TaskStack $)
	 case @String of unit then
	    exception(nil CorruptException TaskStack.2)
	 elseof S then
	    InputStreamBase, appendToBuffer({ByteString.toString S})
	    String <- unit
	    continue(Args TaskStack.2)
	 end
      end
   end

   InputInterpreter =
   inputInterpreter(
      run:
	 fun {$ Args=args(InputStream _ _) TaskStack}
	    {InputStream fillBuffer(Args TaskStack $)}
	 end
      handle:
	 fun {$ Debug Exn Frame|Rest}
	    exception(Frame|Debug Exn Rest)
	 end
      toString: fun {$ _} 'Fill Unpickling Buffer' end)

   TransformInterpreter =
   transformInterpreter(
      run:
	 fun {$ Args transform(_ transient(TransientState) tuple(F X))|Rest}
	    {Assign TransientState
	     ref({ApplyTransform {VirtualString.toAtom F} X})}
	    continue(Args Rest)
	 end
      handle:
	 fun {$ Debug Exn Frame|Rest}
	    exception(Frame|Debug Exn Rest)
	 end
      toString: fun {$ _} 'Apply Transform' end)

   proc {Set X I Y}
      case {Value.type X} of array then
	 if I =< {Array.high X} then X.I := Y end
      [] cell then {Assign X Y}
      [] name then skip   %--**
      [] tuple then
	 case {Label X} of con then X.(I + 1) = Y
	 [] vector then
	    if I < {Width X} then X.(I + 1) = Y end
	 [] tag then X.(I + 2) = Y
	 [] tuple then X.(I + 1) = Y
	 [] closure then X.(I + 1) = Y
	 end
      end
   end

   fun {PushUnpickleFrame X I N Rest}
      if I == N then Rest
      else unpickling(UnpickleInterpreter X I N)|Rest
      end
   end

   fun {UnpickleInterpreterRun Args=args(InputStream Env Count) TaskStack}
      case TaskStack of unpickling(_ X I N)|Rest then
	 if I == N then continue(Args Rest)
	 elsecase {InputStream getByte($)} of eob then
	    continue(Args input(InputInterpreter)|TaskStack)
	 [] !POSINT then
	    case {InputStream getUInt($)} of eob then
	       continue(Args input(InputInterpreter)|TaskStack)
	    elseof Y then
	       {Set X I Y}
	       {InputStream commit()}
	       continue(Args {PushUnpickleFrame X I + 1 N Rest})
	    end
	 [] !NEGINT then
	    case {InputStream getUInt($)} of eob then
	       continue(Args input(InputInterpreter)|TaskStack)
	    elseof Y then
	       {Set X I ~(Y + 1)}
	       {InputStream commit()}
	       continue(Args {PushUnpickleFrame X I + 1 N Rest})
	    end
	 [] !CHUNK then
	    case {InputStream getUInt($)} of eob then
	       continue(Args input(InputInterpreter)|TaskStack)
	    elseof Size then
	       case {InputStream getBytes(Size $)} of eob then
		  continue(Args input(InputInterpreter)|TaskStack)
	       elseof Bs then Y in
		  Y = {ByteString.make Bs}
		  {Set X I Y}
		  Env.Count := Y
		  {InputStream commit()}
		  continue(args(InputStream Env Count + 1)
			   {PushUnpickleFrame X I + 1 N Rest})
	       end
	    end
	 [] !BLOCK then
	    case {InputStream getUInt($)} of eob then
	       continue(Args input(InputInterpreter)|TaskStack)
	    elseof Label then
	       case {InputStream getUInt($)} of eob then
		  continue(Args input(InputInterpreter)|TaskStack)
	       elseof Size then
		  case Label of !ARRAY then
		     case {InputStream getByte($)} of eob then
			continue(Args input(InputInterpreter)|TaskStack)
		     [] !POSINT then
			case {InputStream getUInt($)} of eob then
			   continue(Args input(InputInterpreter)|TaskStack)
			elseof RealSize then Y in
			   Y = {NewArray 0 RealSize - 1 unit}
			   {Set X I Y}
			   Env.Count := Y
			   {InputStream commit()}
			   continue(args(InputStream Env Count + 1)
				    unpickling(UnpickleInterpreter Y 0
					       Size - 1)|
				    {PushUnpickleFrame X I + 1 N Rest})
			end
		     end
		  [] !CELL then Y in
		     Y = {NewCell unit}
		     {Set X I Y}
		     Env.Count := Y
		     {InputStream commit()}
		     continue(args(InputStream Env Count + 1)
			      unpickling(UnpickleInterpreter Y 0 1)|
			      {PushUnpickleFrame X I + 1 N Rest})
		  [] !CON_VAL then Y in
		     Y = {MakeTuple con Size}
		     {Set X I Y}
		     Env.Count := Y
		     {InputStream commit()}
		     continue(args(InputStream Env Count + 1)
			      unpickling(UnpickleInterpreter Y 0 Size)|
			      {PushUnpickleFrame X I + 1 N Rest})
		  [] !VECTOR then
		     case {InputStream getByte($)} of eob then
			continue(Args input(InputInterpreter)|TaskStack)
		     [] !POSINT then
			case {InputStream getUInt($)} of eob then
			   continue(Args input(InputInterpreter)|TaskStack)
			elseof RealSize then Y in
			   Y = {MakeTuple vector RealSize}
			   {Set X I Y}
			   Env.Count := Y
			   {InputStream commit()}
			   continue(args(InputStream Env Count + 1)
				    unpickling(UnpickleInterpreter Y 0
					       Size - 1)|
				    {PushUnpickleFrame X I + 1 N Rest})
			end
		     end
		  else Y in
		     Y = {MakeTuple tag Size + 1}
		     Y.1 = Label - LabelOffset
		     {Set X I Y}
		     Env.Count := Y
		     {InputStream commit()}
		     continue(args(InputStream Env Count + 1)
			      unpickling(UnpickleInterpreter Y 0 Size)|
			      {PushUnpickleFrame X I + 1 N Rest})
		  end
	       end
	    end
	 [] !TUPLE then
	    case {InputStream getUInt($)} of eob then
	       continue(Args input(InputInterpreter)|TaskStack)
	    elseof Size then Y in
	       Y = {MakeTuple tuple Size}
	       {Set X I Y}
	       Env.Count := Y
	       {InputStream commit()}
	       continue(args(InputStream Env Count + 1)
			unpickling(UnpickleInterpreter Y 0 Size)|
			{PushUnpickleFrame X I + 1 N Rest})
	    end
	 [] !CLOSURE then
	    case {InputStream getUInt($)} of eob then
	       continue(Args input(InputInterpreter)|TaskStack)
	    elseof Size then Y in
	       Y = {MakeTuple closure Size}
	       {Set X I Y}
	       Env.Count := Y
	       {InputStream commit()}
	       continue(args(InputStream Env Count + 1)
			unpickling(UnpickleInterpreter Y 0 Size)|
			{PushUnpickleFrame X I + 1 N Rest})
	    end
	 [] !TRANSFORM then Y Y2 in
	    Y = transient({NewCell hole(noFuture)})
	    {Set X I Y}
	    Env.Count := Y
	    {InputStream commit()}
	    Y2 = tuple(_ _)
	    continue(args(InputStream Env Count + 1)
		     unpickling(UnpickleInterpreter Y2 0 2)|
		     transform(TransformInterpreter Y Y2)|
		     {PushUnpickleFrame X I + 1 N Rest})
	 [] !REF then
	    case {InputStream getUInt($)} of eob then
	       continue(Args input(InputInterpreter)|TaskStack)
	    elseof Index then
	       {Set X I Env.Index}
	       {InputStream commit()}
	       continue(Args {PushUnpickleFrame X I + 1 N Rest})
	    end
	 end
      end
   end

   UnpickleInterpreter =
   unpickleInterpreter(
      run: UnpickleInterpreterRun
      handle:
	 fun {$ Debug Exn Frame|Rest}
	    exception(Frame|Debug Exn Rest)
	 end
      toString: fun {$ unpickling(_ _ I N)} 'Unpickling Task '#I#' of '#N end)

   PickleUnpackInterpreter =
   pickleUnpackInterpreter(
      run:
	 fun {$ _ pickleUnpack(_ X)|Rest}
	    continue(arg(X.1) Rest)
	 end
      handle:
	 fun {$ Debug Exn Frame|Rest}
	    exception(Frame|Debug Exn Rest)
	 end
      toString: fun {$ _} 'Pickle Unpack' end)

   fun {Unpack S TaskStack} X in
      X = tuple(_)
      continue(args({New StringInputStream init(S)} {NewDictionary} 0)
	       unpickling(UnpickleInterpreter X 0 1)|
	       pickleUnpack(PickleUnpackInterpreter X)|TaskStack.2)
   end

   PickleLoadInterpreter =
   pickleLoadInterpreter(
      run:
	 fun {$ args(InputStream _ _) pickleLoad(_ X)|Rest}
	    {InputStream close()}
	    continue(arg(X.1) Rest)
	 end
      handle:
	 fun {$ Debug Exn Frame|Rest}
	    exception(Frame|Debug Exn Rest)
	 end
      toString: fun {$ _} 'Pickle Load' end)

   fun {Load Filename TaskStack}
      try X in
	 X = tuple(_)
	 continue(args({New FileInputStream init(Filename)} {NewDictionary} 0)
		  unpickling(UnpickleInterpreter X 0 1)|
		  pickleLoad(PickleLoadInterpreter X)|TaskStack.2)
      catch E=system(os(os ...) ...) then
	 exception(nil con(IoException
			   I_name: Filename
			   I_function: {ByteString.make 'load'}
			   I_cause: E)   %--** cause not of type exn
		   TaskStack.2)
      end
   end

   %%-------------------------------------------------------------------
   %% Pickling
   %%-------------------------------------------------------------------

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
	    [] atom then
	       case X of tuple then
		  {OutputStream putByte(TUPLE)}
		  {OutputStream putUInt(0)}
	       [] vector then
		  {OutputStream putByte(BLOCK)}
		  {OutputStream putUInt(VECTOR)}
		  {OutputStream putUInt(1)}
		  {OutputStream putByte(POSINT)}
		  {OutputStream putUInt(0)}
	       end
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
		  {OutputStream putUInt({Width X} + 1)}
		  {OutputStream putByte(POSINT)}
		  {OutputStream putUInt({Width X})}
		  continue(args(Id + 1 OutputStream X#Id|Seen)
			   {Record.foldR X
			    fun {$ Y Rest}
			       pickling(PicklingInterpreter Y)|Rest
			    end Rest})
	       [] closure then
		  {OutputStream putByte(CLOSURE)}
		  {OutputStream putUInt({Width X})}
		  continue(args(Id + 1 OutputStream X#Id|Seen)
			   {Record.foldR X
			    fun {$ Y Rest}
			       pickling(PicklingInterpreter Y)|Rest
			    end Rest})
	       [] function then
		  continue(Args
			   pickling(PicklingInterpreter {X.1.abstract X})|Rest)
	       [] specialized then
		  continue(Args
			   pickling(PicklingInterpreter {X.1.abstract X})|Rest)
	       [] primitive then
		  continue(Args
			   pickling(PicklingInterpreter {X.1.abstract X})|Rest)
	       [] transform then
		  {OutputStream putByte(TRANSFORM)}
		  continue(args(Id + 1 OutputStream X#Id|Seen)
			   pickling(PicklingInterpreter X.1)|
			   pickling(PicklingInterpreter X.2)|Rest)
	       end
	    [] array then N = {Array.high X} + 1 in
	       {OutputStream putByte(BLOCK)}
	       {OutputStream putUInt(ARRAY)}
	       {OutputStream putUInt(N + 1)}
	       {OutputStream putByte(POSINT)}
	       {OutputStream putUInt(N)}
	       continue(args(Id + 1 OutputStream X#Id|Seen)
			{ForThread N - 1 0 ~1
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
	    continue(Args Rest)
	 end
      end
   end

   PicklingInterpreter =
   picklingInterpreter(run: PicklingInterpreterRun
		       handle:
			  fun {$ Debug Exn Frame|Rest}
			     exception(Frame|Debug Exn Rest)
			  end
		       toString: fun {$ _} 'Pickling Task' end)

   PicklePackInterpreter =
   picklePackInterpreter(
      run:
	 fun {$ args(_ OutputStream _) picklePack(_)|Rest}
	    continue(arg({OutputStream close($)}) Rest)
	 end
      handle:
	 fun {$ Debug Exn Frame|Rest}
	    exception(Frame|Debug Exn Rest)
	 end
      toString: fun {$ _} 'Pickle Pack' end)

   fun {Pack X TaskStack}
      continue(args(0 {New StringOutputStream init()} nil)
	       pickling(PicklingInterpreter X)|
	       picklePack(PicklePackInterpreter)|TaskStack.2)
   end

   PickleSaveInterpreter =
   pickleSaveInterpreter(
      run:
	 fun {$ args(_ OutputStream _) pickleSave(_)|Rest}
	    {OutputStream close()}
	    continue(args() Rest)
	 end
      handle:
	 fun {$ Debug Exn Frame|Rest}
	    exception(Frame|Debug Exn Rest)
	 end
      toString: fun {$ _} 'Pickle Save' end)

   fun {Save Filename X TaskStack}
      try
	 continue(args(0 {New FileOutputStream init(Filename)} nil)
		  pickling(PicklingInterpreter X)|
		  pickleSave(PickleSaveInterpreter)|TaskStack.2)
      catch E=system(os(os ...) ...) then
	 exception(nil con(IoException
			   I_name: Filename
			   I_function: {ByteString.make 'save'}
			   I_cause: E)   %--** cause not of type exn
		   TaskStack.2)
      end
   end

   PickleComponent = tuple(Pickle)

   I_pack = 1
   I_save = 2

   Pickle =
   tuple(I_pack: Pack#i_t
	 I_save: Save#ri_t)
end
