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
%% pickle    ::= int | chunk | block | closure | transform
%% int       ::= POSINT <uint> | NEGINT <uint>
%% chunk     ::= CHUNK size <byte>*size
%% size      ::= <uint>
%% block     ::= BLOCK label size field*size
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
   Primitives(table)
export
   Unpickle
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

   %% Block Labels:
   ARRAY        = 0
   CELL         = 1
   CONSTRUCTOR  = 2
   CON_VAL      = 3
   GLOBAL_STAMP = 4
   VECTOR       = 5
   LabelOffset  = 6

   fun {ApplyTransform F X}
%      thread
	 case F of 'Alice.primitive' then
	    case X of tag(0 Name) then
	       Primitives.table.{VirtualString.toAtom Name}
	    end
	 [] 'Alice.function' then
	    case X of tag(0 NG NL IdDefArgs Instr)
	    then function(NG NL IdDefArgs Instr)
	    end
	 end
%      end
   end

   proc {ReadFile File ?VS} F in
      F = {New Open.file init(name: File flags: [read])}
      {F read(list: ?VS size: all)}
      {F close()}
   end

   class PickleParser
      attr Bs Dict Counter
      meth init(S $)
	 Bs <- S
	 Dict <- {NewDictionary}
	 Counter <- 0
	 PickleParser, ParsePickle($)
      end
      meth Next($)
	 case @Bs of B|Br then
	    Bs <- Br
	    B
	 end
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
	 [] !NEGINT    then ~(PickleParser, ParseUInt($))
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
	 if B >= 0x80 then PickleParser, ParseUIntSub(B 0x80 $)
	 else B
	 end
      end
      meth ParseUIntSub(X N $) B in
	 PickleParser, Next(?B)
	 if B >= 0x80 then PickleParser, ParseUIntSub(X + B * N N * 0x80 $)
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
	    for I in RealSize+1..Size-1 do
	       PickleParser, ParsePickle(_)
	    end
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

   fun {Unpickle File}
      {New PickleParser init({ReadFile File} $) _}
   end
end
