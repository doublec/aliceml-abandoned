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
   POSINT    = 0
   NEGINT    = 1
   CHUNK     = 2
   BLOCK     = 3
   CLOSURE   = 4
   REF       = 5
   TRANSFORM = 6

%   AlicePrimitive = {ByteString.make 'Alice.primitive'}
%   AliceFunction  = {ByteString.make 'Alice.function'}

%   fun {ApplyTransforms X}
%      %--** recurse
%      case X of 'TRANSFORM'('Alice.primitive' tag(0 Name)) then
%	 Primitives.table.Name
%      [] 'TRANSFORM'('Alice.function' tag(!Function NG NL IdDefArgs Instr))
%      then function(NG NL IdDefArgs Instr)
%      else X
%      end
%   end

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
	 @Dict.N = X
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
	 Chunk = {ByteString.make for _ in 1..Size collect: C do
				     {C PickleParser, Next($)}
				  end}
	 PickleParser, Remember(Chunk)
      end
      meth ParseBlock(?T) Label Size in
	 PickleParser, Remember(T)
	 PickleParser, ParseUInt(?Label)
	 PickleParser, ParseUInt(?Size)
	 T = {MakeTuple block Size + 1}
	 T.1 = Label
	 for I in 2..Size + 1 do
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
	 Transform = transform(F X)
	 PickleParser, ParsePickle(?F)
	 PickleParser, ParsePickle(?X)
      end
      meth ParseReference($)
	 PickleParser, Lookup(PickleParser, ParseUInt($) $)
      end
   end

   fun {Unpickle File}
      {New PickleParser init({ReadFile File} $) _}
   end
end
