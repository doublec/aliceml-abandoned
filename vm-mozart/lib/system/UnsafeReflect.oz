%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2001-2003
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   Pickle(pack)
export
   'UnsafeReflect$': Reflect
define
   local
      fun {UnmarshalNumber S ?Rest}
	 case S of C|Cr andthen C >= 0x80 then X in
	    X = {UnmarshalNumber Cr ?Rest}
	    X * 0x80 + C - 0x80
	 [] C|Cr then Rest = Cr C
	 end
      end

      proc {IntToVector I ?A ?B ?C ?D}
	 A = I div 0x1000000
	 B = (I div 0x10000) mod 0x100
	 C = (I div 0x100) mod 0x100
	 D = I mod 0x100
      end
   in
      fun {RealToVector F}
	 case {ByteString.toString {Pickle.pack F}}
	 of 3|&3|&#|&2|3|Rest then A B C D E F G H Inter in
	    {IntToVector {UnmarshalNumber Rest ?Inter} ?E ?F ?G ?H}
	    {IntToVector {UnmarshalNumber Inter _} ?A ?B ?C ?D}
	    {ByteString.make [A B C D E F G H]}
	 end
      end
   end

   Reflect =
   'Reflect'('cast': fun {$ A} A end
	     'realToVector': RealToVector
	     'Reflect$':
		fun {$ '#'('$S$': _ 'X$': X)} '#'('x': X) end
	     'Reify$':
		fun {$ '#'('x': X '$S$': _)} X end
	     'ReflectSig$':
		fun {$ '#'('$S$': S)} '#'('x': S) end
	     'ReifySig$':
		fun {$ '#'('x': X)} '#'('$S$': X) end)
end
