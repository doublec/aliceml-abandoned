%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 1999
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

functor
import
   NativeWord at 'Word.so{native}'
export
   '$BootWord': BootWord
define
   Hex = hex(&0 &1 &2 &3 &4 &5 &6 &7 &8 &9 &a &b &c &d &e &f)

   fun {ToHex X}
      if X > 15 then {ToHex X div 16} else '' end#Hex.(X mod 16)
   end

   BootWord =
   'BootWord'(
      'fromInt\'': fun {$ N X} {NativeWord.make N X} end
      'toInt': NativeWord.toInt
      'toIntX': NativeWord.toIntX
      '+': fun {$ X Y} {NativeWord.'+' X Y} end
      '-': fun {$ X Y} {NativeWord.'-' X Y} end
      '*': fun {$ X Y} {NativeWord.'*' X Y} end
      'mod': fun {$ X Y} {NativeWord.'mod' X Y} end
      'orb': fun {$ X Y} {NativeWord.orb X Y} end
      'xorb': fun {$ X Y} {NativeWord.xorb X Y} end
      'andb': fun {$ X Y} {NativeWord.andb X Y} end
      'notb': NativeWord.notb
      '<<': fun {$ X Y} {NativeWord.'<<' X Y} end
      '>>': fun {$ X Y} {NativeWord.'>>' X Y} end
      '~>>': fun {$ X Y} {NativeWord.'~>>' X Y} end
      'toString': fun {$ X} {ByteString.make {ToHex {NativeWord.toInt X}}} end)
end
