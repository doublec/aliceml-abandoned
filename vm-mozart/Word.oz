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
   NativeWord at '../../vm-mozart/Word.so{native}'
export
   'Word': Word
   'LargeWord': LargeWord
define
   Word =
   'Word'(
      'Word.fromInt\'': fun {$ N#X} {NativeWord.make N X} end
      'Word.toInt': NativeWord.toInt
      'Word.toIntX': NativeWord.toIntX
      'Word.orb': fun {$ X#Y} {NativeWord.orb X Y} end
      'Word.xorb': fun {$ X#Y} {NativeWord.xorb X Y} end
      'Word.andb': fun {$ X#Y} {NativeWord.andb X Y} end
      'Word.notb': NativeWord.notb
      'Word.<<': fun {$ X#Y} {NativeWord.'<<' X Y} end
      'Word.>>': fun {$ X#Y} {NativeWord.'>>' X Y} end
      'Word.~>>': fun {$ X#Y} {NativeWord.'~>>' X Y} end)

   LargeWord = Word
end
