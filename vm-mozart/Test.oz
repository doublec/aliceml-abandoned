declare [Word] =
{Module.link ['/home/kornstae/stockhausen/vm-mozart/Word.so{native}']}

{Wait Word}

{Browse Word}

{Show {Word.make 2 2}}   % <word2 0w2>

{Show {Word.make 0 2}}   % exception

{Show {Word.make 2 2} == {Word.make 2 2}}   % true

{Show {Word.make 2 2} == {Word.make 2 3}}   % false

{Show {Word.make 2 2} == {Word.make 3 2}}   % false

{Show {Word.toInt {Word.make 2 2}}}   % 2

{Show {Word.toInt {Word.make 32 4000000000}}}

{Show {Word.toIntX {Word.make 4 9}}}   % ~7

{Show {Word.toIntX {Word.make 32 4000000000}}}

{System.showInfo {Value.toVirtualString {Word.make 1 2} 1 1}}   % <word1 0w0>

{Show {Word.'<<' {Word.make 4 10} 1}}   % <word4 0w4>

{Show {Word.'>>' {Word.make 4 10} 1}}   % <word4 0w5>

{Show {Word.'~>>' {Word.make 4 10} 1}}   % <word4 0w13>

{Show {Word.orb {Word.make 4 3} {Word.make 4 8}}}   % <word4 0w11>

{Show {Word.andb {Word.make 4 3} {Word.make 4 9}}}   % <word4 0w1>

{Show {Word.xorb {Word.make 4 3} {Word.make 4 9}}}   % <word4 0w10>

{Show {Word.notb {Word.make 4 3}}}   % <word4 0w12>
