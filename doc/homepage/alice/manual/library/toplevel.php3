<?php include("macros.php3"); ?>
<?php heading("The top-level environment", "The top-level environment") ?>

<?php section("synopsis", "synopsis") ?>

  <P>
    This section describes the top-level environment, i.e. all items that
    are available unqualified. It extends the
    <A href="http://www.dina.kvl.dk/~sestoft/sml/top-level-chapter.html">Standard ML
    Basis' top-level environment</A>.
  </P>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <P>
    The following table presents the top-level types and their defining
    structures. Items marked <TT>(*)</TT> are extensions to the Standard Basis.
  </P>

  <TT>
  <TABLE>
    <TR> <TD>type unit = {}</TD> <TD><A href="general.php3">General.unit</A></TD> </TR>
    <TR> <TD>eqtype int</TD> <TD><A href="integer.php3">Int.t</A></TD> </TR>
    <TR> <TD>eqtype word</TD> <TD><A href="word.php3">Word.t</A></TD> </TR>
    <TR> <TD>eqtype real</TD> <TD><A href="real.php3">Real.t</A></TD> </TR>
    <TR> <TD>eqtype char</TD> <TD><A href="char.php3">Char.t</A></TD> </TR>
    <TR> <TD>eqtype string</TD> <TD><A href="string.php3">String.t</A></TD> </TR>
    <TR> <TD>eqtype substring</TD> <TD><A href="substring.php3">Substring.t</A></TD> </TR>
    <TR> <TD>exttype exn</TD> <TD><A href="general.php3">General.exn</A></TD> </TR>
    <TR> <TD>eqtype 'a vector</TD> <TD><A href="vector.php3">Vector.t</A></TD> </TR>
    <TR> <TD>eqtype 'a array</TD> <TD><A href="array.php3">Array.t</A></TD> </TR>
    <TR> <TD>datatype 'a ref = ref of 'a</TD> <TD><A href="ref.php3">Ref.t</A></TD> </TR>
    <TR> <TD>datatype bool = false | true</TD> <TD><A href="bool.php3">Bool.t</A></TD> </TR>
    <TR> <TD>datatype order = LESS | EQUAL | GREATER</TD> <TD><A href="general.php3">General.order</A></TD> </TR>
    <TR> <TD>datatype 'a option = NONE | SOME of 'a</TD> <TD><A href="option.php3">Option.t</A></TD> </TR>
    <TR> <TD>datatype 'a list = nil | op:: of 'a * 'a list</TD> <TD><A href="list.php3">List.t</A></TD> </TR>
    <TR> <TD>datatype ('a,'b) alt = FST of 'a | SND of 'b</TD> <TD><A href="alt.php3">Alt.t</A></TD> <TD>(*)</TD> </TR>
    <TR> <TD>type ('a,'b) pair = 'a * 'b</TD> <TD><A href="pair.php3">Pair.t</A></TD> <TD>(*)</TD> </TR>
  </TABLE>
  </TT>

  <P>
    The global exceptions are the following:
  </P>

  <TT>
  <TABLE>
    <TR> <TD>exception Alt</TD> <TD><A href="alt.php3">Alt.Alt</A></TD> <TD>(*)</TD> </TR>
    <TR> <TD>exception Bind</TD> <TD><A href="general.php3">General.Bind</A></TD> </TR>
    <TR> <TD>exception Chr</TD> <TD><A href="general.php3">General.Chr</A></TD> </TR>
    <TR> <TD>exception Div</TD> <TD><A href="general.php3">General.Div</A></TD> </TR>
    <TR> <TD>exception Domain</TD> <TD><A href="general.php3">General.Domain</A></TD> </TR>
    <TR> <TD>exception Empty</TD> <TD><A href="list.php3">List.Empty</A></TD> </TR>
    <TR> <TD>exception Fail of string</TD> <TD><A href="general.php3">General.Fail</A></TD> </TR>
    <TR> <TD>exception Match</TD> <TD><A href="general.php3">General.Match</A></TD> </TR>
    <TR> <TD>exception Option</TD> <TD><A href="option.php3">Option.Option</A></TD> </TR>
    <TR> <TD>exception Overflow</TD> <TD><A href="general.php3">General.Overflow</A></TD> </TR>
    <TR> <TD>exception Size</TD> <TD><A href="general.php3">General.Size</A></TD> </TR>
    <TR> <TD>exception Span</TD> <TD><A href="general.php3">General.Span</A></TD> </TR>
    <TR> <TD>exception Subscript</TD> <TD><A href="general.php3">General.Subscript</A></TD> </TR>
  </TABLE>
  </TT>

  <P>
    The next table presents all non-overloaded functions:
  </P>

  <TT>
  <TABLE>
    <TR> <TD>val = : ''a * ''a -> ''a</TD> <TD></TT><I>primitive</I><TT></TD> </TR>
    <TR> <TD>val <> : ''a * ''a -> ''a</TD> <TD></TT><I>primitive</I><TT></TD> </TR>
    <TR> <TD>val real : int -> real</TD> <TD><A href="real.php3">Real.fromInt</A></TD> </TR>
    <TR> <TD>val ceil : real -> int</TD> <TD><A href="real.php3">Real.ceil</A></TD> </TR>
    <TR> <TD>val floor : real -> int</TD> <TD><A href="real.php3">Real.floor</A></TD> </TR>
    <TR> <TD>val trunc : real -> int</TD> <TD><A href="real.php3">Real.trunc</A></TD> </TR>
    <TR> <TD>val round : real -> int</TD> <TD><A href="real.php3">Real.round</A></TD> </TR>
    <TR> <TD>val ord : char -> int</TD> <TD><A href="char.php3">Char.ord</A></TD> </TR>
    <TR> <TD>val chr : int -> char</TD> <TD><A href="char.php3">Char.chr</A></TD> </TR>
    <TR> <TD>val str : char -> string</TD> <TD><A href="string.php3">String.str</A></TD> </TR>
    <TR> <TD>val size : char -> string</TD> <TD><A href="string.php3">String.size</A></TD> </TR>
    <TR> <TD>val ^ : char -> string</TD> <TD><A href="string.php3">String.^</A></TD> </TR>
    <TR> <TD>val concat : char -> string</TD> <TD><A href="string.php3">String.concat</A></TD> </TR>
    <TR> <TD>val explode : char -> string</TD> <TD><A href="string.php3">String.explode</A></TD> </TR>
    <TR> <TD>val implode : char -> string</TD> <TD><A href="string.php3">String.implode</A></TD> </TR>
    <TR> <TD>val substring : char -> string</TD> <TD><A href="string.php3">String.substring</A></TD> </TR>
    <TR> <TD>val vector : 'a list -> 'a vector</TD> <TD><A href="vector.php3">Vector.fromList</A></TD> </TR>
    <TR> <TD>val ! : 'a ref -> 'a</TD> <TD><A href="ref.php3">Ref.!</A></TD> </TR>
    <TR> <TD>val := : 'a ref * 'a -> unit</TD> <TD><A href="ref.php3">Ref.:=</A></TD> </TR>
    <TR> <TD>val :=: : 'a ref * 'a ref -> unit</TD> <TD><A href="ref.php3">Ref.:=:</A></TD> </TR>
    <TR> <TD>val exnName : exn -> string</TD> <TD><A href="general.php3">General.exnName</A></TD> </TR>
    <TR> <TD>val exnMessage : exn -> string</TD> <TD><A href="general.php3">General.exnName</A></TD> </TR>
    <TR> <TD>val not : bool -> bool</TD> <TD><A href="bool.php3">Bool.not</A></TD> </TR>
    <TR> <TD>val inverse : order -> order</TD> <TD><A href="general.php3">General.inverse</A></TD> <TD>(*)</TD> </TR>
    <TR> <TD>val isSome : 'a option -> bool</TD> <TD><A href="option.php3">Option.isSome</A></TD> </TR>
    <TR> <TD>val isNone : 'a option -> bool</TD> <TD><A href="option.php3">Option.isNone</A></TD> <TD>(*)</TD> </TR>
    <TR> <TD>val valOf : 'a option -> 'a</TD> <TD><A href="option.php3">Option.valOf</A></TD> </TR>
    <TR> <TD>val getOpt : 'a option * 'a -> 'a</TD> <TD><A href="option.php3">Option.getOpt</A></TD> </TR>
    <TR> <TD>val fst : ('a,'b) alt -> 'a</TD> <TD><A href="alt.php3">Alt.fst</A></TD> <TD>(*)</TD> </TR>
    <TR> <TD>val snd : ('a,'b) alt -> 'b</TD> <TD><A href="alt.php3">Alt.snd</A></TD> <TD>(*)</TD> </TR>
    <TR> <TD>val isFst : ('a,'b) alt -> bool</TD> <TD><A href="alt.php3">Alt.isFst</A></TD> <TD>(*)</TD> </TR>
    <TR> <TD>val isSnd : ('a,'b) alt -> bool</TD> <TD><A href="alt.php3">Alt.isSnd</A></TD> <TD>(*)</TD> </TR>
    <TR> <TD>val null : 'a list -> bool</TD> <TD><A href="list.php3">List.null</A></TD> </TR>
    <TR> <TD>val hd : 'a list -> 'a</TD> <TD><A href="list.php3">List.hd</A></TD> </TR>
    <TR> <TD>val tl : 'a list -> 'a list</TD> <TD><A href="list.php3">List.tl</A></TD> </TR>
    <TR> <TD>val length : 'a list -> int</TD> <TD><A href="list.php3">List.length</A></TD> </TR>
    <TR> <TD>val rev : 'a list -> 'a list</TD> <TD><A href="list.php3">List.rev</A></TD> </TR>
    <TR> <TD>val @ : 'a list * 'a list -> 'a list</TD> <TD><A href="list.php3">List.@</A></TD> </TR>
    <TR> <TD>val app : ('a -> unit) -> 'a list -> unit</TD> <TD><A href="list.php3">List.app</A></TD> </TR>
    <TR> <TD>val appr : ('a -> unit) -> 'a list -> unit</TD> <TD><A href="list.php3">List.apr</A></TD> </TR>
    <TR> <TD>val map : ('a -> 'b) -> 'a list -> 'b list</TD> <TD><A href="list.php3">List.map</A></TD> </TR>
    <TR> <TD>val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b</TD> <TD><A href="list.php3">List.foldl</A></TD> </TR>
    <TR> <TD>val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b</TD> <TD><A href="list.php3">List.foldr</A></TD> </TR>
    <TR> <TD>val ignore : 'a -> unit</TD> <TD><A href="general.php3">General.ignore</A></TD> </TR>
    <TR> <TD>val before : 'a * unit -> 'a</TD> <TD><A href="general.php3">General.before</A></TD> </TR>
    <TR> <TD>val id : 'a -> 'a</TD> <TD><A href="general.php3">General.id</A></TD> <TD>(*)</TD> </TR>
    <TR> <TD>val const : 'a -> 'b -> 'a</TD> <TD><A href="general.php3">General.const</A></TD> <TD>(*)</TD> </TR>
    <TR> <TD>val flip : ('a * 'b -> 'c) -> ('b * 'a -> 'c)</TD> <TD><A href="general.php3">General.flip</A></TD> <TD>(*)</TD> </TR>
    <TR> <TD>val curry : ('a * 'b -> 'c) -> ('a -> 'b -> 'c)</TD> <TD><A href="general.php3">General.curry</A></TD> <TD>(*)</TD> </TR>
    <TR> <TD>val uncurry : ('a -> 'b -> 'g) -> ('a * 'b -> 'c)</TD> <TD><A href="general.php3">General.uncurry</A></TD> <TD>(*)</TD> </TR>
    <TR> <TD>val o : ('a -> 'b) -> ('c -> 'a) -> ('c -> 'b)</TD> <TD><A href="general.php3">General.o</A></TD> </TR>
    <TR> <TD>val use : string -> unit</TD> <TD></TT><I>primitive</I><TT></TD> </TR>
  </TABLE>
  </TT>

  <P>
    The <TT>use</TT> function is only available in the <A href="../usage.php3#interactive">interactive toplevel</A>.
  </P>

  <P>
    All arithmetic functions are overloaded:
  </P>

  <TT>
  <TABLE>
    <TR> <TD>val ~ : <I>num</I> -> <I>num</I></TD> </TR>
    <TR> <TD>val abs : <I>num</I> -> <I>num</I></TD> </TR>
    <TR> <TD>val + : <I>num</I> * <I>num</I> -> <I>num</I></TD> </TR>
    <TR> <TD>val - : <I>num</I> * <I>num</I> -> <I>num</I></TD> </TR>
    <TR> <TD>val * : <I>num</I> * <I>num</I> -> <I>num</I></TD> </TR>
    <TR> <TD>val div : <I>wordint</I> * <I>wordint</I> -> <I>wordint</I></TD> </TR>
    <TR> <TD>val mod : <I>wordint</I> * <I>wordint</I> -> <I>wordint</I></TD> </TR>
    <TR> <TD>val / : <I>real</I> * <I>real</I> -> <I>real</I></TD> </TR>
    <TR> <TD>val < : <I>numtext</I> * <I>numtext</I> -> bool</TD> </TR>
    <TR> <TD>val > : <I>numtext</I> * <I>numtext</I> -> bool</TD> </TR>
    <TR> <TD>val <= : <I>numtext</I> * <I>numtext</I> -> bool</TD> </TR>
    <TR> <TD>val >= : <I>numtext</I> * <I>numtext</I> -> bool</TD> </TR>
  </TABLE>
  </TT>

  <P>
    <I>Limitations:</I> Currently, overloading is not supported, so that the overloading classes are
    defined trivially as follows:
  </P>

  <DL>
    <DD><TT><I>num</I>&nbsp;&nbsp;&nbsp;&nbsp;</TT> := <TT>int</TT></DD>
    <DD><TT><I>wordint</I></TT> := <TT>int</TT></DD>
    <DD><TT><I>real</I>&nbsp;&nbsp;&nbsp;</TT> := <TT>real</TT></DD>
    <DD><TT><I>numtext</I></TT> := <TT>int</TT></DD>
  </DL>

  <P>
    The initial infix declarations are as follows:
  </P>

  <PRE>
    infix  7  * / div mod
    infix  6  + - ^
    infixr 5  :: @
    infix  4  = <> > >= < <=
    infix  3  := :=: o
    infix  0  before
  </PRE>

<?php footing() ?>
