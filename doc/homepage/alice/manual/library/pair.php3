<?php include("macros.php3"); ?>
<?php heading("The <TT>Pair</TT> structure", "The <TT>Pair</TT> structure") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature PAIR
    structure Pair : PAIR
  </PRE>

  <P>
    Common combinators on type <TT>'a * 'b</TT>.
  </P>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <PRE>
    signature PAIR =
    sig
	type ('a,'b) pair = 'a * 'b
	type ('a,'b) t    = ('a,'b) pair

	val fst :     ('a,'b) pair -> 'a
	val snd :     ('a,'b) pair -> 'b

	val app :     ('a -> unit) * ('b -> unit) -> ('a,'b) pair -> unit
	val appFst :  ('a -> unit) -> ('a,'b) pair -> unit
	val appSnd :  ('b -> unit) -> ('a,'b) pair -> unit
	val map :     ('a -> 'c) * ('b -> 'd) -> ('a,'b) pair -> ('c,'d) pair
	val mapFst :  ('a -> 'c) -> ('a,'b) pair -> ('c,'b) pair
	val mapSnd :  ('b -> 'c) -> ('a,'b) pair -> ('a,'c) pair

	val equal :   ('a * 'a -> bool) * ('b * 'b -> bool) -> ('a,'b) pair * ('a,'b) pair -> bool
	val compare : ('a * 'a -> order) * ('b * 'b -> order) -> ('a,'b) pair * ('a,'b) pair -> order
    end
  </PRE>

<?php section("description", "description") ?>

  <DL>
    <DT>
      <TT>type ('a,'b) pair = 'a * 'b</TT> <BR>
      <TT>type t = pair</TT>
    </DT>
    <DD>
      <P>The type of cartesian products.</P>
    </DD>

    <DT>
      <TT>fst <I>p</I></TT> <BR>
      <TT>snd <I>p</I></TT>
    </DT>
    <DD>
      <P>Returns the first (second) component of the pair <TT><I>p</I></TT>.</P>
    </DD>

    <DT>
      <TT>app (<I>f</I>,<I>g</I>) <I>p</I></TT> <BR>
      <TT>appFst <I>f</I> <I>p</I></TT> <BR>
      <TT>appSnd <I>g</I> <I>p</I></TT>
    </DT>
    <DD>
      <P>The function <TT>app</TT> applies the functions <TT><I>f</I></TT> and
      <TT><I>g</I></TT> to the first and second component of the pair
      <TT><I>p</I></TT>, respectively. For the functions <TT>appFst</TT> and
      <TT>appSnd</TT> the following equivalences hold:</P>
      <PRE>
	appFst f p = app (f, ignore) p
	appSnd g p = app (ignore, g) p</PRE>
    </DD>

    <DT>
      <TT>map (<I>f</I>,<I>g</I>) <I>p</I></TT> <BR>
      <TT>mapFst <I>f</I> <I>p</I></TT> <BR>
      <TT>mapSnd <I>g</I> <I>p</I></TT>
    </DT>
    <DD>
      <P>The function <TT>map</TT> produces a pair by mapping the functions
      <TT><I>f</I></TT> and <TT><I>g</I></TT> on both components of
      <TT><I>p</I></TT>.
      For the functions <TT>mapFst</TT> and <TT>mapSnd</TT> the following
      equivalences hold:</P>
      <PRE>
	mapFst f p = map (f, id) p
	mapSnd g p = map (id, g) p</PRE>
      <P>where <TT>id</TT> is the identity function.</P>
    </DD>

    <DT>
      <TT>equal (<I>equalFst</I>, <I>equalSnd</I>) (<I>p1</I>, <I>p2</I>)</TT>
    </DT>
    <DD>
      <P>Creates an equality function on pairs, given suitable equality functions
      for each component type.</P>
    </DD>

    <DT>
      <TT>compare (<I>compareFst</I>, <I>compareSnd</I>) (<I>p1</I>, <I>p2</I>)</TT>
    </DT>
    <DD>
      <P>Creates an ordering function on pairs, given suitable ordering functions
      for each component type. The order induced is the lexicographic one.</P>
    </DD>
  </DL>

<?php section("also", "see also") ?>

  <DL><DD>
    <A href="alt.php3">Alt</A>
  </DD></DL>

<?php footing() ?>
