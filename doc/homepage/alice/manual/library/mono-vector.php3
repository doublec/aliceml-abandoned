<?php include("macros.php3"); ?>
<?php heading("The <TT>MONO_VECTOR</TT> signature", "The <TT>MONO_VECTOR</TT> signature") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature MONO_VECTOR
    structure CharVector : MONO_VECTOR where type elem = char
    structure Word8Vector : MONO_VECTOR where type elem = Word8.word
  </PRE>

  <P>
    An extended version of the
    <A href="http://www.dina.kvl.dk/~sestoft/sml/mono-vector.html">Standard ML
    Basis' <TT>MONO_VECTOR</TT> </A> signature.
  </P>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <PRE>
    signature VECTOR =
    sig
	eqtype vector
	type   t = vector

	val maxLen :      int

	val toList :      vector -> elem list
	val fromList :    elem list -> vector
	val tabulate :    int * (int -> elem) -> vector

	val length :      vector -> int
	val sub :         vector * int -> elem
	val replace :     vector * int * elem -> vector
	val extract :     vector * int * int option -> vector
	val append :      vector * vector -> vector
	val concat :      vector list -> vector
	val rev :         vector -> vector

	val app :         (elem -> unit) -> vector -> unit
	val appr :        (elem -> unit) -> vector -> unit
	val map :         (elem -> 'a) -> vector -> 'a vector
	val foldl :       (elem * 'a -> 'a) -> 'a -> vector -> 'a
	val foldr :       (elem * 'a -> 'a) -> 'a -> vector -> 'a
	val appi :        (int * elem -> unit) -> vector * int * int option -> unit
	val appri :       (int * elem -> unit) -> vector * int * int option -> unit
	val mapi :        (int * elem -> 'a) -> vector * int * int option -> 'a vector
	val foldli :      (int * elem * 'a -> 'a) -> 'a -> vector * int * int option -> 'a
	val foldri :      (int * elem * 'a -> 'a) -> 'a -> vector * int * int option -> 'a

	val all :         (elem -> bool) -> vector -> bool
	val exists :      (elem -> bool) -> vector -> bool
	val find :        (elem -> bool) -> vector -> elem option
	val contains :    'vector -> 'elem -> bool
	val notContains : 'vector -> 'elem -> bool

	val equal :       (elem * elem -> bool) -> vector * vector -> bool
	val compare :     (elem * elem -> order) -> vector * vector -> order

	val isSorted :    (elem * elem -> order) -> vector -> bool
	val sort :        (elem * elem -> order) -> vector -> vector
    end
  </PRE>

<?php section("description", "description") ?>

  <P>
    Items not described here are as in the 
    <A href="http://www.dina.kvl.dk/~sestoft/sml/mono-vector.html">Standard ML
    Basis' <TT>MONO_VECTOR</TT></A> signature.
  </P>

  <DL>
    <DT>
      <TT>type t = vector</TT>
    </DT>
    <DD>
      <P>A local synonym for type <TT>vector</TT>.</P>
    </DD>

    <DT>
      <TT>toList <I>vec</I></TT>
    </DT>
    <DD>
      <P>Creates a list of the elements of <TT><I>vec</I></TT> in order of
      increasing indices.</P>
    </DD>

    <DT>
      <TT>rev <I>vec</I></TT>
    </DT>
    <DD>
      <P>Returns a vector that contains the elements of <TT><I>vec</I></TT>
      in reverse order.</P>
    </DD>

    <DT>
      <TT>update (<I>vec</I>, <I>i</I>, <I>a</I>)</TT>
    </DT>
    <DD>
      <P>Returns a new vector, identical to <TT><I>vec</I></TT>, except the
      <I>i</I>th element is set to <TT><I>x</I></TT>. If <TT><I>i</I></TT> < 0
      or |<TT><I>vec</I></TT>| <= <TT><I>i</I></TT>, then the Subscript
       exception is raised.</P>
    </DD>

    <DT>
      <TT>append (<I>vec1</I>, <I>vec2</I>)</TT>
    </DT>
    <DD>
      <P>Returns the vector that is the concatenation of <TT><I>vec1</I></TT>
      and <TT><I>vec2</I></TT>. Raises <TT>Size</TT> if
      |<TT><I>vec1</I></TT>| + |<TT><I>vec2</I></TT>| > <TT>maxLen</TT>.
      Equivalent to <TT>concat [<I>vec1</I>, <I>vec2</I>]</TT>.</P>
    </DD>

    <DT>
      <TT>appri <I>f</I> <I>slice</I></TT> <BR>
      <TT>appr <I>f</I> <I>vec</I></TT>
    </DT>
    <DD>
      <P>Like <TT>appi</TT> and <TT>app</TT>, but apply <TT><I>f</I></TT> in
      right to left order (i.e., decreasing indices). The expression
      <TT>app <I>f vec</I></TT> is equivalent to: 
      <PRE>
        appri (f o #2) (vec, 0, NONE)</PRE>
    </DD>

    <DT>
      <TT>exists <I>f</I> <I>vec</I></TT>
    </DT>
    <DD>
      <P>Applies <TT><I>f</I></TT> to each element <TT><I>x</I></TT> of the
      vector <TT><I>vec</I></TT>, from left to right, until <TT><I>f x</I></TT>
      evaluates to <TT>true</TT>; returns <TT>true</TT> if such an
      <TT><I>x</I></TT> exists and <TT>false</TT> otherwise.</P>
    </DD>

    <DT>
      <TT>all <I>f</I> <I>vec</I></TT>
    </DT>
    <DD>
      <P>Applies <TT><I>f</I></TT> to each element <TT><I>x</I></TT> of the
      vector <TT><I>vec</I></TT>, from left to right, until <TT><I>f x</I></TT>
      evaluates to <TT>false</TT>; returns <TT>false</TT> if such an
      <TT><I>x</I></TT> exists and <TT>true</TT> otherwise.
      Equivalent to <TT>not(exists (not o <I>f</I>) <I>l</I>))</TT>.</P>
    </DD>

    <DT>
      <TT>find <I>f</I> <I>vec</I></TT>
    </DT>
    <DD>
      <P>Applies <TT><I>f</I></TT> to each element <TT><I>x</I></TT> of the
      vector <TT><I>vec</I></TT>, from left to right, until <TT><I>f x</I></TT>
      evaluates to <TT>true</TT>; returns <TT>SOME <I>x</I></TT> if such an
      <TT><I>x</I></TT> exists and <TT>NONE</TT> otherwise.</P>
    </DD>

    <DT>
      <TT>contains <I>vec</I> <I>a</I></TT>
    </DT>
    <DD>
      <P>Returns <TT>true</TT> if the element <TT><I>a</I></TT> occurs in the
      vector <TT><I>vec</I></TT>; otherwise <TT>false</TT>.</P>
    </DD>

    <DT>
      <TT>notContains <I>vec</I> <I>a</I></TT>
    </DT>
    <DD>
      <P>Returns <TT>true</TT> if the element <TT><I>a</I></TT> does not occur in the
      vector <TT><I>vec</I></TT>; otherwise <TT>false</TT>.
      Equivalent to <TT>not(contains <I>vec a</I>)</TT>.</P>
    </DD>

    <DT>
      <TT>equal <I>equal'</I> (<I>vec1</I>, <I>vec2</I>)</TT>
    </DT>
    <DD>
      <P>Creates an equality function on vectors given an equality on the
      element type.</P>
    </DD>

    <DT>
      <TT>compare <I>compare'</I> (<I>vec1</I>, <I>vec2</I>)</TT>
    </DT>
    <DD>
      <P>Creates an ordering function on vectors, given a suitable ordering functions
      for its element type. The order induced is lexicographic.</P>
    </DD>

    <DT>
      <TT>isSorted <I>f</I> <I>vec</I></TT>
    </DT>
    <DD>
      <P>Returns <TT>true</TT> iff <TT><I>vec</I></TT> is sorted with respect
      to the ordering function <TT><I>f</I></TT>.</P>
    </DD>

    <DT>
      <TT>sort <I>f</I> <I>vec</I></TT>
    </DT>
    <DD>
      <P>Returns a new vector that contains the same elements as
      <TT><I>vec</I></TT>, but sorted with respect to the ordering function
      <TT><I>f</I></TT>. Sorting may be unstable with respect to equal
      elements.</P>
    </DD>

<?php section("also", "see also") ?>

  <DL><DD>
    <A href="vector.php3"><TT>Vector</TT></A>,
    <A href="mono-array.php3"><TT>MONO_ARRAY</TT></A>
  </DD></DL>

<?php footing() ?>
