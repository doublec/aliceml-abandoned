<?php include("macros.php3"); ?>
<?php heading("The <TT>MONO_ARRAY</TT> signature", "The <TT>MONO_ARRAY</TT> signature") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature MONO_ARRAY
    structure CharArray : MONO_ARRAY where type elem = char
    structure Word8Array : MONO_ARRAY where type elem = Word8.word
  </PRE>

  <P>
    An extended version of the
    <A href="http://www.dina.kvl.dk/~sestoft/sml/array.html">Standard ML
    Basis' <TT>MONO_ARRAY</TT> </A> signature.
  </P>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <PRE>
    signature MONO_ARRAY =
    sig
	structure Vector : MONO_VECTOR

	eqtype array
	type t = array
	type elem = Vector.elem

	val maxLen :     int

	val array :      int * elem -> array
	val new :        int * elem -> array
	val fromList :   elem list -> array
	val toList :     array -> elem list
	val fromVector : Vector.vector -> array
	val toVector :   array -> Vector.vector
	val tabulate :   int * (int -> elem) -> array

	val length :     array -> int
	val sub :        array * int -> elem
	val update :     array * int * elem -> unit
	val swap :       array * int * int -> unit
	val rev :        array -> unit
	val extract :    array * int * int option -> Vector.vector
	val copy :       {di:int, dst:array, len:int option, si:int, src:array} -> unit
	val copyVec :    {di:int, dst:array, len:int option, si:int, src:Vector.vector} -> unit

	val app :        (elem -> unit) -> array -> unit
	val appr :       (elem -> unit) -> array -> unit
	val modify :     (elem -> elem) -> array -> unit
	val foldl :      (elem * 'a -> 'a) -> 'a -> array -> 'a
	val foldr :      (elem * 'a -> 'a) -> 'a -> array -> 'a
	val appi :       (int * elem -> unit) -> array * int * int option -> unit
	val appri :      (int * elem -> unit) -> array * int * int option -> unit
	val modifyi :    (int * elem -> elem) -> array * int * int option -> unit
	val foldli :     (int * elem * 'a -> 'a) -> 'a -> array * int * int option -> 'a
	val foldri :     (int * elem * 'a -> 'a) -> 'a -> array * int * int option -> 'a

	val all :        (elem -> bool) -> array -> bool
	val exists :     (elem -> bool) -> array -> bool
	val find :       (elem -> bool) -> array -> elem option

	val equal :      (elem * elem -> bool) -> array * array -> bool
	val compare :    (elem * elem -> order) -> array * array -> order

	val isSorted :   (elem * elem -> order) -> array -> bool
	val sort :       (elem * elem -> order) -> array -> unit
    end
  </PRE>

<?php section("description", "description") ?>

  <P>
    Items not described here are as in the 
    <A href="http://www.dina.kvl.dk/~sestoft/sml/mono-array.html">Standard ML
    Basis' <TT>MONO_ARRAY</TT></A> signature.
  </P>

  <DL>
    <DT>
      <TT>type t = array</TT>
    </DT>
    <DD>
      <P>A local synonym for type <TT>array</TT>.</P>
    </DD>

    <DT>
      <TT>new (<I>n</I>, <I>init</I>)</TT>
    </DT>
    <DD>
      <P>Creates a new array of size <TT><I>n</I></TT>, initialised with
      value <TT><I>init</I></TT>. Synonym for funciton <TT>array</TT>.</P>
    </DD>

    <DT>
      <TT>fromVector <I>v</I></TT>
    </DT>
    <DD>
      <P>Creates a vector containing the same elements as the array
      <TT><I>arr</I></TT>. If <TT><I>v</I></TT> contains more than
      <TT>maxLen</TT> elements, then the <TT>Size</TT> exception is raised.</P>
    </DD>

    <DT>
      <TT>toVector <I>arr</I></TT>
    </DT>
    <DD>
      <P>Creates a vector containing the same elements as the array
      <TT><I>arr</I></TT>. If <TT><I>arr</I></TT> contains more than
      <TT>Vector.maxLen</TT> elements, then the <TT>Size</TT> exception is raised.</P>
    </DD>

    <DT>
      <TT>toList <I>arr</I></TT>
    </DT>
    <DD>
      <P>Creates a list of the elements of <TT><I>arr</I></TT> in order of
      increasing indices.</P>
    </DD>

    <DT>
      <TT>rev <I>arr</I></TT>
    </DT>
    <DD>
      <P>Reverses in-place the order of elements in array
      <TT><I>arr</I></TT>.</P>
    </DD>

    <DT>
      <TT>swap (<I>arr</I>, <I>i</I>, <I>j</I>)</TT>
    </DT>
    <DD>
      <P>Swaps the <I>i</I>th and <I>j</I>th element of array
      <TT><I>arr</I></TT>. If <I>i</I> &lt; 0 or |<I>arr</I>| <= <I>i</I>,
      or <I>j</I> &lt; 0 or |<I>arr</I>| <= <I>j</I>,
      then the <TT>Subscript</TT> exception is raised.</P>
    </DD>

    <DT>
      <TT>appri <I>f</I> <I>slice</I></TT> <BR>
      <TT>appr <I>f</I> <I>arr</I></TT>
    </DT>
    <DD>
      <P>Like <TT>appi</TT> and <TT>app</TT>, but apply <TT><I>f</I></TT> in
      right to left order (i.e., decreasing indices). The expression
      <TT>app <I>f arr</I></TT> is equivalent to: 
      <PRE>
        appri (f o #2) (arr, 0, NONE)</PRE>
    </DD>

    <DT>
      <TT>exists <I>f</I> <I>arr</I></TT>
    </DT>
    <DD>
      <P>Applies <TT><I>f</I></TT> to each element <TT><I>x</I></TT> of the
      array <TT><I>arr</I></TT>, from left to right, until <TT><I>f x</I></TT>
      evaluates to <TT>true</TT>; returns <TT>true</TT> if such an
      <TT><I>x</I></TT> exists and <TT>false</TT> otherwise.</P>
    </DD>

    <DT>
      <TT>all <I>f</I> <I>arr</I></TT>
    </DT>
    <DD>
      <P>Applies <TT><I>f</I></TT> to each element <TT><I>x</I></TT> of the
      array <TT><I>arr</I></TT>, from left to right, until <TT><I>f x</I></TT>
      evaluates to <TT>false</TT>; returns <TT>false</TT> if such an
      <TT><I>x</I></TT> exists and <TT>true</TT> otherwise.
      Equivalent to <TT>not(exists (not o <I>f</I>) <I>l</I>))</TT>.</P>
    </DD>

    <DT>
      <TT>find <I>f</I> <I>arr</I></TT>
    </DT>
    <DD>
      <P>Applies <TT><I>f</I></TT> to each element <TT><I>x</I></TT> of the
      array <TT><I>arr</I></TT>, from left to right, until <TT><I>f x</I></TT>
      evaluates to <TT>true</TT>; returns <TT>SOME <I>x</I></TT> if such an
      <TT><I>x</I></TT> exists and <TT>NONE</TT> otherwise.</P>
    </DD>

    <DT>
      <TT>equal <I>equal'</I> (<I>arr1</I>, <I>arr2</I>)</TT>
    </DT>
    <DD>
      <P>Creates an equality function on arrays given an equality on the
      element type.</P>
    </DD>

    <DT>
      <TT>compare <I>compare'</I> (<I>arr1</I>, <I>arr2</I>)</TT>
    </DT>
    <DD>
      <P>Creates an ordering function on arrays, given a suitable ordering functions
      for its element type. The order induced is lexicographic.</P>
    </DD>

    <DT>
      <TT>isSorted <I>f</I> <I>arr</I></TT>
    </DT>
    <DD>
      <P>Returns <TT>true</TT> iff <TT><I>arr</I></TT> is sorted with respect
      to the ordering function <TT><I>f</I></TT>.</P>
    </DD>

    <DT>
      <TT>sort <I>f</I> <I>arr</I></TT>
    </DT>
    <DD>
      <P>Sorts <TT><I>arr</I></TT> with respect to the ordering function
      <TT><I>f</I></TT>. Sorting may be unstable with respect to equal
      elements.</P>
    </DD>

<?php section("also", "see also") ?>

  <DL><DD>
    <A href="mono-vector.php3"><TT>MONO_VECTOR</TT></A>,
    <A href="array.php3"><TT>Array</TT></A>
  </DD></DL>

<?php footing() ?>
