<?php include("macros.php3"); ?>
<?php heading("The <TT>Array</TT> structure", "The <TT>Array</TT> structure") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature ARRAY
    structure Array : ARRAY
  </PRE>

  <P>
    An extended version of the
    <A href="http://www.dina.kvl.dk/~sestoft/sml/array.html">Standard ML
    Basis' <TT>Array</TT> </A> structure.
  </P>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <PRE>
    signature ARRAY =
    sig
	eqtype 'a array
	type 'a vector
	type 'a t = 'a array

	val maxLen :     int

	val array :      int * 'a -> 'a array
	val new :        int * 'a -> 'a array
	val fromList :   'a list -> 'a array
	val toList :     'a array -> 'a list
	val fromVector : 'a vector -> 'a array
	val toVector :   'a array -> 'a vector
	val tabulate :   int * (int -> 'a) -> 'a array

	val length :     'a array -> int
	val sub :        'a array * int -> 'a
	val update :     'a array * int * 'a -> unit
	val swap :       'a array * int * int -> unit
	val rev :        'a array -> unit
	val extract :    'a array * int * int option -> 'a vector
	val copy :       {di:int, dst:'a array, len:int option, si:int, src:'a array} -> unit
	val copyVec :    {di:int, dst:'a array, len:int option, si:int, src:'a vector} -> unit

	val app :        ('a -> unit) -> 'a array -> unit
	val appr :       ('a -> unit) -> 'a array -> unit
	val modify :     ('a -> 'a) -> 'a array -> unit
	val foldl :      ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
	val foldr :      ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
	val appi :       (int * 'a -> unit) -> 'a array * int * int option -> unit
	val appri :      (int * 'a -> unit) -> 'a array * int * int option -> unit
	val modifyi :    (int * 'a -> 'a) -> 'a array * int * int option -> unit
	val foldli :     (int * 'a * 'b -> 'b) -> 'b -> 'a array * int * int option -> 'b
	val foldri :     (int * 'a * 'b -> 'b) -> 'b -> 'a array * int * int option -> 'b

	val all :        ('a -> bool) -> 'a array -> bool
	val exists :     ('a -> bool) -> 'a array -> bool
	val find :       ('a -> bool) -> 'a array -> 'a option

	val equal :      ('a * 'a -> bool) -> 'a array * 'a array -> bool
	val compare :    ('a * 'a -> order) -> 'a array * 'a array -> order

	val isSorted :   ('a * 'a -> order) -> 'a array -> bool
	val sort :       ('a * 'a -> order) -> 'a array -> unit
    end
  </PRE>

<?php section("description", "description") ?>

  <P>
    Items not described here are as in the 
    <A href="http://www.dina.kvl.dk/~sestoft/sml/array.html">Standard ML
    Basis' <TT>Array</TT></A> structure.
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
    <A href="mono-array.php3"><TT>MONO_ARRAY</TT></A>,
    <A href="vector.php3"><TT>Vector</TT></A>
  </DD></DL>

<?php footing() ?>
