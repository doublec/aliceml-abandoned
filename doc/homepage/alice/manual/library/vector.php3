<?php include("macros.php3"); ?>
<?php heading("The Vector structure", "The <TT>Vector</TT> structure") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature VECTOR
    structure Vector : VECTOR
  </PRE>

  <P>
    An extended version of the
    <A href="http://www.dina.kvl.dk/~sestoft/sml/vector.html">Standard ML
    Basis' <TT>Vector</TT></A> structure.
  </P>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <PRE>
    signature VECTOR =
    sig
	eqtype 'a vector
	type   'a t = 'a vector

	val maxLen :      int

	val toList :      'a vector -> 'a list
	val fromList :    'a list -> 'a vector
	val tabulate :    int * (int -> 'a) -> 'a vector

	val length :      'a vector -> int
	val sub :         'a vector * int -> 'a
	val replace :     'a vector * int * 'a -> 'a vector
	val extract :     'a vector * int * int option -> 'a vector
	val concat :      'a vector list -> 'a vector
	val rev :         'a vector -> 'a vector

	val app :         ('a -> unit) -> 'a vector -> unit
	val appr :        ('a -> unit) -> 'a vector -> unit
	val map :         ('a -> 'b) -> 'a vector -> 'b vector
	val foldl :       ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
	val foldr :       ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
	val appi :        (int * 'a -> unit) -> 'a vector * int * int option -> unit
	val appri :       (int * 'a -> unit) -> 'a vector * int * int option -> unit
	val mapi :        (int * 'a -> 'b) -> 'a vector * int * int option -> 'b vector
	val foldli :      (int * 'a * 'b -> 'b) -> 'b -> 'a vector * int * int option -> 'b
	val foldri :      (int * 'a * 'b -> 'b) -> 'b -> 'a vector * int * int option -> 'b

	val all :         ('a -> bool) -> 'a vector -> bool
	val exists :      ('a -> bool) -> 'a vector -> bool
	val find :        ('a -> bool) -> 'a vector -> 'a option
	val contains :    ''a vector -> ''a -> bool
	val notContains : ''a vector -> ''a -> bool

	val equal :       ('a * 'a -> bool) -> 'a vector * 'a vector -> bool
	val compare :     ('a * 'a -> order) -> 'a vector * 'a vector -> order

	val isSorted :    ('a * 'a -> order) -> 'a vector -> bool
	val sort :        ('a * 'a -> order) -> 'a vector -> 'a vector
    end
  </PRE>

<?php section("description", "description") ?>

  <P>
    Items not described here are as in the 
    <A href="http://www.dina.kvl.dk/~sestoft/sml/vector.html">Standard ML
    Basis' <TT>Vector</TT></A> structure.
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
      <I>i</I>th element is set to <TT><I>x</I></TT>. If <TT><I>i</I></TT> &lt; 0
      or |<TT><I>vec</I></TT>| &lt;= <TT><I>i</I></TT>, then the Subscript
      exception is raised.</P>
    </DD>

    <DT>
      <TT>appri <I>f</I> <I>slice</I></TT> <BR>
      <TT>appr <I>f</I> <I>vec</I></TT>
    </DT>
    <DD>
      <P>Like <TT>appi</TT> and <TT>app</TT>, but apply <TT><I>f</I></TT> in
      right to left order (i.e., decreasing indices). The expression
      <TT>app <I>f vec</I></TT> is equivalent to:</P>
      <PRE>
        appri (<I>f</I> o #2) (<I>vec</I>, 0, NONE)</PRE>
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
  </DL>

<?php section("also", "see also") ?>

  <DL><DD>
    <A href="mono-vector.php3"><TT>MONO_VECTOR</TT></A>,
    <A href="vector-pair.php3"><TT>VectorPair</TT></A>,
    <A href="array.php3"><TT>Array</TT></A>
  </DD></DL>

<?php footing() ?>
