<?php include("macros.php3"); ?>
<?php heading("The VectorPair structure", "The <TT>VectorPair</TT> structure") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature VECTOR_PAIR
    structure VectorPair : VECTOR_PAIR
  </PRE>

  <P>
    The <TT>VectorPair</TT> structure provides operations on pairs of vectors,
    very much like the <A href="list-pair.php3"><TT>ListPair</TT></A> structure
    does for pairs of lists. These operations do not require that the vectors
    have the same length; when they are of uneven lengths, the excess elements
    from the longer vector are ignored.
  </P>

  <P>See also:
    <A href="vector.php3"><TT>Vector</TT></A>,
    <A href="list-pair.php3"><TT>ListPair</TT></A>
  </P>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <PRE>
    signature VECTOR_PAIR =
    sig
	val zip :    'a vector * 'b vector -> ('a * 'b) vector
	val unzip :  ('a * 'b) vector -> 'a vector * 'b vector

	val app :    ('a * 'b -> unit) -> 'a vector * 'b vector -> unit
	val appr :   ('a * 'b -> unit) -> 'a vector * 'b vector -> unit
	val map :    ('a * 'b -> 'c) -> 'a vector * 'b vector -> 'c vector
	val foldl :  ('a * 'b * 'c ->'c) -> 'c -> 'a vector * 'b vector -> 'c
	val foldr :  ('a * 'b * 'c ->'c) -> 'c -> 'a vector * 'b vector -> 'c
	val appi :   (int * 'a * 'b -> unit) -> 'a vector * 'b vector * int * int option -> unit
	val appri :  (int * 'a * 'b -> unit) -> 'a vector * 'b vector * int * int option -> unit
	val mapi :   (int * 'a * 'b -> 'c) -> 'a vector * 'b vector * int * int option -> 'c vector
	val foldli : (int * 'a * 'b * 'c -> 'c) -> 'c -> 'a vector * 'b vector * int * int option -> 'c
	val foldri : (int * 'a * 'b * 'c -> 'c) -> 'c -> 'a vector * 'b vector * int * int option -> 'c
	val all :    ('a * 'b -> bool) -> 'a vector * 'b vector -> bool
	val exists : ('a * 'b -> bool) -> 'a vector * 'b vector -> bool
	val find :   ('a * 'b -> bool) -> 'a vector * 'b vector -> ('a * 'b) option
    end
  </PRE>

<?php section("description", "description") ?>

  <DL>
    <DT>
      <TT>zip (<I>vec1</I>, <I>vec2</I>)</TT>
    </DT>
    <DD>
      <P>Combines the two vectors <TT><I>vec1</I></TT> and <TT><I>vec2</I></TT>
      into a vector of pairs, with the first element of each vector comprising
      the first element of the result, the second elements comprising the
      second element of the result, and so on. If the vectors are of unequal
      lengths, the excess elements from the longer one are ignored.</P>
    </DD>

    <DT>
      <TT>unzip <I>vec</I></TT>
    </DT>
    <DD>
      <P>Returns a pair of vectors formed by splitting the elements of
      <TT><I>vec</I></TT>. This is the inverse of <TT>zip</TT> for equal length
      vectors.</P>
    </DD>

    <DT>
      <TT>map <I>f</I> (<I>vec1</I>, <I>vec2</I>)</TT>
    </DT>
    <DD>
      <P>Maps the function <TT><I>f</I></TT> over the vector of pairs of
      elements from from the vectors <TT><I>vec1</I></TT> and
      <TT><I>vec2</I></TT>, returning the vector of results.
      If the vectors are of unequal lengths, the excess elements from the
      longer one are ignored. The following equivalence holds:</P>
      <PRE>
        map <TT><I>f</I></TT> (<TT><I>vec1</I>, </TT><TT><I>vec2</I></TT>) = Vector.map <TT><I>f</I></TT> (zip (<TT><I>vec1</I></TT>, <TT><I>vec2</I></TT>))</PRE>
    </DD>

    <DT>
      <TT>app <I>f</I> (<I>vec1</I>, <I>vec2</I>)</TT> <BR>
      <TT>appr <I>f</I> (<I>vec1</I>, <I>vec2</I>)</TT>
    </DT>
    <DD>
      <P>Applies the function <TT><I>f</I></TT> to the vector of pairs of
      elements from from the vectors <TT><I>vec1</I></TT> and
      <TT><I>vec2</I></TT> in left to right order (right to left, respectively).
      If the vectors are of unequal lengths, the excess elements from the
      longer one are ignored. The following equivalences hold:</P>
      <PRE>
        app  <I>f</I> (<I>vec1</I>, <I>vec2</I>) = Vector.app  <I>f</I> (zip (<I>vec1</I>, <I>vec2</I>))
        appr <I>f</I> (<I>vec1</I>, <I>vec2</I>) = Vector.appr <I>f</I> (zip (<I>vec1</I>, <I>vec2</I>))</PRE>
    </DD>

    <DT>
      <TT>foldl <I>f</I> <I>b</I> (<I>vec1</I>, <I>vec2</I>)</TT> <BR>
      <TT>foldr <I>f</I> <I>b</I> (<I>vec1</I>, <I>vec2</I>)</TT>
    </DT>
    <DD>
      <P>Return the result of folding the function <TT><I>f</I></TT> over the
      pair of vectors <TT><I>vec1</I></TT> and
      <TT><I>vec2</I></TT>. The following equivalences hold:</P>
      <PRE>
        foldl <I>f</I> <I>b</I> (<I>vec1</I>, <I>vec2</I>) = Vector.foldl <I>f'</I> <I>b</I> (zip (<I>vec1</I>, <I>vec2</I>))
        foldr <I>f</I> <I>b</I> (<I>vec1</I>, <I>vec2</I>) = Vector.foldr <I>f'</I> <I>b</I> (zip (<I>vec1</I>, <I>vec2</I>))</PRE>
      <P>where <TT><I>f'</I></TT> is <TT>fn ((<I>a</I>,<I>b</I>),<I>c</I>) => <I>f</I>(<I>a</I>,<I>b</I>,<I>c</I>)</TT>.</P>
    </DD>

    <DT>
      <TT>appi <I>f</I> (<I>l1</I>, <I>l2</I>, <I>i</I>, <I>opt</I>)</TT> <BR>
      <TT>appri <I>f</I> (<I>l1</I>, <I>l2</I>, <I>i</I>, <I>opt</I>)</TT> <BR>
      <TT>mapi <I>f</I> (<I>l1</I>, <I>l2</I>, <I>i</I>, <I>opt</I>)</TT> <BR>
      <TT>foldli <I>f</I> <I>b</I> (<I>l1</I>, <I>l2</I>, <I>i</I>, <I>opt</I>)</TT> <BR>
      <TT>foldri <I>f</I> <I>b</I> (<I>l1</I>, <I>l2</I>, <I>i</I>, <I>opt</I>)</TT>
    </DT>
    <DD>
      <P>Indexed versions of the functions <TT>app</TT>, <TT>appr</TT>,
      <TT>map</TT>, <TT>foldl</TT> and <TT>foldr</TT>. The index of each element
      is passed to <TT><I>f</I></TT> as an additional
      argument. In the case of <TT>appri</TT> and <TT>foldri</TT>, processing
      starts at the highest index. The following equivalences hold:</P>
      <PRE>
        app <I>f</I> (<I>l1</I>, <I>l2</I>)     = appi (fn (<I>i</I>,<I>a</I>,<I>b</I>) => <I>f</I>(<I>a</I>,<I>b</I>)) (<I>l1</I>, <I>l2</I>, 0, NONE)
        appr <I>f</I> (<I>l1</I>, <I>l2</I>)    = appri (fn (<I>i</I>,<I>a</I>,<I>b</I>) => <I>f</I>(<I>a</I>,<I>b</I>)) (<I>l1</I>, <I>l2</I>, 0, NONE)
        map <I>f</I> (<I>l1</I>, <I>l2</I>)     = mapi (fn (<I>i</I>,<I>a</I>,<I>b</I>) => <I>f</I>(<I>a</I>,<I>b</I>)) (<I>l1</I>, <I>l2</I>, 0, NONE)
        foldl <I>f b</I> (<I>l1</I>, <I>l2</I>) = foldli (fn (<I>i</I>,<I>a</I>,<I>b</I>,<I>c</I>) => <I>f</I>(<I>a</I>,<I>b</I>,<I>c</I>)) <I>b</I> (<I>l1</I>, <I>l2</I>, 0, NONE)
        foldr <I>f b</I> (<I>l1</I>, <I>l2</I>) = foldri (fn (<I>i</I>,<I>a</I>,<I>b</I>,<I>c</I>) => <I>f</I>(<I>a</I>,<I>b</I>,<I>c</I>)) <I>b</I> (<I>l1</I>, <I>l2</I>, 0, NONE)</PRE>
    </DD>

    <DT>
      <TT>all <I>f</I> (<I>vec1</I>, <I>vec2</I>)</TT> <BR>
      <TT>exists <I>f</I> (<I>vec1</I>, <I>vec2</I>)</TT>
    </DT>
    <DD>
      <P>These functions provide short-circuit testing of a predicate
      <TT><I>f</I></TT> over a pair of vectors. The following respective
      equivalences hold:</P>
      <PRE>
         all <I>f</I> (<I>vec1</I>, <I>vec2</I>) = Vector.all <I>f</I> (zip (<I>vec1</I>, <I>vec2</I>))
         exists <I>f</I> (<I>vec1</I>, <I>vec2</I>) = Vector.exists <I>f</I> (zip (<I>vec1</I>, <I>vec2</I>))</PRE>
    </DD>

    <DT>
      <TT>find <I>f</I> (<I>l1</I>, <I>l2</I>)</TT>
    </DT>
    <DD>
      <P>Applies <TT><I>f</I></TT> to each pair <TT>(<I>x1</I>,<I>x2</I>)</TT>
      of elements of the vectors, from left to right, until
      <TT><I>f</I>(<I>x1</I>,<I>x2</I>)</TT>
      evaluates to <TT>true</TT>; returns <TT>SOME(<I>x1</I>,<I>x2</I>)</TT> if
      such a pair exists and <TT>NONE</TT> otherwise. If the vectors are of
      unequal lengths, the excess elements of the longer one are
      ignored. The above expression is equivalent to:</P>
      <PRE>
         Vector.find <I>f</I> (zip (<I>vec1</I>, <I>vec2</I>))</PRE>
    </DD>
  </DL>

<?php footing() ?>
