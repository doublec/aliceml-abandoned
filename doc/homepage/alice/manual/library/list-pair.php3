<?php include("macros.php3"); ?>
<?php heading("The ListPair structure", "The <TT>ListPair</TT> structure") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature LIST_PAIR
    structure ListPair : LIST_PAIR
  </PRE>

  <P>
    An extended version of the
    <A href="http://www.dina.kvl.dk/~sestoft/sml/list-pair.html">Standard ML
    Basis' <TT>ListPair</TT></A> structure.
  </P>

  <P>See also:
    <A href="list.php3"><TT>List</TT></A>,
    <A href="vector-pair.php3"><TT>VectorPair</TT></A>
  </P>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <PRE>
    signature LIST_PAIR =
    sig
	val zip :        'a list * 'b list -> ('a * 'b) list
	val unzip :      ('a * 'b) list -> 'a list * 'b list

	val app :        ('a * 'b -> unit) -> 'a list * 'b list -> unit
	val appr :       ('a * 'b -> unit) -> 'a list * 'b list -> unit
	val appi :       (int * 'a * 'b -> unit) -> 'a list * 'b list -> unit
	val appri :      (int * 'a * 'b -> unit) -> 'a list * 'b list -> unit
	val map :        ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list
	val mapi :       (int * 'a * 'b -> 'c) -> 'a list * 'b list -> 'c list
	val mapPartial : ('a * 'b -> 'c option) -> 'a list * 'b list -> 'c list
	val foldl :      ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
	val foldr :      ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
	val foldli :     (int * 'a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list ->'c
	val foldri :     (int * 'a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
	val all :        ('a * 'b -> bool) -> 'a list * 'b list -> bool
	val exists :     ('a * 'b -> bool) -> 'a list * 'b list -> bool
	val find :       ('a * 'b -> bool) -> 'a list * 'b list -> ('a * 'b) option
    end
  </PRE>

<?php section("description", "description") ?>

  <P>
    Items not described here are as in the 
    <A href="http://www.dina.kvl.dk/~sestoft/sml/list-pair.html">Standard ML
    Basis' <TT>ListPair</TT></A> structure.
  </P>

  <DL>
    <DT>
      <TT>appr <I>f</I> (<I>l1</I>, <I>l2</I>)</TT>
    </DT>
    <DD>
      <P>Like <TT>app</TT>, but applies <TT><I>f</I></TT> in
      right to left order.</P>
    </DD>

    <DT>
      <TT>mapPartial <I>f</I> (<I>l1</I>, <I>l2</I>)</TT>
    </DT>
    <DD>
      <P>Maps the function <TT><I>f</I></TT> over the list of pairs of elements
      from the lists <TT><I>l1</I></TT> and <TT><I>l2</I></TT>, returning the
      list of results where <TT><I>f</I></TT> was defined. If the lists are of
      unequal lengths, the excess elements from the tail of the longer one are
      ignored. The above expression is equivalent to:</P>
      <PRE>
         List.mapPartial f (zip (l1, l2)).</PRE>
    </DD>

    <DT>
      <TT>find <I>f</I> (<I>l1</I>, <I>l2</I>)</TT>
    </DT>
    <DD>
      <P>Applies <TT><I>f</I></TT> to each pair <TT>(<I>x1</I>,<I>x2</I>)</TT>
      of elements of the lists, from left to right, until
      <TT><I>f</I>(<I>x1</I>,<I>x2</I>)</TT>
      evaluates to <TT>true</TT>; returns <TT>SOME(<I>x1</I>,<I>x2</I>)</TT> if
      such a pair exists and <TT>NONE</TT> otherwise. If the lists are of
      unequal lengths, the excess elements from the tail of the longer one are
      ignored. The above expression is equivalent to:</P>
      <PRE>
         List.find <I>f</I> (zip (<I>l1</I>, <I>l2</I>))</PRE>
    </DD>

    <DT>
      <TT>appi <I>f</I> (<I>l1</I>, <I>l2</I>)</TT> <BR>
      <TT>appri <I>f</I> (<I>l1</I>, <I>l2</I>)</TT> <BR>
      <TT>mapi <I>f</I> (<I>l1</I>, <I>l2</I>)</TT> <BR>
      <TT>foldli <I>f</I> <I>b</I> (<I>l1</I>, <I>l2</I>)</TT> <BR>
      <TT>foldri <I>f</I> <I>b</I> (<I>l1</I>, <I>l2</I>)</TT>
    </DT>
    <DD>
      <P>Indexed versions of the functions <TT>app</TT>, <TT>appr</TT>,
      <TT>map</TT>, <TT>foldl</TT> and <TT>foldr</TT>. The index of each element
      (starting from 0) is passed to <TT><I>f</I></TT> as an additional
      argument. In the case of <TT>appri</TT> and <TT>foldri</TT>, processing
      starts at the highest index. The following equivalences hold:</P>
      <PRE>
        app <I>f</I> (<I>l1</I>, <I>l2</I>)     = appi (fn (<I>i</I>,<I>a</I>,<I>b</I>) => <I>f</I>(<I>a</I>,<I>b</I>)) (<I>l1</I>, <I>l2</I>)
        appr <I>f</I> (<I>l1</I>, <I>l2</I>)    = appri (fn (<I>i</I>,<I>a</I>,<I>b</I>) => <I>f</I>(<I>a</I>,<I>b</I>)) (<I>l1</I>, <I>l2</I>)
        map <I>f</I> (<I>l1</I>, <I>l2</I>)     = mapi (fn (<I>i</I>,<I>a</I>,<I>b</I>) => <I>f</I>(<I>a</I>,<I>b</I>)) (<I>l1</I>, <I>l2</I>)
        foldl <I>f b</I> (<I>l1</I>, <I>l2</I>) = foldli (fn (<I>i</I>,<I>a</I>,<I>b</I>,<I>c</I>) => <I>f</I>(<I>a</I>,<I>b</I>,<I>c</I>)) <I>b</I> (<I>l1</I>, <I>l2</I>)
        foldr <I>f b</I> (<I>l1</I>, <I>l2</I>) = foldri (fn (<I>i</I>,<I>a</I>,<I>b</I>,<I>c</I>) => <I>f</I>(<I>a</I>,<I>b</I>,<I>c</I>)) <I>b</I> (<I>l1</I>, <I>l2</I>)</PRE>
    </DD>
  </DL>

<?php footing() ?>
