<?php include("macros.php3"); ?>
<?php heading("The List structure", "The <TT>List</TT> structure") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature LIST
    structure List : LIST
  </PRE>

  <P>
    An extended version of the
    <A href="http://www.dina.kvl.dk/~sestoft/sml/list.html">Standard ML
    Basis' <TT>List</TT></A> structure.
  </P>

  <P>See also:
    <A href="list-pair.php3"><TT>ListPair</TT></A>,
    <A href="vector.php3"><TT>Vector</TT></A>
  </P>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <PRE>
    signature LIST =
    sig
	datatype 'a list = nil | op:: of 'a * 'a list
	type     'a t    = 'a list						(**)

	exception Empty

	val null :        'a list -> bool
	val length :      'a list -> int

	val hd :          'a list -> 'a
	val tl :          'a list -> 'a list
	val last :        'a list -> 'a
	val getItem :     'a list -> ('a * 'a list) option
	val nth :         'a list * int -> 'a
	val sub :         'a list * int -> 'a
	val take :        'a list * int -> 'a list
	val drop :        'a list * int -> 'a list

	val rev :         'a list -> 'a list
	val op @ :        'a list * 'a list -> 'a list
	val revAppend :   'a list * 'a list -> 'a list
	val concat :      'a list list -> 'a list

	val app :         ('a -> unit) -> 'a list -> unit
	val appr :        ('a -> unit) -> 'a list -> unit
	val appi :        (int * 'a -> unit) -> 'a list -> unit
	val appri :       (int * 'a -> unit) -> 'a list -> unit
	val map :         ('a -> 'b) -> 'a list -> 'b list
	val mapi :        (int * 'a -> 'b) -> 'a list -> 'b list
	val mapPartial :  ('a -> 'b option) -> 'a list -> 'b list
	val find :        ('a -> bool) -> 'a list -> 'a option
	val filter :      ('a -> bool) -> 'a list -> 'a list
	val partition :   ('a -> bool) -> 'a list -> 'a list * 'a list
	val foldl :       ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
	val foldr :       ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
	val foldli :      (int * 'a * 'b -> 'b) -> 'b -> 'a list -> 'b
	val foldri :      (int * 'a * 'b -> 'b) -> 'b -> 'a list -> 'b
	val all :         ('a -> bool) -> 'a list -> bool
	val exists :      ('a -> bool) -> 'a list -> bool
	val contains :    ''a list -> ''a -> bool
	val notContains : ''a list -> ''a -> bool

	val tabulate :    int * (int -> 'a) -> 'a list

	val equal :       ('a * 'a -> bool) -> 'a list * 'a list -> bool
	val compare :     ('a * 'a -> order) -> 'a list * 'a list -> order

	val isSorted :    ('a * 'a -> order) -> 'a list -> bool
	val sort :        ('a * 'a -> order) -> 'a list -> 'a list
    end
  </PRE>

<?php section("description", "description") ?>

  <P>
    Items not described here are as in the 
    <A href="http://www.dina.kvl.dk/~sestoft/sml/list.html">Standard ML
    Basis' <TT>List</TT></A> structure.
  </P>

  <DL>
    <DT>
      <TT>type t = list</TT>
    </DT>
    <DD>
      <P>A local synonym for type <TT>list</TT>.</P>
    </DD>

    <DT>
      <TT>sub (<I>l</I>, <I>i</I>)</TT>
    </DT>
    <DD>
      <P>Returns the <I>i</I>th element of the list <TT><I>l</I></TT>, counting from 0.
      Raises <TT>Subscript</TT> if <TT><I>i</I></TT> &lt; 0 or
      <TT><I>i</I></TT> &gt;= <TT>length <I>l</I></TT>. This function is a
      synonym for <TT>nth</TT>, for consistency with other collection types
      like <A href="vector.php3">vectors</A> and <A
      href="array.php3">arrays</A>.</P>
    </DD>

    <DT>
      <TT>appr <I>f</I> <I>l</I></TT>
    </DT>
    <DD>
      <P>Like <TT>app</TT>, but applies <TT><I>f</I></TT> in
      right to left order.</P>
    </DD>

    <DT>
      <TT>appi <I>f</I> <I>l</I></TT> <BR>
      <TT>appri <I>f</I> <I>l</I></TT> <BR>
      <TT>mapi <I>f</I> <I>l</I></TT> <BR>
      <TT>foldli <I>f</I> <I>b</I> <I>l</I></TT> <BR>
      <TT>foldri <I>f</I> <I>b</I> <I>l</I></TT>
    </DT>
    <DD>
      <P>Indexed versions of the functions <TT>app</TT>, <TT>appr</TT>,
      <TT>map</TT>, <TT>foldl</TT> and <TT>foldr</TT>. The index of each element
      (starting from 0) is passed to <TT><I>f</I></TT> as an additional
      argument. In the case of <TT>appri</TT> and <TT>foldri</TT>, processing
      starts at the highest index. The following equivalences hold:</P>
      <PRE>
        app <I>f l</I>     = appi (<I>f</I> o #2) <I>l</I>
        appr <I>f l</I>    = appri (<I>f</I> o #2) <I>l</I>
        map <I>f l</I>     = mapi (<I>f</I> o #2) <I>l</I>
        foldl <I>f b l</I> = foldli (fn (<I>i</I>,<I>a</I>,<I>b</I>) => <I>f</I>(<I>a</I>,<I>b</I>)) <I>b</I> <I>l</I>
        foldr <I>f b l</I> = foldri (fn (<I>i</I>,<I>a</I>,<I>b</I>) => <I>f</I>(<I>a</I>,<I>b</I>)) <I>b</I> <I>l</I></PRE>
    </DD>

    <DT>
      <TT>contains <I>l</I> <I>a</I></TT>
    </DT>
    <DD>
      <P>Returns <TT>true</TT> if the element <TT><I>a</I></TT> occurs in the
      list <TT><I>l</I></TT>; otherwise <TT>false</TT>.</P>
    </DD>

    <DT>
      <TT>notContains <I>l</I> <I>a</I></TT>
    </DT>
    <DD>
      <P>Returns <TT>true</TT> if the element <TT><I>a</I></TT> does not occur in the
      list <TT><I>l</I></TT>; otherwise <TT>false</TT>.
      Equivalent to <TT>not(contains <I>l a</I>)</TT>.</P>
    </DD>

    <DT>
      <TT>equal <I>equal'</I> (<I>l1</I>, <I>l2</I>)</TT>
    </DT>
    <DD>
      <P>Creates a specific equality function on lists given an equality on the
      element type.</P>
    </DD>

    <DT>
      <TT>compare <I>compare'</I> (<I>l1</I>, <I>l2</I>)</TT>
    </DT>
    <DD>
      <P>Creates an ordering function on lists, given a suitable ordering functions
      for its element type. The order induced is lexicographic.</P>
    </DD>

    <DT>
      <TT>isSorted <I>f</I> <I>l</I></TT>
    </DT>
    <DD>
      <P>Returns <TT>true</TT> iff <TT><I>l</I></TT> is sorted with respect
      to the ordering function <TT><I>f</I></TT>.</P>
    </DD>

    <DT>
      <TT>sort <I>f</I> <I>l</I></TT>
    </DT>
    <DD>
      <P>Returns a new list that contains the same elements as
      <TT><I>l</I></TT>, but sorted with respect to the ordering function
      <TT><I>f</I></TT>. Sorting may be unstable with respect to equal
      elements.</P>
    </DD>
  </DL>

<?php footing() ?>
