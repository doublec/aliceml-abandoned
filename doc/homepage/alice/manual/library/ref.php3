<?php include("macros.php3"); ?>
<?php heading("The <TT>Ref</TT> structure", "The <TT>Ref</TT> structure") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature REF
    structure Ref : REF
  </PRE>

  <P>
    Common operations on references.
  </P>

  <P>
    The type <TT>alt</TT> and its constructors are available in the
    <A href="toplevel.php3">top-level environment</A>.
  </P>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <PRE>
    signature REF =
    sig
	datatype ref = datatype ref
	type  'a t   = 'a ref

	val new :      'a -> 'a ref
	val ! :        'a ref -> 'a
	val := :       'a ref * 'a -> unit
	val :=: :      'a ref * 'a ref -> unit
	val exchange : 'a ref * 'a -> 'a

	val app :      ('a -> unit) -> 'a ref -> unit
	val map :      ('a -> 'a) -> 'a ref -> 'a ref

	val equal :    'a ref * 'a ref -> bool
    end
  </PRE>

<?php section("description", "description") ?>

  <DL>
    <DT>
      <TT>datatype 'a ref = ref of 'a</TT> <BR>
      <TT>type t = ref</TT>
    </DT>
    <DD>
      <P>The type of mutable references.</P>
    </DD>

    <DT>
      <TT>new <I>s</I></TT>
    </DT>
    <DD>
      <P>Creates a reference to the value <TT><I>a</I></TT>.
      This function <TT>new</TT> is a synonym for the <TT>ref</TT>
      constructor.</P>
    </DD>

    <DT>
      <TT>! <I>re</I></TT>
    </DT>
    <DD>
      <P>Returns the value referred to by <TT><I>re</I></TT>.</P>
    </DD>

    <DT>
      <TT><I>re</I> := <I>a</I></TT>
    </DT>
    <DD>
      <P>Makes the reference <TT><I>re</I></TT> refer to value
      <TT><I>a</I></TT>.</P>
    </DD>

    <DT>
      <TT><I>re1</I> :=: <I>re2</I></TT>
    </DT>
    <DD>
      <P>Swaps the values referred to by the references <TT><I>re1</I></TT> and
      <TT><I>re2</I></TT>.</P>
    </DD>

    <DT>
      <TT>exchange (<I>re</I>, <I>a</I>)</TT>
    </DT>
    <DD>
      <P>Makes the reference <TT><I>re</I></TT> refer to value
      <TT><I>a</I></TT> and returns the previously referred to value
      in a single atomic operation. This function can be used to program
      mutexes and similar locking mechanisms.</P>
    </DD>

    <DT>
      <TT>app <I>f</I> <I>re</I></TT>
    </DT>
    <DD>
      <P>Applies the function <TT>f</TT> to the value referred to by the
      reference <TT><I>re</I></TT>. Equivalent to <TT><I>f</I>
      (!<I>re</I>)</TT>.</P>
    </DD>

    <DT>
      <TT>map <I>f</I> <I>re</I></TT>
    </DT>
    <DD>
      <P>Makes the reference <TT><I>re</I></TT> refer to the value
      <TT><I>f a</I></TT>, where <TT><I>a</I></TT> is the value previously
      referred to. Note that this is <I>not</I> an atomic operation, since
      <TT><I>f</I></TT> might perform arbitrary computations.</P>
    </DD>

    <DT>
      <TT>equal (<I>re1</I>, <I>re2</I>)</TT>
    </DT>
    <DD>
      <P>An explicit equality function on references. Equivalent to <TT>op=</TT>.</P>
    </DD>
  </DL>

<?php section("also", "see also") ?>

  <DL><DD>
    <A href="general.php3">General</A>
  </DD></DL>

<?php footing() ?>
