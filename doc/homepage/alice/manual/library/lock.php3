<?php include("macros.php3"); ?>
<?php heading("The Lock structure", "The <TT>Lock</TT> structure") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature LOCK
    structure Lock : LOCK
  </PRE>

  <P>
    The <TT>Lock</TT> structure defines a simple means to implement
    monitor-like synchronisation of arbitrary sets of functions.
  </P>

  <P>
    See the <A href="#example">example</A> at the and of this page for
    a common idiom for building up such synchronisation.
  </P>

  <P>See also:
    <A href="future.php3"><TT>Future</TT></A>,
    <A href="thread.php3"><TT>Thread</TT></A>
  </P>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <PRE>
    signature LOCK =
    sig
	type lock
	type t = lock

	val lock : unit -> lock
	val sync : lock -> ('a -> 'b) -> ('a -> 'b)
    end
  </PRE>

<?php section("description", "description") ?>

  <DL>
    <DT>
      <TT>type lock</TT> <BR>
      <TT>type t = alt</TT>
    </DT>
    <DD>
      <P>The type of synchronisation locks.</P>
    </DD>

    <DT>
      <TT>lock ()</TT>
    </DT>
    <DD>
      <P>Creates a new lock.</P>
    </DD>

    <DT>
      <TT>sync <I>lock</I> <I>f</I></TT>
    </DT>
    <DD>
      <P>Returns a function <TT><I>f'</I></TT>, that has the same behaviour as
      <TT><I>f</I></TT>, but is synchronised with respect to
      <TT><I>lock</I></TT>. Only one function per lock can be evaluated at one
      time, other threads will block until that function returns.
      Locking is <EM>not</EM> reentrant.</P>
    </DD>
  </DL>

<?php section("example", "example") ?>

  <P>
    The following structure provides two synchronised functions <TT>even</TT>
    and <TT>odd</TT>. Only one thread can run within the implementation of
    these functions.
  </P>

  <PRE>
    signature MONITOR =
    sig
      val even : int -> unit
      val odd :  int -> unit
    end

    structure Monitor : MONITOR =
    struct
      fun even 0 = ()
        | even n = odd(n-1)
      and odd 0  = ()
        | odd n  = even(n-1)

      val lock = Lock.lock()
      val even = Lock.sync lock even
      val odd  = Lock.sync lock odd
    end
  </PRE>

<?php footing() ?>
