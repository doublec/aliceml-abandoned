<?php include("macros.php3"); ?>

<?php heading("Stockhausen Operette 1 - Laziness", "laziness") ?>



<?php section("overview", "overview") ?>

  <P>
    Although ML is a strict language, Alice provides full support for
    lazy evaluation. A lazy computation is created through the predefined
    procedure
  </P>

  <PRE>
	val byneed: (unit -> 'a) -> 'a 
  </PRE>

  <P>
    It takes a procedure and returns a handle for its result (a so-called
    future). The procedure will be evaluated when some operation first tries
    to access this result.
  </P>

  <P>
    If the procedure passed to <TT>byneed</TT> terminates with an exception
    <I>e</I>, any attempt to access the result value will cause an exception
    <TT>Future(</TT><I>e</I><TT>)</TT> to be raised.
  </P>

  <P>
    Note that by-need evaluation is just one aspect of Alice's
    <A href="futures.php3">future mechanism</A>.
  </P>



<?php section("examples", "examples") ?>

  <P>
    The following code lazily generates an infinite list of integers:
  </P>

  <PRE>
	fun enum n = n :: byneed(fn() => enum(n+1))

	val ns = enum 0
  </PRE>

  <P>
    Printing this list will produce infinite output:
  </P>

  <PRE>
	List.app (fn n => print(Int.toString n ^ "\n")) ns
  </PRE>

  <P>
    Here is lazy variant of <TT>map</TT> that can deal with infinite lists:
  </P>

  <PRE>
	fun mapl f   []    = nil
	  | mapl f (x::xs) = f x :: byneed(fn() => mapl f xs)
  </PRE>


<?php footing() ?>
