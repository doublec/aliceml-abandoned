<?php include("macros.php3"); ?>

<?php heading("Laziness", "laziness") ?>



<?php section("overview", "overview") ?>

  <P>
    Although ML is a strict language, Alice provides full support for
    lazy evaluation. A lazy computation is created through the predefined
    procedure
  </P>

  <PRE>
	val byneed : (unit -> 'a) -> 'a 
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
	fun enumFrom n = byneed(fn() => n :: enumFrom(n+1))

	val ns = enumFrom 0
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
	  | mapl f (x::xs) = byneed(fn() => f x :: mapl f xs)
  </PRE>


<?php section("modules", "modules") ?>

  <P>
    Laziness is not only available in the core language, but in the module
    language as well. There is the predefined
    <A href="modules.php3#higher">higher-order functor</A>
  </P>

  <PRE>
	functor ByNeed(signature S functor F() : S) : S
  </PRE>

  <P>
    that can be used to evaluate any module expression lazily. For example,
    consider you want to provide a module whose execution is very costly,
    but which is rarely needed. You can let it execute lazily as follows:
  </P>

  <PRE>
	signature EXPENSIVE = ...

	structure Expensive :> EXPENSIVE =
	    let
		functor It() =
		struct
		    ...
		end
	    in
	        ByNeed(signature S = EXPENSIVE functor F = It)
	    end
  </PRE>

  <P>
    Note that signature abstraction has operational semantics in
    Alice (because of <A href="packages.php3">runtime types</A>), but is
    always performed lazily, so that the suspension is not forced by
    the mere presence of a signature annotation.
  </P>


<?php footing() ?>
