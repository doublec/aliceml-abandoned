<?php include("macros.php3"); ?>

<?php heading("Laziness", "laziness") ?>



<?php section("overview", "overview") ?>

  <P>
    Although ML is a strict language, Alice provides full support for
    lazy evaluation. A lazy computation is created using the keyword
    <TT>lazy</TT>:
  </P>

  <TABLE>
    <TR>
      <TD> <I>exp</I> </TD>
      <TD align="center">::=</TD>
      <TD> ... </TD>
      <TD> </TD>
    </TR>
    <TR>
      <TD></TD> <TD></TD>
      <TD> <TT>lazy</TT> <I>exp</I> </TD>
      <TD> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;lazy expression </TD>
    </TR>
  </TABLE>

  <P>
    This expression form evaluates to a place-holder for the value of
    <I>exp</I> (a so-called future). The expression will not be evaluated
    before some operation first tries to access this value, however.
  </P>

  <P>
    If evaluation of the expression terminates with an exception
    <I>e</I>, any attempt to access the result value will cause an exception
    <TT>Future(</TT><I>e</I><TT>)</TT> to be raised.
  </P>

  <P>
    There also is syntactic sugar for declaring lazy functions using the 
    <TT>fun</TT> syntax:
  </P>

  <TABLE>
    <TR>
      <TD> <I>fvalbind</I> </TD>
      <TD align="center">::=</TD>
      <TD align="right"> &lt;<TT>lazy</TT>&gt; </TD>
      <TD> &lt;<TT>op</TT>&gt; <I>vid atpat ... atpat</I> &lt;<TT>:</TT> ty&gt;
	   <TT>=</TT> <I>exp</I> </TD>
    </TR><TR>
      <TD></TD><TD></TD>
      <TD align="right"> <TT>|</TT> </TD>
      <TD> &lt;<TT>op</TT>&gt; <I>vid atpat ... atpat</I> &lt;<TT>:</TT> ty&gt;
	   <TT>=</TT> <I>exp</I> </TD>
    </TR><TR>
      <TD></TD><TD></TD>
      <TD align="right"> <TT>|</TT> </TD>
      <TD> ... </TD>
    </TR><TR>
      <TD></TD><TD></TD>
      <TD align="right"> <TT>|</TT> </TD>
      <TD> &lt;<TT>op</TT>&gt; <I>vid atpat ... atpat</I> &lt;<TT>:</TT> ty&gt;
	   <TT>=</TT> <I>exp</I> </TD>
    </TR>
  </TABLE>

  <P>
    Note that by-need evaluation is just one aspect of Alice's
    <A href="futures.php3">future mechanism</A>.
  </P>



<?php section("examples", "examples") ?>

  <P>
    The following code lazily generates an infinite list of integers:
  </P>

  <PRE>
	fun enumFrom n = lazy n :: enumFrom(n+1)

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
	  | mapl f (x::xs) = lazy f x :: mapl f xs
  </PRE>

  <P>
    However, this version is still strict in the head of the list. A fully
    lazy version is:
  </P>

  <PRE>
	fun mapl' f xs = lazy (case xs of
	                             []   => nil
	                          | x::xs => f x :: mapl' f xs)
  </PRE>

  <P>
    which is equivalent to the more readable formulation
  </P>

  <PRE>
	fun lazy mapl f   []    = nil
	       | mapl f (x::xs) = f x :: mapl f xs
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
