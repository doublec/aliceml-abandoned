<?php include("macros.php3"); ?>
<?php heading("The <TT>IEEEReal</TT> structure", "The <TT>IEEEReal</TT> structure") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature IEEE_REAL
    structure IEEEReal : IEEE_REAL
  </PRE>

  <P>
    The
    <A href="http://www.dina.kvl.dk/~sestoft/sml/ieee-float.html">Standard ML
    Basis' <TT>IEEEReal</TT></A> structure.
  </P>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <PRE>
    signature IEEE_REAL =
    sig
	 exception Unordered

	 datatype real_order    = LESS | EQUAL | GREATER | UNORDERED
	 datatype nan_mode      = QUIET | SIGNALLING
	 datatype float_class   = NAN of nan_mode | INF | ZERO | NORMAL | SUBNORMAL
	 datatype rounding_mode = TO_NEAREST | TO_NEGINF | TO_POSINF | TO_ZERO
	 type decimal_approx    = {kind : float_class, sign : bool, digits : int list,  exp : int}
    end
  </PRE>

<?php section("description", "description") ?>

  <P>
    Like the
    <A href="http://www.dina.kvl.dk/~sestoft/sml/ieee-float.html">Standard ML
    Basis' <TT>IEEEReal</TT></A> structure.
  </P>

  <P>
    <I>Limitations:</I> The following standard functions are
    currently missing:
  </P>

  <UL>
    <LI><TT>getRoundingMode</TT>, <TT>setRoundingMode</TT></LI>
    <LI><TT>toString</TT>, <TT>fromString</TT></LI>
  </UL>

<?php section("also", "see also") ?>

  <DL><DD>
    <A href="real.php3"><TT>Real</TT></A>
  </DD></DL>

<?php footing() ?>
