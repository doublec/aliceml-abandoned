<?php include("macros.php3"); ?>
<?php heading("The <TT>Bool</TT> structure", "The <TT>Bool</TT> structure") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature BOOL
    structure Bool : BOOL
  </PRE>

  <P>
    An extended version of the
    <A href="http://www.dina.kvl.dk/~sestoft/sml/bool.html">Standard ML
    Basis' <TT>Bool</TT></A> structure.
  </P>

  <P>
    The type <TT>bool</TT> and its constructors, 
    as well as the function <TT>not</TT> are available in the
    <A href="toplevel.php3">top-level environment</A>.
  </P>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <PRE>
    signature BOOL =
    sig
	datatype bool = false | true
	type     t    = bool

	val equal :      bool * bool -> bool
	val compare :    bool * bool -> order

	val not :        bool -> bool

	val toString :   bool -> string
	val fromString : string -> bool option
	val scan :       (char,'a) StringCvt.reader -> (bool,'a) StringCvt.reader
    end
  </PRE>

<?php section("description", "description") ?>

  <P>
    Items not described here are as in the 
    <A href="http://www.dina.kvl.dk/~sestoft/sml/bool.html">Standard ML
    Basis' <TT>Bool</TT></A> structure.
  </P>

  <DL>
    <DT>
      <TT>type t = bool</TT>
    </DT>
    <DD>
      <P>A local synonym for type <TT>bool</TT>.</P>
    </DD>

    <DT>
      <TT>equal (<I>b1</I>, <I>b2</I>)</TT>
    </DT>
    <DD>
      <P>An explicit equality function on bools. Equivalent to <TT>op=</TT>.</P>
    </DD>

    <DT>
      <TT>compare (<I>b1</I>, <I>b2</I>)</TT>
    </DT>
    <DD>
      <P>An ordering function on bools. It defines <TT>false < true</TT>.
    </DD>
  </DL>

<?php footing() ?>
