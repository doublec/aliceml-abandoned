<?php include("macros.php3"); ?>
<?php heading("The StringCvt structure", "The <TT>StringCvt</TT> structure") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature STRING_CVT
    structure StringCvt : STRING_CVT
  </PRE>

  <P>
    The
    <A href="http://www.dina.kvl.dk/~sestoft/sml/string-cvt.html">Standard ML
    Basis' <TT>StringCvt</TT></A> structure.
  </P>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <PRE>
    signature STRING_CVT =
    sig
	datatype radix   = BIN | OCT | DEC | HEX
	datatype realfmt = EXACT | SCI of int option | FIX of int option | GEN of int option

	type ('a,'b) reader = 'b -> ('a * 'b) option
	type cs

	val padLeft :    char -> int -> string -> string
	val padRight :   char -> int -> string -> string
	val splitl :     (char -> bool) -> (char,'a) reader ->'a -> string * 'a
	val takel :      (char -> bool) -> (char,'a) reader -> 'a -> string
	val dropl :      (char -> bool) -> (char,'a) reader -> 'a -> 'a
	val skipWS :     (char,'a) reader -> 'a -> 'a
	val scanString : ((char,cs) reader -> ('a,cs) reader) -> string -> 'a option
    end
  </PRE>

<?php section("description", "description") ?>

  <P>
    All items are as described in the 
    <A href="http://www.dina.kvl.dk/~sestoft/sml/string-cvt.html">Standard ML
    Basis' <TT>StringCvt</TT></A> structure.
  </P>

<?php section("also", "see also") ?>

  <DL><DD>
    <A href="string.php3"><TT>STRING</TT></A>,
    <A href="substring.php3"><TT>SUBSTRING</TT></A>
  </DD></DL>

<?php footing() ?>
