<?php include("macros.php3"); ?>
<?php heading("The SUBSTRING signature", "The <TT>SUBSTRING</TT> signature") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature SUBSTRING
    structure Substring : SUBSTRING where String = String
    structure WideSubstring : SUBSTRING where String = WideString
  </PRE>

  <P>
    An extended version of the
    <A href="http://www.dina.kvl.dk/~sestoft/sml/substring.html">Standard ML
    Basis' <TT>SUBSTRING</TT></A> signature.
  </P>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <PRE>
    signature SUBSTRING =
    sig
	structure String : STRING

	type substring
	type t = substring							(**)

	val equal :     substring * substring -> bool
	val hash :      substring -> int

	val base :      substring -> String.string * int * int
	val string :    substring -> String.string
	val substring : String.string * int * int -> substring
	val extract :   String.string * int * int option -> substring

	val isEmpty :   substring -> bool
	val size :      substring -> int

	val all :       String.string -> substring
	val getc :      substring -> (String.Char.char * substring) option
	val first :     substring -> String.Char.char option
	val triml :     int -> substring -> substring
	val trimr :     int -> substring -> substring
	val sub :       substring * int -> String.Char.char
	val slice :     substring * int * int option -> substring
	val concat :    substring list -> String.string
	val explode :   substring -> String.Char.char list

	val isPrefix :  String.string -> substring -> bool
	val compare :   substring * substring -> order
	val collate :   (String.Char.char * String.Char.char -> order) -> substring * substring -> order

	val splitl :    (String.Char.char -> bool) -> substring -> substring * substring
	val splitr :    (String.Char.char -> bool) -> substring -> substring * substring
	val splitAt :   substring * int -> substring * substring
	val dropl :     (String.Char.char -> bool) -> substring -> substring
	val dropr :     (String.Char.char -> bool) -> substring -> substring
	val takel :     (String.Char.char -> bool) -> substring -> substring
	val taker :     (String.Char.char -> bool) -> substring -> substring

	val translate : (String.Char.char -> String.string) -> substring -> String.string

	val app :       (String.Char.char -> unit) -> substring -> unit
	val appr :      (String.Char.char -> unit) -> substring -> unit
	val foldl :     (String.Char.char * 'a -> 'a) -> 'a -> substring -> 'a
	val foldr :     (String.Char.char * 'a -> 'a) -> 'a -> substring -> 'a
    end
  </PRE>

<?php section("description", "description") ?>

  <P>
    Items not described here are as in the 
    <A href="http://www.dina.kvl.dk/~sestoft/sml/substring.html">Standard ML
    Basis' <TT>SUBSTRING</TT></A> signature.
  </P>

  <P>
    <I>Limitations:</I> The following standard functions are
    currently missing:
  </P>

  <UL>
    <LI><TT>position</TT></LI>
    <LI><TT>span</TT></LI>
    <LI><TT>tokens</TT>, <TT>fields</TT></LI>
  </UL>

  <DL>
    <DT>
      <TT>type t = substring</TT>
    </DT>
    <DD>
      <P>A local synonym for type <TT>substring</TT>.</P>
    </DD>

    <DT>
      <TT>equal (<I>ss1</I>, <I>ss2</I>)</TT>
    </DT>
    <DD>
      <P>An explicit equality function on subtrings. Equivalent to <TT>op=</TT>.</P>
    </DD>

    <DT>
      <TT>hash <I>ss</I></TT>
    </DT>
    <DD>
      <P>A hash function for substrings.</P>
    </DD>

    <DT>
      <TT>appr <I>f</I> <I>ss</I></TT>
    </DT>
    <DD>
      <P>Like <TT>app</TT>, but applies <TT><I>f</I></TT> in
      right to left order (i.e., decreasing indices). Equivalent to:</P>
      <PRE>
        List.appr <I>f</I> (explode <I>s</I>)</PRE>
    </DD>
  </DL>

<?php section("also", "see also") ?>

  <DL><DD>
    <A href="string.php3"><TT>STRING</TT></A>
  </DD></DL>

<?php footing() ?>
