<?php include("macros.php3"); ?>
<?php heading("The UNIQUE_STRING signature", "The <TT>UNIQUE_STRING</TT> signature") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature UNIQUE_STRING
    structure UniqueString : UNIQUE_STRING where String = String
    structure WideUniqueString : UNIQUE_STRING where String = WideString
  </PRE>

  <P>
    This structure provides an efficient representation for strings that
    guarantees that equal strings share their representation. Such strings
    are also known as <EM>internalized string</EM> or <EM>symbols</EM>.
  </P>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <PRE>
    signature UNIQUE_STRING =
    sig
	structure String : STRING

	eqtype unique_string
	type t = unique_string

	val unique :  String.string -> unique_string
	val string :  unique_string -> String.string

	val equal :   unique_string * unique_string -> bool
	val compare : unique_string * unique_string -> order
	val hash :    unique_string -> int
    end
  </PRE>

<?php section("description", "description") ?>

  <DL>
    <DT>
      <TT>eqtype unique_string</TT>
      <TT>type t = unique_string</TT>
    </DT>
    <DD>
      <P>The type of uniquely represented strings. Equality is defined
      structurally.</P>
    </DD>

    <DT>
      <TT>unique <I>s</I></TT>
    </DT>
    <DD>
      <P>Creates a unique string with content <TT><I>s</I></TT>. If a
      unique string with the same content already exists, that one is
      returned.</P>
    </DD>

    <DT>
      <TT>string <I>us</I></TT>
    </DT>
    <DD>
      <P>Extracts the content of the unique string <TT><I>s</I></TT>. It holds
      that</P>
      <PRE>
        string (unique <I>s</I>) = <I>s</I></PRE>
    </DD>

    <DT>
      <TT>equal (<I>us1</I>, <I>us2</I>)</TT>
    </DT>
    <DD>
      <P>An explicit equality function on unique strings.
      Equivalent to <TT>op=</TT>.</P>
    </DD>

    <DT>
      <TT>compare (<I>us1</I>, <I>us2</I>)</TT>
    </DT>
    <DD>
      <P>Performs a lexicographic comparison of the content of the unique
      strings. Equivalent to</P>
      <PRE>
        compare (string <I>us1</I>, string <I>us2</I>)</PRE>
    </DD>

    <DT>
      <TT>hash <I>us</I></TT>
    </DT>
    <DD>
      <P>A hash function for unique strings. Hashing is constant time.</P>
    </DD>
  </DL>

<?php section("also", "see also") ?>

  <DL><DD>
    <A href="string.php3"><TT>STRING</TT></A>,
    <A href="substring.php3"><TT>SUBSTRING</TT></A>
  </DD></DL>

<?php footing() ?>
