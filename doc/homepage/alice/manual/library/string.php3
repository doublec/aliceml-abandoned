<?php include("macros.php3"); ?>
<?php heading("The STRING signature", "The <TT>STRING</TT> signature") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature STRING
    structure String : STRING where type string = string
                              where Char = Char
    structure WideString : STRING where Char = WideChar
  </PRE>

  <P>
    An extended version of the
    <A href="http://www.dina.kvl.dk/~sestoft/sml/string.html">Standard ML
    Basis' <TT>STRING</TT></A> signature.
  </P>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <PRE>
    signature STRING =
    sig
	type string
	type t = string

	structure Char :  CHAR

	val maxSize :     int

	val size :        string -> int
	val str :         Char.char -> string
	val sub :         string * int -> Char.char
	val substring :   string * int * int -> string
	val extract :     string * int * int option -> string

	val op ^ :        string * string -> string
	val concat :      string list -> string
	val concatWith :  string -> string list -> string
	val implode :     Char.char list -> string
	val explode :     string -> Char.char list
	val tabulate :    int * (int -> Char.char) -> string

	val map :         (Char.char -> Char.char) -> string -> string
	val translate :   (Char.char -> string) -> string -> string
	val fields :      (Char.char -> bool) -> string -> string list
	val tokens :      (Char.char -> bool) -> string -> string list

	val op < :        string * string -> bool
	val op > :        string * string -> bool
	val op <= :       string * string -> bool
	val op >= :       string * string -> bool
	val equal :       string * string -> bool
	val compare :     string * string -> order
	val collate :     (Char.char * Char.char -> order) -> string * string -> order
	val isPrefix :    string -> string -> bool
	val isSuffix :    string -> string -> bool
	val hash :        string -> int

	val toWide :      string -> WideString.string
	val fromWide :    WideString.string -> string

	val toString :    string -> string
	val toCString :   string -> string
	val fromString :  string -> string option
	val fromCString : string -> string option
	val scan :        (char,'a) StringCvt.reader -> (string,'a) StringCvt.reader
	val scanC :       (char,'a) StringCvt.reader -> (string,'a) StringCvt.reader
    end
  </PRE>

<?php section("description", "description") ?>

  <P>
    Items not described here are as in the 
    <A href="http://www.dina.kvl.dk/~sestoft/sml/string.html">Standard ML
    Basis' <TT>STRING</TT></A> signature.
  </P>

  <DL>
    <DT>
      <TT>type t = string</TT>
    </DT>
    <DD>
      <P>A local synonym for type <TT>string</TT>.</P>
    </DD>

    <DT>
      <TT>concatWith <I>s</I> <I>l</I></TT>
    </DT>
    <DD>
      <P>Returns the concatenation of the strings in the list
      <TT><I>l</I></TT> using the string <TT><I>s</I></TT> as a separator.</P>
    </DD>

    <DT>
      <TT>tabulate (<I>n</I>, <I>f</I>)</TT>
    </DT>
    <DD>
      <P>Creates a string of size <TT><I>n</I></TT>, where the characters are
      defined in order of increasing index by applying <TT><I>f</I></TT> to the
      character's index. This is equivalent to the expression:</P>
      <PRE>
        implode (List.tabulate (<I>n</I>, <I>f</I>))</PRE>
      <P>If <TT><I>n</I></TT> &lt; 0 or <TT>maxSize</TT> &lt; <TT><I>n</I></TT>,
      then the <TT>Size</TT> exception is raised. </P>
    </DD>

    <DT>
      <TT>equal (<I>s1</I>, <I>s2</I>)</TT>
    </DT>
    <DD>
      <P>An explicit equality function on strings. Equivalent to <TT>op=</TT>.</P>
    </DD>

    <DT>
      <TT>hash <I>s</I></TT>
    </DT>
    <DD>
      <P>A hash function for strings.</P>
    </DD>

    <DT>
      <TT>isSuffix <I>s1</I> <I>s2</I></TT>
    </DT>
    <DD>
      <P>Returns <TT>true</TT> if the string <TT><I>s1</I></TT> is a suffix
      of the string <TT><I>s2</I></TT>. Note that the empty string is a
      suffix of any string, and that a string is a suffix of itself.</P>
    </DD>

    <DT>
      <TT>scan <I>getc</I> <I>strm</I></TT> <BR>
      <TT>scanC <I>getc</I> <I>strm</I></TT>
    </DT>
    <DD>
      <P>Scans a string as an SML (C) source program string, converting escape
      sequences into the appropriate characters. These functions are similar to
      <TT>fromString</TT> and <TT>fromCString</TT>, but can convert from
      arbitrary streams.</P>
    </DD>

    <DT>
      <TT>toWide <I>s</I></TT> <BR>
      <TT>fromWide <I>s</I></TT>
    </DT>
    <DD>
      <P>Convert between the standard and the wide character set. Raise
      <TT>Chr</TT> if any character of the given string is not representable
      in the target character set.</P>
    </DD>
  </DL>

<?php section("also", "see also") ?>

  <DL><DD>
    <A href="mono-vector.php3"><TT>MONO_VECTOR</TT></A>,
    <A href="substring.php3"><TT>SUBSTRING</TT></A>,
    <A href="unique-string.php3"><TT>UNIQUE_STRING</TT></A>,
    <A href="string-cvt.php3"><TT>StringCvt</TT></A>
  </DD></DL>

<?php footing() ?>
