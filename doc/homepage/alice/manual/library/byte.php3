<?php include("macros.php3"); ?>
<?php heading("The Byte structure", "The <TT>Byte</TT> structure") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature BYTE
    structure Byte : BYTE
  </PRE>

  <P>
    The Standard ML Basis'
    <A href="http://SML.sourceforge.net/Basis/byte.html"><TT>Byte</TT></A> structure.
  </P>

  <P>See also:
    <A href="word.php3"><TT>Word8</TT></A>,
    <A href="char.php3"><TT>Char</TT></A>,
    <A href="string.php3"><TT>String</TT></A>,
    <A href="substring.php3"><TT>Substring</TT></A>,
    <A href="mono-vector.php3"><TT>CharVector</TT></A>,
    <A href="mono-vector-slice.php3"><TT>CharVectorSlice</TT></A>,
    <A href="mono-vector.php3"><TT>Word8Vector</TT></A>,
    <A href="mono-array.php3"><TT>Word8Array</TT></A>
  </P>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <PRE>
    signature BYTE =
    sig
	val byteToChar :      Word8.t -> char
	val charToByte :      char -> Word8.t
	val bytesToString :   Word8Vector.t -> string
	val stringToBytes :   string -> Word8Vector.t
	val unpackStringVec : Word8Vector.t * int * int option -> string
	val unpackString :    Word8Array.t * int * int option -> string
	val packString :      Word8Array.t * int * Substring.t -> unit
    end
  </PRE>

<?php section("description", "description") ?>

  <P>
    Like the Standard ML Basis'
    <A href="http://SML.sourceforge.net/Basis/byte.html"><TT>Byte</TT></A> structure.
  </P>

<?php footing() ?>
