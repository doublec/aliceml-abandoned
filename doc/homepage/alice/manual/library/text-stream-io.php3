<?php include("macros.php3"); ?>
<?php heading("The TEXT_STREAM_IO signature", "The <TT>TEXT_STREAM_IO</TT> signature") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature TEXT_STREAM_IO
    structure TextIO.StreamIO : TEXT_STREAM_IO
  </PRE>

  <P>
    The
    <A href="http://www.dina.kvl.dk/~sestoft/sml/text-stream-io.html">Standard ML
    Basis' <TT>TEXT_STREAM_IO</TT></A> signature.
  </P>

  <P>See also:
    <A href="stream-io.php3"><TT>STREAM_IO</TT></A>,
    <A href="text-io.php3"><TT>TextIO</TT></A>
  </P>

<?php section("import", "import") ?>

  <P>
    Imported implicitly.
  </P>

<?php section("interface", "interface") ?>

  <PRE>
    signature TEXT_STREAM_IO =
    sig
	type elem   = char
	type vector = string

	type instream
	type pos
	type reader

	type outstream
	type out_pos
	type writer

	val input :         instream -> string * instream
	val input1 :        instream -> (char * instream) option
	val inputN :        instream * int -> string * instream
	val inputLine :     instream -> string * instream
	val inputAll :      instream -> string * instream
	val canInput :      instream * int -> int option
	val closeIn :       instream -> unit
	val endOfStream :   instream -> bool
	val mkInstream :    reader * string -> instream
	val getReader :     instream -> reader * string

	val output :        outstream * string -> unit
	val output1 :       outstream * char -> unit
	val outputSubstr :  outstream * substring -> unit
	val flushOut :      outstream -> unit
	val closeOut :      outstream -> unit
	val setBufferMode : outstream * IO.buffer_mode -> unit
	val getBufferMode : outstream -> IO.buffer_mode
	val mkOutstream :   writer * IO.buffer_mode -> outstream
	val getWriter :     outstream -> writer * IO.buffer_mode
	val getPosOut :     outstream -> out_pos
	val setPosOut :     out_pos -> outstream
    end
  </PRE>

<?php section("description", "description") ?>

  <P>
    Like the
    <A href="http://www.dina.kvl.dk/~sestoft/sml/text-stream-io.html">Standard ML
    Basis' <TT>TEXT_STREAM_IO</TT></A> signature.
  </P>

  <P>
    <I>Limitations:</I> The following standard functions are
    currently missing:
  </P>

  <UL>
    <LI><TT>filePosIn</TT>, <TT>filePosOut</TT></LI>
  </UL>

<?php footing() ?>
