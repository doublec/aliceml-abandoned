<?php include("macros.php3"); ?>
<?php heading("The Remote structure",
	      "The <TT>Remote</TT> structure") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature REMOTE
    structure Remote : REMOTE
  </PRE>

  <P>
    This structure provides functions for implementing distributed
    applications.  This includes exporting values from and importing values
    into sites, performing remote function application and starting a new
    site.
  </P>

<?php section("import", "import") ?>

  <PRE>
    import signature REMOTE from "x-alice:/lib/distribution/REMOTE-sig"
    import structure Remote from "x-alice:/lib/distribution/Remote"
  </PRE>

<?php section("interface", "interface") ?>

  <PRE>
    signature REMOTE =
    sig
	type <A href="#ticket">ticket</A> = string

	exception <A href="#Ticket-exn">Ticket</A>
	exception <A href="#Protocol">Protocol</A> of int * string

	val <A href="#proxy">proxy</A> : ('a -> 'b) -> 'a -> 'b
	val <A href="#offer">offer</A> : Package.package -> string
	val <A href="#take">take</A> : string -> Package.package

	functor <A href="#Offer-fn">Offer</A>(signature S  structure X : S) :
	    sig  val ticket : ticket  end

	functor <A href="#Take-fn">Take</A>(val ticket : ticket  signature S) : S

	functor <A href="#Execute">Execute</A>(val host : string
			signature RESULT
			functor Start(ComponentManager :
				      COMPONENT_MANAGER) : RESULT) : RESULT
    end
  </PRE>

<?php section("description", "description") ?>

  <DL>
    <DT>
      <TT>type <A name="ticket">ticket</A> = string</TT>
    </DT>
    <DD>
      <P>The type of tickets representing offered values.  Tickets are
	short strings suitable for communication, for instance, over voice
	lines or by email.  Tickets are suitable for parsing as URLs.</P>
    </DD>
  </DL>

<?php section("also", "see also") ?>

  <DL><DD>
    <A href="component-manager.php3"><TT>COMPONENT_MANAGER</TT></A>
  </DD></DL>

<?php footing() ?>
