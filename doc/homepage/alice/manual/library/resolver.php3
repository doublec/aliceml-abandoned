<?php include("macros.php3"); ?>
<?php heading("The Resolver structure", "The <TT>Resolver</TT> structure") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature RESOLVER
    structure Resolver : RESOLVER
  </PRE>

<?php section("import", "import") ?>

  <PRE>
    import signature RESOLVER from "x-alice:/lib/system/RESOLVER-sig"
    import signature Resolver from "x-alice:/lib/system/Resolver"
  </PRE>

<?php section("interface", "interface") ?>

  <PRE>
    signature RESOLVER =
    sig
	structure Handler : RESOLVER_HANDLER

	type resolver
	type t = resolver

	datatype result =
	    FILE of string
	  | STRING of string

	val new: string * Handler.handler list -> resolver
	val localize: resolver -> string -> result option
    end
  </PRE>

<?php section("description", "description") ?>

<?php section("also", "see also") ?>

  <DL><DD>
    <A href="resolver-handler.php3"><TT>RESOLVER_HANDLER</TT></A>,
    <A href="component.php3"><TT>Component</TT></A>
  </DD></DL>

<?php footing() ?>
