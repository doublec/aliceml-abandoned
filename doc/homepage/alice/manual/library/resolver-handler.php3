<?php include("macros.php3"); ?>
<?php heading("The RESOLVER_HANDLER signature",
	      "The <TT>RESOLVER_HANDLER</TT> signature") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature RESOLVER_HANDLER
  </PRE>

<?php section("import", "import") ?>

  <PRE>
    import signature RESOLVER_HANDLER from "x-alice:/lib/system/RESOLVER_HANDLER-sig"
  </PRE>

<?php section("interface", "interface") ?>

  <PRE>
    signature RESOLVER_HANDLER =
    sig
	type handler
	type t = handler

	exception Syntax

	val default: handler
	val root: string -> handler
	val cache: string -> handler
	val prefix: string * string -> handler
	val pattern: string * string -> handler

	val parse: string -> handler list
    end
  </PRE>

<?php section("description", "description") ?>

<?php section("also", "see also") ?>

  <DL><DD>
    <A href="resolver.php3">Resolver</A>
  </DD></DL>

<?php footing() ?>
