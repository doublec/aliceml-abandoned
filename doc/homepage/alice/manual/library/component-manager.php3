<?php include("macros.php3"); ?>
<?php heading("The COMPONENT_MANAGER signature",
	      "The <TT>COMPONENT_MANAGER</TT> signature") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature COMPONENT_MANAGER
  </PRE>

<?php section("import", "import") ?>

  <PRE>
    import signature COMPONENT_MANAGER from "x-alice:/lib/system/COMPONENT_MANAGER-sig"
  </PRE>

<?php section("interface", "interface") ?>

  <PRE>
    signature COMPONENT_MANAGER =
    sig
	exception Conflict
	exception Mismatch of {component : Url.t,
			       request : Url.t option,
			       cause : Inf.mismatch}

	type component

	functor Eval(val component : component
		     val url : Url.t
		     signature S) : S

	functor Link(val url : Url.t
		     signature S) : S

	functor Enter(val url: Url.t
		      signature S
		      structure X : S) : any

	val eval : Url.t * component -> Reflect.value
	val link : Url.t -> component
	val enter : Url.t * component -> unit
	val lookup : Url.t -> component option

	val start : Url.t -> unit
    end
  </PRE>

<?php section("description", "description") ?>

<?php section("also", "see also") ?>

  <DL><DD>
    <A href="component.php3"><TT>Component</TT></A>
  </DD></DL>

<?php footing() ?>
