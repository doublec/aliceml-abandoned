<?php include("macros.php3"); ?>
<?php heading("The Url structure", "The <TT>Url</TT> structure") ?>

<?php section("synopsis", "synopsis") ?>

  <PRE>
    signature URL
    structure Url : URL
  </PRE>

<?php section("import", "import") ?>

  <PRE>
    import signature URL from "x-alice:/lib/utility/URL-sig"
    import signature Url from "x-alice:/lib/utility/Url"
  </PRE>

<?php section("interface", "interface") ?>

  <PRE>
    signature URL =
    sig
	eqtype url
	type t = url

	type scheme = string option
	type authority = string option
	type device = char option
	type path = string list
	type query = string option
	type fragment = string option

	exception Malformed

	val empty: url
	val fromString: string -> url
	val toString: url -> string
	val toStringRaw: url -> string
	val isAbsolute: url -> bool
	val resolve: url -> url -> url
	val equal: url * url -> bool
	val compare: url * url -> order
	val hash: url -> int

	val getScheme: url -> scheme
	val getAuthority: url -> authority
	val getDevice: url -> device
	val isAbsolutePath: url -> bool
	val getPath: url -> path
	val getQuery: url -> query
	val getFragment: url -> fragment

	val setScheme: url * scheme -> url
	val setAuthority: url * authority -> url
	val setDevice: url * device -> url
	val makeAbsolutePath: url -> url
	val makeRelativePath: url -> url
	val setPath: url * path -> url
	val setQuery: url * query -> url
	val setFragment: url * fragment -> url
    end
  </PRE>

<?php section("description", "description") ?>

<?php footing() ?>
