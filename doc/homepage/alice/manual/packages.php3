<?php include("macros.php3"); ?>

<?php heading("Packages", "packages") ?>



<?php section("overview", "overview") ?>

  <P>
    SML provides a strict static typing discipline. However, for particular
    programming tasks - especially when it comes to open programming -
    it is desirable to relax the rules and shift some of the type
    checking to runtime.
  </P>

  <P>
    Alice thus provides runtime typing. Arbitrary values can be wrapped
    as <I>packages</I> together with their type. These packages are
    first-class values that can be passed around und later be unpacked
    in which case a dynamic type check takes place.
  </P>

  <P>
    Unlike most other languages, Alice not only allows packaging of
    ordinary data structures, but also of arbitrary module values.
    This way, abstract types can be passed around for example.
  </P>

  <P>
    Packages and runtime types are made available through Alice's
    <A href="modules.php3">higher-order module system</A>.
  </P>



<?php section("package", "packages") ?>

  <P>
    Packages are provided through the structure
    <TT>Package</TT> which contains several functors:
  </P>

  <PRE>
	structure Package :
	sig
	    type package
	    type t = package

	    type mismatch
	    exception Mismatch of mismatch

	    functor Pack(signature S structure X : S) : (val package : package)
	    functor Unpack(val package : package signature S) : S

	    (* ... *)
	end
  </PRE>

  <P>
    By applying the <TT>Pack</TT> functor one can inject an arbitrary
    module into a package. For example:
  </P>

  <PRE>
	structure P = Package.Pack(structure X = Int64
	                           signature S = INTEGER)
  </PRE>

  <P>
    This functor application creates a package containing the complete
    structure <TT>Int64</TT>, specifying its signature as
    <TT>INTEGER</TT>. A less specific signature may be given, but the
    functor type enforces that the module matches that signature. This
    guarantees that the package is always typed consistently.
  <P>

  <P>
    Note that in this example <TT>INTEGER</TT> in fact is not the
    most specific signature possible. More specific would be
  </P>

  <PRE>
	signature S = INTEGER where type int = Int64.int
  </PRE>

  <P>
    Not specifying this equivalence for the package signature
    actually makes the type int in the package abstract. The original
    type cannot be regained in any way. It is thus best to think of
    this application as
  </P>

  <PRE>
	structure P = Package.Pack(structure X = Int64 :> INTEGER
	                           signature S = INTEGER)
  </PRE>

  <P>
    The resulting structure contains a single value <TT>package</TT>,
    which is a first-class core value. You can pass it to a function, for
    example:
  <P>

  <PRE>
	val x = foo(P.package)
  </PRE>

  <P>
    The function <TT>foo</TT> might be defined as follows:
  </P>

  <PRE>
	(* foo : package -> int *)
	fun foo p =
	    let
		structure MyInt = Package.Unpack(val package = p
		                                 signature S = INTEGER)
		val n = MyInt.*(MyInt.fromInt 1000, MyInt.fromInt 876)
	    in
		MyInt.toInt n
	    end
	    handle Package.Mismatch _ => 0
  </PRE>

  <P>
    Here, the package is unpacked via the functor <TT>Unpack</TT>. By
    specifying the argument signature <TT>S</TT> appropriately, the signature
    of <TT>MyInt</TT> is statically known to be <TT>INTEGER</TT>.
  </P>

  <P>
    If the package does not contain a module that matches the signature
    passed to <TT>Unpack</TT>, the functor application will raise an
    exception <TT>Mismatch</TT>. If desired, this exception can be
    handled in the usual way, as seen in the example. The argument
    to <TT>Mismatch</TT> is an abstract description of the reason for
    the match failure. Currently, this cannot be investigated.
  </P>

  <P>
    Note that packing is not restricted to structures: since Alice
    provides a true <A href="modules.php3">higher-order module system</A>,
    functors can be packed in the same way.
  </P>


<?php section("valpackage", "value packages") ?>

  <P>
    Often it is not necessary to package whole modules. For convenience,
    the <TT>Package</TT> module thus provides functors that allow packaging
    of simple core values. Essentially, this provides a type dynamic, as
    available in a few other languages.
  </P>

  <P>
    The respective functors are the following:
  </P>

  <PRE>
	structure Package :
	sig
	    (* ... *)

	    type val_package
 
	    exception MismatchVal
 
	    functor PackVal(type t val x : t) : (val package : val_package)
	    functor UnpackVal(val package : val_package type t) : (val x : t)
	end
  </PRE>

  <P>
    These are essentially just short hands for special cases of the more
    heavy-weight general functors. Their use is similar. Instead of the
    general <TT>Mismatch</TT> exception, the simpler <TT>MismatchVal</TT>
    is raised in case of type errors during unpacking.
  </P>


<?php section("limitations", "limitations") ?>

  <P>
    The package mechanism in Alice (as well as the
    <A href="pickling.php3">pickling</A> machinery) relies on runtime
    representations of types and signatures. Every type or signature
    declaration in Alice not only has a static semantics, but also
    a dynamic semantics (which closely mirrors the static one). Unlike
    SML there is no type erasure performed before evaluation.
  </P>

  <P>
    In particular, abstraction via <TT>:&gt;</TT> has an operational
    semantics (and cost) in Alice, as it performs abstraction on runtime
    types.
  </P>

  <P>
    As an implementation limitation of the current Alice system, the operational
    semantics of abstraction are currently not correctly implemented
    for cases where the signature is a parameter and thus not known
    statically. In that case, no runtime abstraction on type fields
    is performed. A simple example is:
  </P>

  <PRE>
	functor Abs(signature S structure X : S) = X :> S

	structure M  = struct type t = int val x = 13 end
	structure M' = Abs(signature = sig type t val x : t end
	                   structure = M)
  </PRE>

  <P>
    In this example, the runtime representation of <TT>M'.t</TT>
    will still be <TT>int</TT> and not an abstract type.
    However, since the static semantics work correctly, this can
    only be observed by going through a package:
  </P>

  <PRE>
	structure P = Package.PackVal(type t = M'.t val x = M'.x)
	structure X = Package.UnpackVal(val package = P.package type t = int)

	val y = X.x + 1    (* Aha! *)
  </PRE>

  <P>
    Unpacking should fail in this example, but currently does not.
  </P>

  <P>
    Since abstraction during packing uses the same mechanism,
    packing does not properly treat abstract types either.
    Unfortunately, this property does not only lead to a too permissive
    treatment of abstract types, but can also cause failure for
    examples that should work. Consider:
  </P>

  <PRE>
	structure A0 = struct type t = int val x = 9 end
	signature S  = sig type t val x : t end
	structure P  = Package.Pack(structure X = A0 signature S = S)

	structure A1 = Package.Unpack(val package = P.package
				      signature S = S)
	structure A2 = Package.Unpack(val package = P.package
				      signature S = S where type t = A1.t)
  </PRE>

  <P>
    The second unpack uses the type found when unpacking the first time.
    This way, A1.t and A2.t should be the same, so that A1.x and A2.x are
    compatible. One would expect that the second unpacking always succeeds
    since we are unpacking the same package and already know its type
    <TT>t</TT>. However, since no abstraction is performed on <TT>t</TT>
    during packing, <TT>A1.t</TT> actually is <TT>int</TT>. So we are
    trying to unpack with <TT>t = int</TT>. But the signature stored in the
    package does not specify this equivalence, so signature matching will
    fail during unpacking.
  </P>

  <P>
    The problem is that the package signature and the runtime types
    contained in the package module are inconsistent, if the signature
    passed to <TT>Pack</TT> does not declare the most specific types. So
    a simple workaround is to avoid this situation. For our example, this
    can be done as follows:
  </P>

  <PRE>
	structure P = Package.Pack(structure X = A0
				   signature S = S where type t = X.t)
  </PRE>
<!--
  <P>
    Or if you actually want the abstraction:
  </P>

  <PRE>
	structure P = Package.Pack(structure X = A0 :> S
				   signature S = S where type t = X.t)
  </PRE>
-->
  <P>
    This bug will be corrected very shortly.
    Note that it does not compromise soundness, though.
  </P>


<?php footing() ?>
