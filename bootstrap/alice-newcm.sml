CM.make "$smlnj/cm/tools.cm";
structure CM = struct open CM; val make' = make end;

(*
Tools.registerStdShellCmdTool
	{ tool           = "Alice-to-SML"
	, class          = "alice"
	, suffixes       = ["aml"]
	, cmdStdPath     = case OS.Process.getEnv "AML2SML"
			    of SOME command => command
			     | NONE         => "./aml2sml"
	, template       = NONE
	, extensionStyle = Tools.EXTEND [("sml", SOME "sml", fn x => x)]
	, dflopts        = []
	};
*)

local
    val tool    = "Alice-to-SML"
    val class   = "alice"
    val suffix  = "aml"
    val command = case OS.Process.getEnv "AML2SML"
		    of SOME command => command
		     | NONE         => "./aml2sml"

    open Tools

    fun rewrite path =
	let
	    val {dir, file} = OS.Path.splitDirFile path
	    val  dir'       = OS.Path.joinDirFile {dir = dir, file = "NJ"}
	    val  file'      = OS.Path.joinBaseExt {base = OS.Path.base file,
						   ext = SOME "sml"}
	    val  path'      = OS.Path.joinDirFile {dir = dir', file = file'}
	in
	    path'
	end

    val rule : rule =
	fn {spec, context, native2pathmaker, defaultClassOf, ...} =>
	let
	    val {mkpath, opts, derived, ...} : spec = spec
	    val name        = srcpath (mkpath ())
	    val nativename  = nativeSpec name
	    val nativename' = rewrite nativename
	    val partial_expansion =
		({ smlfiles = [],
		   cmfiles  = [],
		   sources  = [(name, {class = class, derived = derived})] },
		 [{ name    = nativename',
		    mkpath  = native2pathmaker nativename',
		    class   = SOME "sml",
		    opts    = opts,
		    derived = true }])
	    fun runcmd () =
		let
		    val cmd = command ^ " " ^ nativename
		in
		    if OS.Process.system cmd = OS.Process.success
		    then ()
		    else raise ToolError {tool = tool, msg = cmd}
		end
	    fun rulefn () =
		(if outdated tool ([nativename'], nativename)
		 then runcmd ()
		 else ();
		 partial_expansion)
	in
	    context rulefn
	end
in
    val _ = registerClass (class, rule)
    val _ = registerClassifier (stdSfxClassifier {sfx = suffix, class = class})
end;
