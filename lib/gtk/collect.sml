(*
 * Authors:
 *   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
 *
 * Copyright:
 *   Thorsten Brunklaus, 2000
 *
 * Last Change:
 *   $Date$ by $Author$
 *   $Revision$
 *
 *)

signature COLLECT =
    sig
	val alicegtk : string -> unit
    end

structure Collect : COLLECT =
    struct
	open Ast
	open Tables

	datatype CType =
	    POINTER  of CType
	  | VALUE    of string
	  | FUNCARGS of CType * (ARGVAL list)
	  | FUNCTION of string * CType
	  | IGNORED
	and ARGVAL = ARG of CType * int

	datatype PTRVAL = PNormal | PObject | PString

	fun intKindToString CHAR       = "char"
	  | intKindToString SHORT      = "short"
	  | intKindToString INT        = "int"
	  | intKindToString LONG       = "long"
	  | intKindToString LONGLONG   = "long long"
	  | intKindToString FLOAT      = "float"
	  | intKindToString DOUBLE     = "double"
	  | intKindToString LONGDOUBLE = "long double"
	    
	fun signToString SIGNED   = ""
	  | signToString UNSIGNED = "unsigned "
	    
	fun tidToString(i, t : Tables.tidtab) =
	    (case Tidtab.find(t, i) of
		 SOME({name=NONE, ...})     => Tid.toString i
	       | SOME({name=SOME(id), ...}) => id
	       | NONE                       => Tid.toString i)

	fun transform (Void, tab)                    = VALUE("void")
	  | transform (Ellipses, tab)                = IGNORED
	  | transform (Qual(_, t), tab)              = transform(t, tab)
	  | transform (Numeric(_, _, s, ik, _), tab) =
	    VALUE((signToString s) ^ (intKindToString ik))
	  | transform (Array(_, _), tab)             = VALUE("Array")
	  | transform (Pointer(t), tab)              = POINTER(transform(t, tab))
	  | transform (Function(rt, args), tab)      = FUNCARGS(transform(rt, tab),
								transArgs(args, tab, 0))
	  | transform (StructRef(i), tab)            = VALUE(tidToString(i, tab))
	  | transform (UnionRef(i), tab)             = VALUE(tidToString(i, tab))
	  | transform (EnumRef(i), tab)              = VALUE(tidToString(i, tab))
	  | transform (TypeRef(i), tab)              = VALUE(tidToString (i, tab))
	  | transform (Error, tab)                   = IGNORED
	and transArgs (nil, tab, i)   = nil
	  | transArgs (a::ar, tab, i) = (ARG(transform(a, tab), i))::transArgs(ar, tab, (i + 1))

	fun collectDecl (TypeDecl(_), tab)        = IGNORED
	  | collectDecl (VarDecl(var, expr), tab) =
	    let
		val transRes = transform ((#ctype var), tab)
	    in
		case transRes of
		    FUNCARGS(_) => FUNCTION(Symbol.name (#name var), transRes)
		  | _           => transRes
	    end

	fun collect' (nil, tab)                                    = nil
	  | collect' (((DECL(ExternalDecl(decl), _, _))::Xr, tab)) = (collectDecl (decl, tab)::
								      collect' (Xr, tab))
	  | collect' ((DECL(_, _, _))::Xr, tab)                    = (print "ignored entry\n";
								      collect' (Xr, tab))
	    
	fun firstUpper s =
	    String.implode (case String.explode s of
				c::cr => (Char.toUpper c)::cr
			      | nil   => nil)

	fun firstLower s =
	    String.implode (case String.explode s of
				c::cr => (Char.toLower c)::cr
			      | nil   => nil)

	fun hasPrefix (p, FUNCTION(s, t)) =
	    (case String.tokens (fn #"_" => true | _ => false) s of
		 s::_ => (case String.compare(p, s) of
			      EQUAL => true
			    | _     => false)
	       | _    => false)
	  | hasPrefix (p, _)              = false
		 
	fun cNameToOzName(f, s) =
	    let
		val tokens = String.tokens (fn #"_" => true | _ => false) s
	    in
		String.concat (map firstUpper (List.filter f tokens))
	    end

	local
	    fun transArgs (FUNCARGS(rt, (ARG(VALUE("void"), _)::nil))) = FUNCARGS(rt, nil)
	      | transArgs f                                            = f
	in
	    fun transName (f, FUNCTION(s, t)) = FUNCTION(cNameToOzName(f, s), transArgs t)
	      | transName (f, v)              = v
	end

	fun glbFilter(FUNCTION("gtk_signal_connect", _))         = false
	  | glbFilter(FUNCTION("gtk_signal_disconnect", _))      = false
	  | glbFilter(FUNCTION("gtk_signal_handler_block", _))   = false
	  | glbFilter(FUNCTION("gtk_signal_handler_unblock", _)) = false
	  | glbFilter(FUNCTION("gtk_signal_emit", _))            = false
	  | glbFilter(FUNCTION("gtk_exit", _))                   = false
	  | glbFilter(FUNCTION("gtk_main", _))                   = false
	  | glbFilter(FUNCTION("gtk_main_quit", _))              = false
	  | glbFilter _                                          = true

	fun collect file =
	    let
		val tree  = ParseToAst.fileToAst file
		val ast   = #ast tree
		val tab   = #tidtab tree
		val filFn  = fn IGNORED => false | VALUE(_) => false | _ => true
		val funcs = List.filter filFn (collect' (ast, tab))
		val gtkfs = List.filter glbFilter (List.filter (fn x => hasPrefix("gtk", x)) funcs)
		val gdkfs = List.filter (fn x => hasPrefix("gdk", x)) funcs
	    in
		(map (fn x => transName((fn "gtk" => false | _ => true), x)) gtkfs,
		 map (fn x => transName((fn "gdk" => false | _ => true), x)) gdkfs)
	    end

	val ptrLs          = ["char", "unsigned char",
			      "int", "unsigned int",
			      "short", "unsigned short",
			      "long", "unsigned long",
			      "float", "double",
			      "gchar", "guchar",
			      "gintä", "guint",
			      "gshort", "gushort",
			      "glong", "gulong",
			      "gfloat", "gdouble"]
	    
	val strLs          = ["char", "unsigned char",
			      "gchar", "guchar"]
	    
	fun compare (x, y) =
	    (case String.compare(x, y) of
		 EQUAL => true
	       | _     => false)
		     
	fun isObjectPtr (POINTER(VALUE(s))) = not (List.exists (fn x => compare(s, x)) ptrLs)
	  | isObjectPtr _                   = false
		 
	fun isStringPtr (POINTER(VALUE(s))) = List.exists (fn x => compare(s, x)) strLs
	  | isStringPtr _                   = false

	fun checkVal v =
		 if isObjectPtr v then PObject else (if isStringPtr v then PString else PNormal)

	fun emitArgs(ps, i, nil)    = ps "}\n"
	  | emitArgs(ps, i, _::ar)  = (ps (" A" ^ (Int.toString i)); emitArgs(ps, (i + 1), ar))

	fun emitHeader(ps, s, nil)  = ps ("fun {" ^ s ^ " _}\n")
	  | emitHeader(ps, s, args) = (ps ("fun {" ^ s); emitArgs(ps, 0, args))

	fun argWrapper(ps, x) =
		 let
		     val ARG(ct, i) = x
		     val is         = Int.toString i
		 in
		     case checkVal ct of
			 PObject => ps (" {ObjectToPointer A" ^ is ^ "}")
		       | PString => ps (" {ByteString.toString A" ^ is ^ "}")
		       | PNormal => ps (" A" ^ is)
		 end

	fun emitCore(ps, name, VALUE("void"), args) =
	    (ps "   {";
	     ps ("Native." ^ (firstLower name));
	     map (fn x => argWrapper(ps, x)) args;
	     ps "}\n   unit\n")
	  | emitCore(ps, name, rt, args)            =
	    let
		val pw = fn x => argWrapper(ps, x)
	    in
		case checkVal rt of
		    PObject => (ps ("   {PointerToObject {Native." ^ (firstLower name));
				map pw args;
				ps "}}\n")
		  | PString => (ps ("   {ByteString.make {Native." ^ (firstLower name));
				map pw args;
				ps "}}\n")
		  | PNormal => (ps ("   {Native." ^ (firstLower name)); map pw args; ps "}\n")
	    end

	fun returnPrimType (VALUE("void"))           = "unit"
	  | returnPrimType (VALUE("float"))          = "real"
	  | returnPrimType (VALUE("double"))         = "real"
	  | returnPrimType (VALUE("gfloat"))         = "real"
	  | returnPrimType (VALUE("gdouble"))        = "real"
	  | returnPrimType (VALUE("gboolean"))       = "bool"
	  | returnPrimType (VALUE(_))                = "int"
	  | returnPrimType (POINTER(VALUE(_)))       = "object" (* security assumptions *)
	  | returnPrimType _                         = "object"

	fun returnType v =
	    (case checkVal v of
		 PObject => "object"
	       | PString => "string"
	       | PNormal => returnPrimType v)

	fun emitArgTypes(ps, nil)              = ps "unit"
	  | emitArgTypes(ps, (ARG(t, _))::nil) = ps (returnType t)
	  | emitArgTypes(ps, (ARG(t, _))::ar)  = (ps ((returnType t) ^ " * ");
						  emitArgTypes(ps, ar))

	fun emitSignature(ps, s, rt, args) =
	    (ps ("                val " ^ (firstLower s) ^ " : ");
	     emitArgTypes(ps, args);
	     ps " -> ";
	     ps (returnType rt);
	     ps "\n")
	    
	fun emitWrapperInterface(ps, is, sis, FUNCTION(s, FUNCARGS(rt, args))) =
	    (emitHeader(ps, s, args);
	     emitSignature(sis, s, rt, args);
	     is ("         " ^ (firstLower s) ^ " : " ^ s ^ "\n");
	     emitCore(ps, s, rt, args);
	     ps "end\n")
	  | emitWrapperInterface (_, _, _, _)                                  = ()
	    
	fun writeText(ps, nil)   = ()
	  | writeText(ps, s::sr) = (ps s; writeText(ps, sr))

	val wrpGTKFile = "GTKWrapper.oz"
	val expGTKFile = "GTKExport.oz"
	val sigGTKFile = "GTK.oz.sig"

	val wrpGDKFile = "GDKWrapper.oz"
	val expGDKFile = "GDKExport.oz"
	val sigGDKFile = "GDK.oz.sig"

	val wrpGTKPrefix = ["%%%\n",
			    "%%% Notice:\n",
			    "%%%   This file is automatically generated.\n",
			    "%%%   Please do not edit.\n",
			    "%%%\n\n"]

	val wrpGDKPrefix = ["%%%\n",
			    "%%% Notice:\n",
			    "%%%   This file is automatically generated.\n",
			    "%%%   Please do not edit.\n",
			    "%%%\n\n"]
	    
	val expGTKPrefix = ["%%%\n",
			    "%%% Notice:\n",
			    "%%%   This file is automatically generated.\n",
			    "%%%   Please do not edit.\n",
			    "%%%\n\n",
			    "GTK = 'GTK'(\n",
			    "         '$object' : GTKCore.'$object'\n",
			    "         pointerToObject : GTKCore.pointerToObject\n",
			    "         objectToPointer : GTKCore.objectToPointer\n",
			    "         removeObject : GTKCore.removeObject\n",
			    "         signalConnect : GTKCore.signalConnect\n",
			    "         signalDisconnect : GTKCore.signalDisconnect\n",
			    "         signalHandlerBlock : GTKCore.signalHandlerBlock\n",
			    "         signalHandlerUnblock : GTKCore.signalHandlerUnblock\n",
			    "         signalEmit : GTKCore.signalEmit\n",
			    "         exit : GTKCore.exit\n"]

	val expGDKPrefix = ["%%%\n",
			    "%%% Notice:\n",
			    "%%%   This file is automatically generated.\n",
			    "%%%   Please do not edit.\n",
			    "%%%\n\n",
			    "GDK = 'GDK'(\n",
			    "         '$object' : GTKCore.'$object'\n"]
	    
	val expGTKEnd = ["         )\n"]

	val expGDKEnd = ["         )\n"]

	val sigGTKPrefix =
	    ["(*\n",
	     " * Notice:\n",
	     " *   This file is generated.\n",
	     " *   Please do not edit.\n",
	     " *\n",
	     " *)\n",
	     "\n",
	     "import\n",
	     "    structure GTKCore\n",
	     "from \"GTKCore.ozf\"\n\n",
	     "signature GTK_COMPONENT =\n",
	     "    sig\n",
	     "        signature GTK =\n",
	     "            sig\n",
	     "                type object = GTKCore.object\n\n",
	     "                val pointerToObject : object -> int\n",
	     "                val objectToPointer : int -> object\n",
	     "                val removeObject : int -> unit\n",
	     "                val signalConnect : object * string * (unit -> unit) -> int\n",
	     "                val signalDisconnect : object * int -> unit\n",
	     "                val signalHandlerBlock : object * int -> unit\n",
	     "                val signalEmit : object * string -> unit\n",
	     "                val exit : unit -> unit\n"]

	val sigGDKPrefix =
	    ["(*\n",
	     " * Notice:\n",
	     " *   This file is generated.\n",
	     " *   Please do not edit.\n",
	     " *\n",
	     " *)\n",
	     "\n",
	     "import\n",
	     "    structure GTKCore\n",
	     "from \"GTKCore.ozf\"\n\n",
	     "signature GDK_COMPONENT =\n",
	     "    sig\n",
	     "        signature GDK =\n",
	     "            sig\n",
	     "                type object = GTKCore.object\n\n"]

	val sigGTKEnd =
	    ["            end\n\n",
	     "        structure GTK : GTK\n",
	     "    end\n"]

	val sigGDKEnd =
	    ["            end\n\n",
	     "        structure GDK : GDK\n",
	     "    end\n"]

	fun alicegtk inFile =
	    let
		val (gtks, gdks) = collect inFile
		val ws           = ref (TextIO.openOut wrpGTKFile)
		val is           = ref (TextIO.openOut expGTKFile)
		val ss           = ref (TextIO.openOut sigGTKFile)
		val pws          = fn s => TextIO.output(!ws, s)
		val pis          = fn s => TextIO.output(!is, s)
		val sis          = fn s => TextIO.output(!ss, s)
	    in
		(writeText(pws, wrpGTKPrefix);
		 writeText(pis, expGTKPrefix);
		 writeText(sis, sigGTKPrefix);
		 List.map (fn x => emitWrapperInterface(pws, pis, sis, x)) gtks;
		 writeText(pis, expGTKEnd); writeText(sis, sigGTKEnd);
		 TextIO.closeOut (!ws); TextIO.closeOut (!is); TextIO.closeOut (!ss);
		 ws := (TextIO.openOut wrpGDKFile);
		 is := (TextIO.openOut expGDKFile);
		 ss := (TextIO.openOut sigGDKFile);
		 writeText(pws, wrpGDKPrefix);
		 writeText(pis, expGDKPrefix);
		 writeText(sis, sigGDKPrefix);
		 List.map (fn x => emitWrapperInterface(pws, pis, sis, x)) gdks;
		 writeText(pis, expGDKEnd); writeText(sis, sigGDKEnd);
		 TextIO.closeOut (!ws); TextIO.closeOut (!is); TextIO.closeOut (!ss))
	    end

    end

local
    fun main _ = (Collect.alicegtk "header/gtkheader.c"; OS.Process.success)
in
    val _ = SMLofNJ.exportFn("collect", main)
end
