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
	open Bindings

	datatype CType =
	    POINTER  of CType
	  | VALUE    of string
	  | FUNCARGS of CType * (ARGVAL list)
	  | FUNCTION of string * CType
	  | CONSTANT of string * string
	  | ENUM     of CType list
	  | IGNORED
	and ARGVAL = ARG of CType * int

	datatype PTRVAL = PNormal | PObject | PString | PList

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
	  | transform (Ellipses, tab)                = VALUE("...")
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

	fun collectEnumEntry({name=s, kind=ENUMmem(e), ...} : Ast.member, v) =
	    CONSTANT(Symbol.name s, LargeInt.toString v)
	  | collectEnumEntry(_, v)                                           = IGNORED

	fun collectEnumValues(nil, t)        = nil
	  | collectEnumValues((m, v)::xr, t) = collectEnumEntry(m, v)::collectEnumValues(xr, t)

	fun collectType(i, tab) =
	    (case Tidtab.find(tab, i) of
		 SOME({ntype=SOME(Enum(_, vl)), ...} : tidBinding) =>
		     let
			 val es  = collectEnumValues(vl, tab)
			 val fes = List.filter (fn IGNORED => false | _ => true) es
		     in
			 ENUM(fes)
		     end
	       | _                                                 => IGNORED)

	fun collectDecl(TypeDecl({tid=t, ...}), tab) = collectType(t, tab)
	  | collectDecl(VarDecl(var, expr), tab)     =
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

	fun checkPrefix(nil, s)       = true
	  | checkPrefix(p::pr, s::sr) = (case String.compare(p, s) of
					     EQUAL => checkPrefix(pr, sr)
					   | _     => false)
	  | checkPrefix(_, _)         = false

	fun splitString s = String.tokens (fn #"_" => true | _ => false) s
		 
	fun hasPrefix(p, FUNCTION(s, _)) = checkPrefix(p, splitString s)
	  | hasPrefix(p, ENUM(cs))       = (case cs of
						CONSTANT(s,_)::_ => checkPrefix(p, splitString s)
					      | _                => false)
	  | hasPrefix(p, _)              = false

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

	fun createName (nil)    = ""
	  | createName (s::nil) = s
	  | createName (s::sr)  = s ^ "_" ^ createName sr

	fun cutPrefix(i, nil)   = ""
	  | cutPrefix(i, s::sr) = if (i = 1) then createName sr else cutPrefix((i - 1), sr)

	fun transConstName(i, CONSTANT(s, v)) = CONSTANT(cutPrefix(i, splitString s), v)
	  | transConstName(_, v)              = v

	fun transEnumName(i, ENUM(cs)) = ENUM(map (fn x => transConstName(i, x)) cs)
	  | transEnumName(_, v)        = v

	fun collect file =
	    let
		val tree          = ParseToAst.fileToAst file
		val ast           = #ast tree
		val tab           = #tidtab tree
		val defs          = collect'(ast, tab)
		val cnsts         = List.filter (fn (ENUM(_)) => true | _ => false) defs
		val funcs         = List.filter (fn (FUNCTION(_, _)) => true | _ => false) defs
		val gtkrfl        = fn x => hasPrefix(["gtk"], x)
		val cvsfl         = fn x => hasPrefix(["gtk", "canvas"], x)
		val gtkrcfl       = fn x => hasPrefix(["GTK"], x)
		val cvscfl        = fn x => hasPrefix(["GTK", "CANVAS"], x)
		val gtkrf         = List.filter glbFilter (List.filter gtkrfl funcs)
		val gtkrc         = List.filter gtkrcfl cnsts
		val (cvfs, gtkfs) = List.partition cvsfl gtkrf
		val (cvcs, gtkcs) = List.partition cvscfl gtkrc
		val gdkfs         = List.filter (fn x => hasPrefix(["gdk"], x)) funcs
		val gdkcs         = List.filter (fn x => hasPrefix(["GDK"], x)) cnsts
		val filCanvas     = fn "gtk" => false | "canvas" => false | _ => true
	    in
		(map (fn x => transName((fn "gtk" => false | _ => true), x)) gtkfs,
		 map (fn x => transEnumName(1, x)) gtkcs,
		 map (fn x => transName((fn "gdk" => false | _ => true), x)) gdkfs,
		 map (fn x => transEnumName(1, x)) gdkcs,
		 map (fn x => transName(filCanvas, x)) cvfs,
		 map (fn x => transEnumName(2, x)) cvcs)
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

	fun isVAList (VALUE("..."))     = true
	  | isVAList (VALUE("va_list")) = true
	  | isVAList _                  = false

	fun checkVal v =
		 if isObjectPtr v then PObject else
		     (if isStringPtr v then PString else (if isVAList v then PList else PNormal))

	fun emitArgs (ps, i, nil)   = ps "}\n"
	  | emitArgs (ps, i, _::ar) = (ps (" A" ^ (Int.toString i)); emitArgs(ps, (i + 1), ar))

	fun emitHeader (ps, s, nil)  = ps ("fun {" ^ s ^ " _}\n")
	  | emitHeader (ps, s, args) = (ps ("fun {" ^ s); emitArgs(ps, 0, args))

	fun argWrapper(ps, x) =
		 let
		     val ARG(ct, i) = x
		     val is         = Int.toString i
		 in
		     case checkVal ct of
			 PObject => ps (" {ObjectToPointer A" ^ is ^ "}")
		       | PString => ps (" {ByteString.toString A" ^ is ^ "}")
		       | PList   => ps (" {VaArgListToOzList A" ^ is ^ "}")
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
		  | PList   => () (* this never happens *)
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
	       | PList   => "va_arg list"
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
	    
	fun constChange "2BUTTON_PRESS" = "TWO_BUTTON_PRESS"
	  | constChange "3BUTTON_PRESS" = "THREE_BUTTON_PRESS"
	  | constChange c               = c

	fun emitConstSignatures(ps, CONSTANT(s, _)::cr) =
	    (ps("                val " ^ constChange(s) ^ " : int\n"); emitConstSignatures(ps, cr))
	  | emitConstSignatures(ps, _)                  = ()

	fun emitConstValues(ps, CONSTANT(s, v)::cr) =
	    (ps("         '" ^ constChange(s) ^ "' : " ^ v ^ "\n"); emitConstValues(ps, cr))
	  | emitConstValues(ps, _)                  = ()
	    
	fun emitWrapperInterface(ps, is, sis, FUNCTION("CalendarGetDate", FUNCARGS(rt, args))) =
	    (ps "fun {CalendarGetDate A0}\n";
             ps "   A1 A2 A3\n";
             ps "in\n";
	     ps "   {Native.calendarGetDate {ObjectToPointer A0} A1 A2 A3}\n";
	     ps "   '#'(A1 A2 A3)\n";
	     ps "end\n";
	     sis ("                val calendarGetDate : object -> int * int * int\n");
	     is "         calendarGetDate : CalendarGetDate\n")
	  | emitWrapperInterface(ps, is, sis, FUNCTION("PixmapGet", FUNCARGS(rt, args))) =
	    (ps "fun {PixmapGet A0}\n";
             ps "   A1 A2\n";
             ps "in\n";
	     ps "   {Native.pixmapGet {ObjectToPointer A0} A1 A2}\n";
	     ps "   '#'({PointerToObject A1} {PointerToObject A2})\n";
	     ps "end\n";
	     sis ("                val pixmapGet : object -> object * object\n");
	     is "         pixmapGet : PixmapGet\n")
	  | emitWrapperInterface(ps, is, sis, FUNCTION("LabelGet", FUNCARGS(rt, args))) =
	    (ps "fun {LabelGet A0}\n";
             ps "   A1\n";
             ps "in\n";
	     ps "   {Native.labelGet {ObjectToPointer A0} A1}\n";
	     ps "   {ByteString.make A1}\n";
	     ps "end\n";
	     sis ("                val labelGet : object -> string\n");
	     is "         labelGet : LabelGet\n")
	  | emitWrapperInterface(ps, is, sis, FUNCTION(s, FUNCARGS(rt, args))) =
	    (emitHeader(ps, s, args);
	     emitSignature(sis, s, rt, args);
	     is("         " ^ (firstLower s) ^ " : " ^ s ^ "\n");
	     emitCore(ps, s, rt, args);
	     ps "end\n")
	  | emitWrapperInterface(_, is, sis, ENUM(cs))                        =
	     (emitConstValues(is, cs); emitConstSignatures(sis, cs))
	  | emitWrapperInterface(_, _, _, _)                                   = ()
	    
	fun writeText(ps, nil)   = ()
	  | writeText(ps, s::sr) = (ps s; writeText(ps, sr))

	val wrpGTKFile = "GTKWrapper.oz"
	val expGTKFile = "GTKExport.oz"
	val sigGTKFile = "GTK.ozf.sig"

	val wrpGDKFile = "GDKWrapper.oz"
	val expGDKFile = "GDKExport.oz"
	val sigGDKFile = "GDK.ozf.sig"

	val wrpCanvasFile = "CanvasWrapper.oz"
	val expCanvasFile = "CanvasExport.oz"
	val sigCanvasFile = "Canvas.ozf.sig"

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
                            "         '$va_arg' : GTKCore.'$va_arg'\n",
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
			    "         '$object' : GTKCore.'$object'\n",
			    "         '$va_arg' : GTKCore.'$va_arg'\n"]
	    
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
	     "import structure GTKCore from \"x-alice:/lib/gtk/GTKCore.ozf\"\n\n",
	     "signature GTK_COMPONENT =\n",
	     "    sig\n",
	     "        signature GTK =\n",
	     "            sig\n",
	     "                type object = GTKCore.object\n\n",
             "                datatype va_arg = datatype GTKCore.va_arg\n\n",
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
	     "import structure GTKCore from \"x-alice:/lib/gtk/GTKCore.ozf\"\n\n",
	     "signature GDK_COMPONENT =\n",
	     "    sig\n",
	     "        signature GDK =\n",
	     "            sig\n",
	     "                type object = GTKCore.object\n\n",
	     "                datatype va_arg = datatype GTKCore.va_arg\n\n"]

	val sigGTKEnd =
	    ["            end\n\n",
	     "        structure GTK : GTK\n",
	     "    end\n"]

	val sigGDKEnd =
	    ["            end\n\n",
	     "        structure GDK : GDK\n",
	     "    end\n"]

	val wrpCanvasPrefix = ["%%%\n",
			       "%%% Notice:\n",
			       "%%%   This file is automatically generated.\n",
			       "%%%   Please do not edit.\n",
			       "%%%\n\n"]
	    
	val expCanvasPrefix = ["%%%\n",
			       "%%% Notice:\n",
			       "%%%   This file is automatically generated.\n",
			       "%%%   Please do not edit.\n",
			       "%%%\n\n",
			       "CANVAS = 'CANVAS'(\n",
			       "            '$object' : GTKCore.'$object'\n",
			       "            '$va_arg' : GTKCore.'$va_arg'\n"]
	    
	val expCanvasEnd = ["            )\n"]

	val sigCanvasPrefix =
	    ["(*\n",
	     " * Notice:\n",
	     " *   This file is generated.\n",
	     " *   Please do not edit.\n",
	     " *\n",
	     " *)\n",
	     "\n",
	     "import structure GTKCore from \"x-alice:/lib/gtk/GTKCore.ozf\"\n\n",
	     "signature CANVAS_COMPONENT =\n",
	     "    sig\n",
	     "        signature CANVAS =\n",
	     "            sig\n",
	     "                type object = GTKCore.object\n\n",
	     "                datatype va_arg = datatype GTKCore.va_arg\n\n"]

	val sigCanvasEnd =
	    ["            end\n\n",
	     "        structure Canvas : CANVAS\n",
	     "    end\n"]

	fun alicegtk inFile =
	    let
		val (gtks, gtkcs, gdks, gdkcs, gcvs, gcvcs) = collect inFile
		val ws                                      = ref (TextIO.openOut wrpGTKFile)
		val is                                      = ref (TextIO.openOut expGTKFile)
		val ss                                      = ref (TextIO.openOut sigGTKFile)
		val pws                                     = fn s => TextIO.output(!ws, s)
		val pis                                     = fn s => TextIO.output(!is, s)
		val sis                                     = fn s => TextIO.output(!ss, s)
	    in
		(writeText(pws, wrpGTKPrefix);
		 writeText(pis, expGTKPrefix);
		 writeText(sis, sigGTKPrefix);
		 app (fn x => emitWrapperInterface(pws, pis, sis, x)) gtks;
		 app (fn x => emitWrapperInterface(pws, pis, sis, x)) gtkcs;
		 writeText(pis, expGTKEnd); writeText(sis, sigGTKEnd);
		 TextIO.closeOut(!ws); TextIO.closeOut(!is); TextIO.closeOut(!ss);
		 ws := (TextIO.openOut wrpGDKFile);
		 is := (TextIO.openOut expGDKFile);
		 ss := (TextIO.openOut sigGDKFile);
		 writeText(pws, wrpGDKPrefix);
		 writeText(pis, expGDKPrefix);
		 writeText(sis, sigGDKPrefix);
		 app (fn x => emitWrapperInterface(pws, pis, sis, x)) gdks;
		 app (fn x => emitWrapperInterface(pws, pis, sis, x)) gdkcs;
		 writeText(pis, expGDKEnd); writeText(sis, sigGDKEnd);
		 TextIO.closeOut(!ws); TextIO.closeOut(!is); TextIO.closeOut(!ss);
		 ws := (TextIO.openOut wrpCanvasFile);
		 is := (TextIO.openOut expCanvasFile);
		 ss := (TextIO.openOut sigCanvasFile);
		 writeText(pws, wrpCanvasPrefix);
		 writeText(pis, expCanvasPrefix);
		 writeText(sis, sigCanvasPrefix);
		 app (fn x => emitWrapperInterface(pws, pis, sis, x)) gcvs;
		 app (fn x => emitWrapperInterface(pws, pis, sis, x)) gcvcs;
		 writeText(pis, expCanvasEnd); writeText(sis, sigCanvasEnd);
		 TextIO.closeOut(!ws); TextIO.closeOut(!is); TextIO.closeOut(!ss))
	    end

    end

local
    fun main _ = (Collect.alicegtk "header/gtkheader.c"; OS.Process.success)
in
    val _ = SMLofNJ.exportFn("collect", main)
end
