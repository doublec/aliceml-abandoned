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
	val alicegtk : string list -> OS.Process.status
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
	  | MEMBER   of string * CType
	  | ENUM     of CType list
	  | STRUCT   of string * CType list
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

	fun collectStructEntry({name=s, kind=STRUCTmem, ctype=ctype, ...} : Ast.member, t) =
	    MEMBER(Symbol.name s, transform(ctype, t))
	  | collectStructEntry _                                                          = IGNORED

	fun collectUnionEntry({name=s, kind=UNIONmem, ctype=ctype, ...} : Ast.member, t) =
	    MEMBER(Symbol.name s, transform(ctype, t))
	  | collectUnionEntry _                                                           = IGNORED

	fun collectStructValues(nil, t)                 = nil
	  | collectStructValues((_, SOME m, _)::xr, t)  =
	    collectStructEntry(m, t)::collectStructValues(xr, t)
	  | collectStructValues(_::xr, t)               = collectStructValues(xr, t)

	fun collectUnionValues(nil, t)        = nil
	  | collectUnionValues((_, m)::xr, t) = collectUnionEntry(m, t)::collectUnionValues(xr, t)

	fun collectType(i, tab) =
	    (case Tidtab.find(tab, i) of
		 SOME({ntype=SOME(Enum(_, vl)), ...} : tidBinding) =>
		     let
			 val es  = collectEnumValues(vl, tab)
			 val fes = List.filter (fn IGNORED => false | _ => true) es
		     in
			 ENUM(fes)
		     end
	       | SOME({ntype=SOME(Struct(n, vl)), ...} : tidBinding) =>
		     let
			 val ss  = collectStructValues(vl, tab)
			 val fss = List.filter (fn IGNORED => false | _ => true) ss
		     in
			 STRUCT(tidToString(n, tab), fss)
		     end
	       | SOME({ntype=SOME(Union(n, vl)), ...} : tidBinding) =>
		     let
			 val ss  = collectUnionValues(vl, tab)
			 val fss = List.filter (fn IGNORED => false | _ => true) ss
		     in
			 STRUCT(tidToString(n, tab), fss)
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
	  | glbFilter(FUNCTION("gtk_object_getv", _))            = false
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
		val structs       = List.filter (fn (STRUCT _) => true | _ => false) defs
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
		 map (fn x => transEnumName(2, x)) cvcs,
		 structs)
	    end

	val ptrLs          = ["char", "unsigned char",
			      "int", "unsigned int",
			      "short", "unsigned short",
			      "long", "unsigned long",
			      "float", "double",
			      "gchar", "guchar",
			      "gint", "guint",
			      "gshort", "gushort",
			      "glong", "gulong",
			      "gfloat", "gdouble"]
	    
	val strLs          = ["char", "unsigned char",
			      "gchar", "guchar"]
	    
	fun compare (x, y) =
	    (case String.compare(x, y) of
		 EQUAL => true
	       | _     => false)
		     
	fun isObjectPtr (VALUE("gpointer")) = true
	  | isObjectPtr (POINTER(VALUE(s))) = not (List.exists (fn x => compare(s, x)) ptrLs)
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

	val natRef = ref "Native."

	fun emitCore(ps, name, VALUE("void"), args) =
	    (ps "   {";
	     ps ((!natRef) ^ (firstLower name));
	     map (fn x => argWrapper(ps, x)) args;
	     ps "}\n   unit\n")
	  | emitCore(ps, name, rt, args)            =
	    let
		val pw = fn x => argWrapper(ps, x)
	    in
		case checkVal rt of
		    PObject => (ps ("   {PointerToObject {" ^ (!natRef) ^ (firstLower name));
				map pw args;
				ps "}}\n")
		  | PString => (ps ("   {ByteString.make {" ^ (!natRef) ^ (firstLower name));
				map pw args;
				ps "}}\n")
		  | PNormal => (ps ("   {" ^ (!natRef) ^ (firstLower name)); map pw args; ps "}\n")
		  | PList   => () (* this never happens *)
	    end

	fun returnPrimType (VALUE("void"))           = "unit"
	  | returnPrimType (VALUE("float"))          = "real"
	  | returnPrimType (VALUE("double"))         = "real"
	  | returnPrimType (VALUE("gfloat"))         = "real"
	  | returnPrimType (VALUE("gdouble"))        = "real"
	  | returnPrimType (VALUE("gboolean"))       = "bool"
	  | returnPrimType (VALUE(_))                = "int"
	  | returnPrimType (POINTER(VALUE(_)))       = "T.object" (* security assumptions *)
	  | returnPrimType _                         = "T.object"

	fun returnType v =
	    (case checkVal v of
		 PObject => "T.object"
	       | PString => "string"
	       | PList   => "T.va_arg list"
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

	val canvasWrapperItemNew =
	    [
	     "local\n",
	     "   proc {ConfItem O Os}\n",
             "      case Os\n",
	     "      of OK|OV|Or then\n",
	     "         Val  = if {IsInt OV} then OV\n",
	     "                elseif {IsFloat OV} then OV\n",
	     "                elseif {IsByteString OV} then {ByteString.toString OV}\n",
	     "                else {ObjectToPointer OV}\n",
	     "                end\n",
	     "      in\n",
             "         {Native.itemSet O {ByteString.toString OK} Val}\n",
	     "         {ConfItem O Or}\n",
	     "      [] _ then skip\n",
             "      end\n",
             "   end\n",
             "in\n",
	     "   fun {ItemNew A0 A1 A2}\n",
	     "      Item = {Native.itemNew {ObjectToPointer A0} A1}\n",
             "      Os   = {VaArgListToOzList A2}\n",
	     "   in\n",
	     "      {ConfItem Item Os}\n",
	     "      {PointerToObject Item}\n",
	     "   end\n",
	     "end\n"
	     ]

	val canvasWrapperItemSet =
	    [
	     "fun {ItemSet A0 A1 A2}\n",
             "   A2New = if     {IsByteString A2} then {ByteString.toString A2}\n",
	     "           elseif {IsInt A2}        then A2\n",
             "           elseif {IsFloat A2}      then A2\n",
	     "           else {ObjectToPointer A2}\n",
             "           end\n",
             "in\n",
	     "   {Native.itemSet {ObjectToPointer A0} {ByteString.toString A1} A2New}\n",
	     "   unit\n",
	     "end\n"
	     ]

	val canvasWrapperPointsNew =
	    [
	     "fun {PointsNew A0}\n",
	     "   {PointerToObject {Native.pointsNew A0}}\n",
	     "end\n",
	     "fun {PointsPut A0 A1 A2}\n",
	     "   {Native.pointsPut {ObjectToPointer A0} A1 A2}\n",
	     "   unit\n",
	     "end\n"
	     ]
	    
	val gtkWrapperComboNew =
	    [
	     "fun {ComboNew _}\n",
	     "  {PointerToObject {Native.comboNew}}\n",
	     "end\n",
	     "fun {ComboGetEntry A0}\n",
	     "  {PointerToObject {GtkCore.comboGetEntry {ObjectToPointer A0}}}\n",
	     "end\n",
	     "fun {ComboGetList A0}\n",
	     "  {PointerToObject {GtkCore.comboGetList {ObjectToPointer A0}}}\n",
	     "end\n"
	     ]

	val gtkWrapperFileSelectionNew =
	    [
	     "fun {FileSelectionNew A0}\n",
	     "   {PointerToObject {Native.fileSelectionNew A0}}\n",
	     "end\n",
	     "fun {FileSelectionGetOkButton A0}\n",
	     "   {PointerToObject {GtkCore.fileSelectionGetOkButton {ObjectToPointer A0}}}\n",
	     "end\n",
	     "fun {FileSelectionGetCancelButton A0}\n",
	     "   {PointerToObject {GtkCore.fileSelectionGetCancelButton {ObjectToPointer A0}}}\n",
	     "end\n"
	     ]

	val gtkWrapperColorSelectionSetColor =
	    [
	     "fun {ColorSelectionSetColor A0 A1 A2 A3 A4}\n",
	     "  {GtkCore.colorSelectionSetColor {ObjectToPointer A0} A1 A2 A3 A4}\n",
	     "  unit\n",
	     "end\n"
	     ]

	val gtkWrapperColorSelectionGetColor =
	    [
	     "fun {ColorSelectionGetColor A0}\n",
	     "  {GtkCore.colorSelectionGetColor {ObjectToPointer A0}}\n",
	     "end\n"
	     ]

	val gtkWrapperWidgetSizeRequest =
	    [
	     "fun {WidgetSizeRequest A0 A1 A2}\n",
	     "   {GtkCore.widgetSizeRequest {ObjectToPointer A0} A1 A2}\n",
	     "   unit\n",
	     "end\n"
	     ]

	val gtkWrapperWidgetGetChildRequisition =
	    [
	     "fun {WidgetGetChildRequisition A0}\n",
	     "   {GtkCore.widgetGetChildRequisition {ObjectToPointer A0}}\n",
	     "end\n"
	     ]

	val gtkWrapperWidgetGetPointer =
	    [
	     "fun {WidgetGetPointer A0}\n",
	     "   {GtkCore.widgetGetPointer {ObjectToPointer A0}}\n",
	     "end\n"
	     ]

	fun emitWrapperInterface(ps, is, sis, "Canvas", FUNCTION("ItemNew", _))                   =
	    (app ps canvasWrapperItemNew;
	     sis ("                val itemNew : T.object * int * T.va_arg list -> T.object\n");
	     is "         itemNew : ItemNew\n")
	  |  emitWrapperInterface(ps, is, sis, "Canvas", FUNCTION("ItemSet", _))                  =
	    (app ps canvasWrapperItemSet;
	     sis ("                val itemSet : T.object * string * 'a -> unit\n");
	     is "         itemSet : ItemSet\n")
	  | emitWrapperInterface(ps, is, sis, "Canvas", FUNCTION("PointsNew", _))                 =
	    (app ps canvasWrapperPointsNew;
	     sis ("                val pointsNew : int -> T.object\n");
	     sis ("                val pointsPut : T.object * int * int -> unit\n");
	     is "         pointsNew : PointsNew\n";
	     is "         pointsPut : PointsPut\n")
	  | emitWrapperInterface(ps, is, sis, "gtk", FUNCTION("ComboNew", _))                     =
	    (app ps gtkWrapperComboNew;
	     sis ("                val comboNew : unit -> T.object\n");
	     sis ("                val comboGetEntry : T.object -> T.object\n");
	     sis ("                val comboGetList : T.object -> T.object\n");
	     is "         comboNew : ComboNew\n";
	     is "         comboGetEntry : ComboGetEntry\n";
	     is "         comboGetList : ComboGetList\n")
	  | emitWrapperInterface(ps, is, sis, "gtk", FUNCTION("FileSelectionNew", _))             =
	    (app ps gtkWrapperFileSelectionNew;
	     sis ("                val fileSelectionNew : string -> T.object\n");
	     sis ("                val fileSelectionGetOkButton : T.object -> T.object\n");
	     sis ("                val fileSelectionGetCancelButton : T.object -> T.object\n");
	     is "         fileSelectionNew : FileSelectionNew\n";
	     is "         fileSelectionGetOkButton : FileSelectionGetOkButton\n";
	     is "         fileSelectionGetCancelButton : FileSelectionGetCancelButton\n")
	  | emitWrapperInterface(ps, is, sis, "gtk", FUNCTION("ColorSelectionSetColor", _))       =
	    (app ps gtkWrapperColorSelectionSetColor;
	     sis ("                val colorSelectionSetColor : T.object * real * real * real * real-> unit\n");
	     is "         colorSelectionSetColor : ColorSelectionSetColor\n")
	  | emitWrapperInterface(ps, is, sis, "gtk", FUNCTION("ColorSelectionGetColor", _))       =
	    (app ps gtkWrapperColorSelectionGetColor;
	     sis ("                val colorSelectionGetColor : T.object -> real * real * real * real\n");
	     is "         colorSelectionGetColor : ColorSelectionGetColor\n")
	  | emitWrapperInterface(ps, is, sis, "gtk", FUNCTION("WidgetSizeRequest", _))            =
	    (app ps gtkWrapperWidgetSizeRequest;
	     sis ("                val widgetSizeRequest : T.object * int * int -> unit\n");
	     is "         widgetSizeRequest : WidgetSizeRequest\n")
	  | emitWrapperInterface(ps, is, sis, "gtk", FUNCTION("WidgetGetChildRequisition", _))    =
	    (app ps gtkWrapperWidgetGetChildRequisition;
	     sis ("                val widgetGetChildRequisition : T.object -> int * int\n");
	     is "         widgetGetChildRequisition : WidgetGetChildRequisition\n")
	  | emitWrapperInterface(ps, is, sis, "gtk", FUNCTION("WidgetGetPointer", _))             =
	    (app ps gtkWrapperWidgetGetPointer;
	     sis ("                val widgetGetPointer : T.object -> int * int\n");
	     is "         widgetGetPointer : WidgetGetPointer\n")
	  | emitWrapperInterface(ps, is, sis, "gtk", FUNCTION("ButtonBoxChildSizeDefault", _))    =
	    (ps "fun {ButtonBoxChildSizeDefault _}\n";
	     ps "   {GtkCore.buttonBoxGetChildSizeDefault}\n";
	     ps "end\n";
	     sis ("                val buttonBoxGetChildSizeDefault : unit -> int * int\n");
	     is "         buttonBoxGetChildSizeDefault : ButtonBoxChildSizeDefault\n")
	  | emitWrapperInterface(ps, is, sis, "gtk", FUNCTION("ButtonBoxChildIpaddingDefault", _))    =
	    (ps "fun {ButtonBoxChildIpaddingDefault _}\n";
	     ps "   {GtkCore.buttonBoxGetChildIpaddingDefault}\n";
	     ps "end\n";
	     sis ("                val buttonBoxGetIpaddingDefault : unit -> int * int\n");
	     is "         buttonBoxGetChildIpaddingDefault : ButtonBoxChildIpaddingDefault\n")
	  | emitWrapperInterface(ps, is, sis, "gtk", FUNCTION("ButtonBoxGetChildSize", _))    =
	    (ps "fun {ButtonBoxGetChildSize A0}\n";
	     ps "   {GtkCore.buttonBoxGetChildSize {ObjectToPointer A0}}\n";
	     ps "end\n";
	     sis ("                val buttonBoxGetChildSize : T.object -> int * int\n");
	     is "         buttonBoxGetChildSize : ButtonBoxGetChildSize\n")
	  | emitWrapperInterface(ps, is, sis, "gtk", FUNCTION("ButtonBoxGetChildIpadding", _))    =
	    (ps "fun {ButtonBoxGetChildIpadding A0}\n";
	     ps "   {GtkCore.buttonBoxGetChildIpadding {ObjectToPointer A0}}\n";
	     ps "end\n";
	     sis ("                val buttonBoxGetChildIpadding : T.object -> int * int\n");
	     is "         buttonBoxGetChildIpadding : ButtonBoxGetChildIpadding\n")
	  | emitWrapperInterface(ps, is, sis, "gtk", FUNCTION("ClistGetText", _))                 =
	    (ps "fun {ClistGetText A0 A1 A2}\n";
	     ps "   {GtkCore.clistGetText {ObjectToPointer A0} A1 A2}\n";
	     ps "end\n";
	     sis ("                val clistGetText : T.object * int * int -> string * int\n");
	     is "         clistGetText : ClistGetText\n")
	  | emitWrapperInterface(ps, is, sis, "gtk", FUNCTION("ClistGetPixmap", _))               =
	    (ps "fun {ClistGetPixmap A0 A1 A2}\n";
	     ps "   case {GtkCore.clistGetPixmap {ObjectToPointer A0} A1 A2}\n";
	     ps "   of '#'(P1 P2 I) then '#'({PointerToObject P1} {PointerToObject P2} I)\n";
	     ps "   end\n";
	     ps "end\n";
	     sis ("                val clistGetPixmap : T.object * int * int -> T.object * T.object * int\n");
	     is "         clistGetPixmap : ClistGetPixmap\n")
	  | emitWrapperInterface(ps, is, sis, "gtk", FUNCTION("ClistGetPixtext", _))              =
	    (ps "fun {ClistGetPixtext A0 A1 A2}\n";
	     ps "   case {GtkCore.clistGetPixtext {ObjectToPointer A0} A1 A2}\n";
	     ps "   of '#'(T I1 P1 P2 I2) then '#'(T I1 {PointerToObject P1} {PointerToObject P2} I2)\n";
	     ps "   end\n";
	     ps "end\n";
	     sis ("                val clistGetPixtext : T.object * int * int -> string * int * T.object * T.object * int\n");
	     is "         clistGetPixtext : ClistGetPixtext\n")
	  | emitWrapperInterface(ps, is, sis, "gtk", FUNCTION("ClistGetSelectionInfo", _))        =
	    (ps "fun {ClistGetSelectionInfo A0 A1 A2}\n";
	     ps "   {GtkCore.clistGetSelectionInfo {ObjectToPointer A0} A1 A2}\n";
	     ps "end\n";
	     sis ("                val clistGetSelectionInfo : T.object * int * int -> int * int * int\n");
	     is "         clistGetSelectionInfo : ClistGetSelectionInfo\n")
	  | emitWrapperInterface(ps, is, sis, "gtk", FUNCTION("CtreeNodeGetText", _))             =
	    (ps "fun {CtreeNodeGetText A0 A1 A2}\n";
	     ps "   {GtkCore.ctreeNodeGetText {ObjectToPointer A0} {ObjectToPointer A1} A2}\n";
	     ps "end\n";
	     sis ("                val ctreeNodeGetText : T.object * T.object * int -> string * int\n");
	     is "         ctreeNodeGetText : CtreeNodeGetText\n")
	  | emitWrapperInterface(ps, is, sis, "gtk", FUNCTION("CtreeNodeGetPixmap", _))           =
	    (ps "fun {CtreeNodeGetPixmap A0 A1 A2}\n";
	     ps "   case {GtkCore.ctreeNodeGetPixmap {ObjectToPointer A0} {ObjectToPointer A1} A2}\n";
	     ps "   of '#'(P1 P2 I) then '#'({PointerToObject P1} {PointerToObject P2} I)\n";
	     ps "   end\n";
	     ps "end\n";
	     sis ("                val ctreeNodeGetPixmap : T.object * T.object * int -> T.object * T.object * int\n");
	     is "         ctreeNodeGetPixmap : CtreeNodeGetPixmap\n")
	  | emitWrapperInterface(ps, is, sis, "gtk", FUNCTION("CtreeNodeGetPixtext", _))          =
	    (ps "fun {CtreeNodeGetPixtext A0 A1 A2}\n";
	     ps "   case {GtkCore.ctreeNodePixtext {ObjectToPointer A0} {ObjectToPointer A1} A2}\n";
	     ps "   of '#'(S I1 P1 P2 I2) then '#'(S I1 {PointerToObject P1} {PointerToObject P2} I2)\n";
	     ps "   end\n";
	     ps "end\n";
	     sis ("                val ctreeNodeGetPixtext : T.object * T.object * int -> string * int * T.object * T.object * int\n");
	     is "         ctreeNodeGetPixtext : CtreeNodeGetPixtext\n")
	  | emitWrapperInterface(ps, is, sis, "gtk", FUNCTION("CtreeGetNodeInfo", _))          =
	    (ps "fun {CtreeGetNodeInfo A0 A1}\n";
	     ps "   case {GtkCore.ctreeGetNodeInfo {ObjectToPointer A0} {ObjectToPointer A1}}\n";
	     ps "   of '#'(S I1 P1 P2 P3 P4 B1 B2 I2) then\n";
	     ps "      '#'(S I1 {PointerToObject P1} {PointerToObject P2}\n";
	     ps "          {PointerToObject P3} {PointerToObject P4} B1 B2 I2)\n";
	     ps "   end\n";
	     ps "end\n";
	     sis ("                val ctreeGetNodeInfo : T.object * T.object -> string * int * T.object * T.object * T.object * T.object * bool * bool * int\n");
	     is "         ctreeGetNodeInfo : CtreeGetNodeInfo\n")
	  | emitWrapperInterface(ps, is, sis, "gtk", FUNCTION("ImageGet", _))                     =
	    (ps "fun {ImageGet A0}\n";
	     ps "   case {GtkCore.gtkImageGet {ObjectToPointer A0}}\n";
	     ps "   of '#'(P1 P2) then\n";
	     ps "     '#'({PointerToObject P1} {PointerToObject P2})\n";
	     ps "   end\n";
	     ps "end\n";
	     sis ("                val imageGet : T.object -> T.object * T.object\n");
	     is "         imageGet : ImageGet\n")
	  | emitWrapperInterface(ps, is, sis, _, FUNCTION("CalendarGetDate", FUNCARGS(rt, args))) =
	    (ps "fun {CalendarGetDate A0}\n";
             ps "   A1 A2 A3\n";
             ps "in\n";
	     ps "   {Native.calendarGetDate {ObjectToPointer A0} A1 A2 A3}\n";
	     ps "   '#'(A1 A2 A3)\n";
	     ps "end\n";
	     sis ("                val calendarGetDate : T.object -> int * int * int\n");
	     is "         calendarGetDate : CalendarGetDate\n")
	  | emitWrapperInterface(ps, is, sis, _, FUNCTION("PixmapGet", FUNCARGS(rt, args))) =
	    (ps "fun {PixmapGet A0}\n";
             ps "   A1 A2\n";
             ps "in\n";
	     ps "   {Native.pixmapGet {ObjectToPointer A0} A1 A2}\n";
	     ps "   '#'({PointerToObject A1} {PointerToObject A2})\n";
	     ps "end\n";
	     sis ("                val pixmapGet : T.object -> T.object * T.object\n");
	     is "         pixmapGet : PixmapGet\n")
	  | emitWrapperInterface(ps, is, sis, _, FUNCTION("LabelGet", FUNCARGS(rt, args))) =
	    (ps "fun {LabelGet A0}\n";
             ps "   A1\n";
             ps "in\n";
	     ps "   {Native.labelGet {ObjectToPointer A0} A1}\n";
	     ps "   {ByteString.make A1}\n";
	     ps "end\n";
	     sis ("                val labelGet : T.object -> string\n");
	     is "         labelGet : LabelGet\n")
	  | emitWrapperInterface(ps, is, sis, _, FUNCTION(s, FUNCARGS(rt, args))) =
	    (emitHeader(ps, s, args);
	     emitSignature(sis, s, rt, args);
	     is("         " ^ (firstLower s) ^ " : " ^ s ^ "\n");
	     emitCore(ps, s, rt, args);
	     ps "end\n")
	  | emitWrapperInterface(_, is, sis, _, ENUM(cs))                        =
	     (emitConstValues(is, cs); emitConstSignatures(sis, cs))
	  | emitWrapperInterface(_, _, _, _, _)                                  = ()
	    
	fun writeText(ps, nil)   = ()
	  | writeText(ps, s::sr) = (ps s; writeText(ps, sr))

	val wrpGTKFile = "GtkWrapper.oz"
	val expGTKFile = "GtkExport.oz"
	val sigGTKFile = "Gtk.asig"

	val wrpGDKFile = "GdkWrapper.oz"
	val expGDKFile = "GdkExport.oz"
	val sigGDKFile = "Gdk.asig"

	val wrpCanvasFile = "CanvasWrapper.oz"
	val expCanvasFile = "CanvasExport.oz"
	val sigCanvasFile = "Canvas.asig"

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
			    "GTK = 'Gtk'(\n",
			    "         pointerToObject : GtkCore.pointerToObject\n",
			    "         objectToPointer : GtkCore.objectToPointer\n",
			    "         removeObject : GtkCore.removeObject\n",
			    "         signalConnect : GtkCore.signalConnect\n",
			    "         signalDisconnect : GtkCore.signalDisconnect\n",
			    "         signalHandlerBlock : GtkCore.signalHandlerBlock\n",
			    "         signalHandlerUnblock : GtkCore.signalHandlerUnblock\n",
			    "         signalEmit : GtkCore.signalEmit\n",
			    "         exit : GtkCore.exit\n"]

	val expGDKPrefix = ["%%%\n",
			    "%%% Notice:\n",
			    "%%%   This file is automatically generated.\n",
			    "%%%   Please do not edit.\n",
			    "%%%\n\n",
			    "GDK = 'Gdk'(\n",
			    "         'allocateGdkColor' : GtkCore.'allocateGdkColor'\n",
			    "         'freeGdkColor' : GtkCore.'freeGdkColor'\n",
			    "         'getGdkEvent' : GtkCore.'getGdkEvent'\n"]
	    
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
	     "import structure GtkCoreTypes from \"GtkCoreTypes\"\n\n",
             "local\n",
             "    structure T = GtkCoreTypes\n",
             "in\n",
	     "    signature GTK_COMPONENT =\n",
	     "        sig\n",
	     "            structure Gtk :\n",
	     "                sig\n",
	     "                    val pointerToObject : T.object -> int\n",
	     "                    val objectToPointer : int -> T.object\n",
	     "                    val removeObject : int -> unit\n",
	     "                    val signalConnect : T.object * string * (T.object -> unit) -> int\n",
	     "                    val signalDisconnect : T.object * int -> unit\n",
	     "                    val signalHandlerBlock : T.object * int -> unit\n",
	     "                    val signalEmit : T.object * string -> unit\n",
	     "                    val exit : unit -> unit\n"]

	val sigGTKEnd =
	    ["                end\n\n",
	     "        end\n",
	     "end\n"]

	val sigGDKPrefix =
	    ["(*\n",
	     " * Notice:\n",
	     " *   This file is generated.\n",
	     " *   Please do not edit.\n",
	     " *\n",
	     " *)\n",
	     "\n",
	     "import structure GtkCoreTypes from \"GtkCoreTypes\"\n\n",
	     "local\n",
	     "    structure T = GtkCoreTypes\n",
	     "in\n",
	     "    signature GDK_COMPONENT =\n",
	     "        sig\n",
	     "            structure Gdk :\n",
	     "                sig\n",
	     "                    val allocateGdkColor : int * int * int -> T.object\n",
	     "                    val freeGdkColor : T.object -> unit\n",
	     "                    val getGdkEvent : T.object -> T.event\n",
	     "                    val freeGdkRectangle : T.object -> unit"]

	val sigGDKEnd =
	    ["                end\n\n",
	     "        end\n",
	     "end\n"]

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
			       "CANVAS = 'Canvas'(\n"]
	    
	val expCanvasEnd = ["            )\n"]

	val sigCanvasPrefix =
	    ["(*\n",
	     " * Notice:\n",
	     " *   This file is generated.\n",
	     " *   Please do not edit.\n",
	     " *\n",
	     " *)\n",
	     "\n",
	     "import structure GtkCoreTypes from \"GtkCoreTypes\"\n\n",
	     "local\n",
	     "    structure T = GtkCoreTypes\n",
	     "in\n",
	     "    signature CANVAS_COMPONENT =\n",
	     "        sig\n",
	     "            structure Canvas :\n",
	     "                sig\n"]

	val sigCanvasEnd =
	    ["                end\n\n",
	     "        end\n",
	     "end\n"]

	fun insertUnderscore nil      = nil
	  | insertUnderscore (a::nil) = [a]
	  | insertUnderscore (a::ar)  = a::[#"_"]::(insertUnderscore ar)

	fun charFirstLower nil     = nil
	  | charFirstLower (c::cr) = (Char.toLower c)::cr

	fun ozNameTokens' (nil, nil, ss)   = rev ss
	  | ozNameTokens' (nil, cts, ss)   = ozNameTokens'(nil, nil, (rev cts)::ss)
	  | ozNameTokens' (c::cr, cts, ss) =
	    (case Char.isUpper c of
		 false => ozNameTokens'(cr, c::cts, ss)
	       | true  => ozNameTokens'((Char.toLower c)::cr, nil, (rev cts)::ss))

	fun ozNameTokens ts = ozNameTokens'(ts, nil, nil)

	fun ozNameToCoreName s =
	    let
		val ss  = String.explode s
		val lss = charFirstLower ss
		val ps  = insertUnderscore (ozNameTokens lss)
	    in
		String.concat(map String.implode ps)
	    end
	    
	fun argToString (VALUE(s))                  = s
	  | argToString (POINTER(VALUE("va_list"))) = "va_list"
	  | argToString (POINTER(p))                = (argToString p) ^ "*" 
	  | argToString _                           = "unknown_type"

	fun argToCType (ARG(v, _)) = v

	fun printOpening(ps, (name, ia, oa)) =
	    (ps "OZ_BI_define (";
	     ps "alice_";
	     ps name;
	     ps (", " ^ ia ^ ", " ^ oa);
	     ps ") {\n")

	fun printClosing ps =
	    (ps "\treturn OZ_ENTAILED;\n";
	     ps "} OZ_BI_end\n\n")

	fun collectInArgs nil             = 0
	  | collectInArgs (ARG(v, _)::ar) =
	    (case v of
		 POINTER(VALUE("double")) => collectInArgs ar
	       | POINTER(VALUE("int"))    => collectInArgs ar
	       | _                        => 1 + collectInArgs ar)

	fun collectOutArgs (nil, VALUE("void")) = 0
	  | collectOutArgs (nil, _)             = 1 
	  | collectOutArgs (ARG(v, _)::ar, rt)  =
	    (case v of
		 POINTER(VALUE("double")) => 1 + collectOutArgs(ar, rt)
	       | POINTER(VALUE("int"))    => 1 + collectOutArgs(ar, rt)
	       | _                        => collectOutArgs(ar, rt))

	fun printRetVal (ps, VALUE("void")) = ()
	  | printRetVal (ps, v)             = (ps "\t"; ps (argToString v); ps " ret;\n")

	fun printRetAssign(ps, VALUE("void")) = ps "\t"
	  | printRetAssign(ps, v)             = ps "\tret = "

	fun checkCast(ps, VALUE("float"))    = ()
	  | checkCast(ps, VALUE("double"))   = ()
	  | checkCast(ps, VALUE("gfloat"))   = ()
	  | checkCast(ps, VALUE("gdouble"))  = ()
	  | checkCast(ps, VALUE("bool"))     = ()
	  | checkCast(ps, VALUE("gboolean")) = ()
	  | checkCast(ps, VALUE("va_list"))  = ()
	  | checkCast(ps, VALUE("..."))      = ()
	  | checkCast(ps, VALUE("int"))      = ()
	  | checkCast(ps, VALUE("gint"))     = ()
	  | checkCast(ps, VALUE("Array"))    = ()
	  | checkCast(ps, VALUE(v))          = (ps "("; ps v; ps ") ")
	  | checkCast(ps, _)                 = ()

	fun printArgs(ps, nil)            = ()
	  | printArgs(ps, ARG(v, i)::nil) = (checkCast(ps, v); ps "arg"; ps (Int.toString i))
	  | printArgs(ps, ARG(v, i)::ar)  =
	    (checkCast(ps, v); ps "arg"; ps (Int.toString i); ps ", "; printArgs(ps, ar))

	fun printCall(ps, name, args) =
	    (ps "gtk_canvas_"; ps name; ps "(";
	     printArgs(ps, args);
	     ps ");\n")

	fun handleRetVal(ps, VALUE("void")) = ()
	  | handleRetVal(ps, VALUE(_))      = ps "\tOZ_out(0) = OZ_int((int) ret);\n"
	  | handleRetVal(ps, POINTER(_))    = ps "\tOZ_out(0) = OZ_makeForeignPointer(ret);\n"
	  | handleRetVal(ps, _)             = ()

	fun handleRetArgs(ps, n, nil)                                  = ()
	  | handleRetArgs(ps, n, ARG(POINTER(VALUE("double")), i)::ar) =
	    (ps "\tOZ_out("; ps (Int.toString n); ps ") = ";
	     ps "OZ_float(arg_v"; ps (Int.toString i); ps ");\n";
	     handleRetArgs(ps, (n + 1), ar))
	  | handleRetArgs(ps, n, ARG(POINTER(VALUE("int")), i)::ar)    =
	    (ps "\tOZ_out("; ps (Int.toString n); ps ") = ";
	     ps "OZ_int(arg_v"; ps (Int.toString i); ps ");\n";
	     handleRetArgs(ps, (n + 1), ar))
	  | handleRetArgs(ps, n, ARG(_)::ar)                           = handleRetArgs(ps, n, ar)

	fun declareValue("float")    = "\tGOZ_DECLARE_GFLOAT"
	  | declareValue("double")   = "\tGOZ_DECLARE_GDOUBLE"
	  | declareValue("gfloat")   = "\tGOZ_DECLARE_GFLOAT"
	  | declareValue("gdouble")  = "\tGOZ_DECLARE_GDOUBLE"
	  | declareValue("bool")     = "\tGOZ_DECLARE_GBOOLEAN"
	  | declareValue("gboolean") = "\tGOZ_DECLARE_GBOOLEAN"
	  | declareValue("va_list")  = "\tALICE_DECLARE_VALIST"
	  | declareValue("...")      = "\tALICE_CONVERT_DOTDOTDOT"
	  | declareValue v           = "\tGOZ_DECLARE_GINT"

	fun printArgDecl(ps, nil)                          = ()
	  | printArgDecl(ps, ARG(VALUE("va_list"), i)::ar) =
	    printArgDecl(ps, ARG(POINTER(VALUE("va_list")), i)::ar)
	  | printArgDecl(ps, ARG(VALUE("Array"), i)::ar) =
	    printArgDecl(ps, ARG(POINTER(VALUE("double")), i)::ar)
	  | printArgDecl(ps, ARG(VALUE(v), i)::ar)         =
	    let
		val is = Int.toString i
	    in
		(ps (declareValue v); ps "("; ps is; ps ", arg"; ps is; ps ");\n";
		 printArgDecl(ps, ar))
	    end
	  | printArgDecl(ps, ARG(POINTER(VALUE("int")), i)::ar) =
	    let
		val is = Int.toString i
	    in
		(ps "\tGOZ_DECLARE_GINT("; ps is; ps ", arg_v"; ps is; ps ");\n";
		 ps "\tint *arg"; ps is; ps " = &arg_v"; ps is; ps ";\n";
		 printArgDecl(ps, ar))
	    end
	  | printArgDecl(ps, ARG(POINTER(VALUE("double")), i)::ar) =
	    let
		val is = Int.toString i
	    in
		(ps "\tGOZ_DECLARE_GDOUBLE("; ps is; ps ", arg_v"; ps is; ps ");\n";
		 ps "\tdouble *arg"; ps is; ps " = &arg_v"; ps is; ps ";\n";
		 printArgDecl(ps, ar))
	    end
	  | printArgDecl(ps, ARG(v, i)::ar)                =
	    let
		val is = Int.toString i
	    in
		(case isStringPtr v of
		     true  => (ps "\tGOZ_DECLARE_VIRTUAL_STRING(";
			       ps is; ps ", arg"; ps is; ps ");\n";
			       printArgDecl(ps, ar))
		   | false => (ps "\tOZ_declareForeignType(";
			       ps is; ps ", arg"; ps is; ps ", ";
			       ps (argToString v); ps ");\n";
			       printArgDecl(ps, ar)))
	    end

	val canvasItemNewCode =
	    ["OZ_BI_define (alice_item_new, 2, 1) {\n",
	     "\tGtkCanvasItem *ret;\n",
	     "\tOZ_declareForeignType(0, arg0, GtkCanvasGroup*);\n",
	     "\tGOZ_DECLARE_GINT(1, arg1);\n",
	     "\tret = gtk_canvas_item_new(arg0, arg1, NULL);\n",
	     "\tOZ_out(0) = OZ_makeForeignPointer(ret);\n",
	     "\treturn OZ_ENTAILED;\n",
	     "} OZ_BI_end\n"
	     ]

	val canvasPointsNewCode =
	    ["OZ_BI_define (alice_points_new, 1, 1) {\n",
	     "\tGtkCanvasPoints *ret;\n",
	     "\tGOZ_DECLARE_GINT(0, arg0);\n",
	     "\tret = gtk_canvas_points_new(arg0);\n",
	     "\tOZ_out(0) = OZ_makeForeignPointer(ret);\n",
	     "\treturn OZ_ENTAILED;\n",
	     "} OZ_BI_end\n",
	     "OZ_BI_define (alice_points_put, 3, 0) {\n",
	     "\tOZ_declareForeignType(0, arg0, GtkCanvasPoints*);\n",
	     "\tGOZ_DECLARE_GINT(1, arg1);\n",
	     "\tGOZ_DECLARE_GINT(2, arg2);\n",
	     "\targ0->coords[arg1] = (double) arg2;\n",
	     "\treturn OZ_ENTAILED;\n",
	     "} OZ_BI_end\n"
	     ]

	val canvasItemSetCode =
	    ["OZ_BI_define (alice_item_set, 3, 0) {\n",
	     "\tOZ_declareForeignType(0, arg0, GtkCanvasItem*);\n",
	     "\tOZ_Term t_arg1 = OZ_deref(OZ_in(1));\n",
             "\tif (OZ_isVariable(t_arg1)) {\n",
	     "\t  OZ_suspendOn(t_arg1);\n",
	     "\t}\n",
	     "\tchar *arg1 = strdup(OZ_stringToC(t_arg1, NULL));\n",
	     "\tOZ_Term ptr = OZ_deref(OZ_in(2));\n",
	     "\tif (OZ_isInt(ptr)) {\n",
	     "\t  int val = OZ_intToC(ptr);\n",
	     "\t  gtk_canvas_item_set(arg0, arg1, val, NULL);\n\t}\n",
	     "\telse if (OZ_isFloat(ptr)) {\n",
	     "\t  double val = OZ_floatToC(ptr);\n",
	     "\t  gtk_canvas_item_set(arg0, arg1, val, NULL);\n\t}\n",
	     "\telse if (OZ_isVirtualString(ptr, NULL)) {\n",
             "\t  int n;\n",
	     "\t  char *val = strdup(OZ_stringToC(ptr, &n));\n",
	     "\t  gtk_canvas_item_set(arg0, arg1, val, NULL);\n\t}\n",
	     "\telse if (OZ_isForeignPointer(ptr)) {\n",
	     "\t  void *val = OZ_getForeignPointer(ptr);\n",
	     "\t  gtk_canvas_item_set(arg0, arg1, val, NULL);\n\t}\n",
	     "\telse if (OZ_isVariable(ptr)) {\n",
	     "\t  OZ_suspendOn(ptr);\n\t}\n",
	     "\treturn OZ_ENTAILED;\n",
	     "} OZ_BI_end\n"
	     ]

	fun createBuiltin(ps, FUNCTION("ItemNew", _))             = app ps canvasItemNewCode
	  | createBuiltin(ps, FUNCTION("PointsNew", _))           = app ps canvasPointsNewCode
	  | createBuiltin(ps, FUNCTION("ItemSet", _))             = app ps canvasItemSetCode
	  | createBuiltin(ps, FUNCTION(name, FUNCARGS(rt, args))) =
	    let
		val coreName = ozNameToCoreName name
		val inArgs   = collectInArgs args
		val outArgs  = collectOutArgs(args, rt)
	    in
		(printOpening(ps, (coreName, Int.toString inArgs, Int.toString outArgs));
		 printRetVal(ps, rt);
		 printArgDecl(ps, args);
		 printRetAssign(ps, rt);
		 printCall(ps, coreName, args);
		 handleRetVal(ps, rt);
		 handleRetArgs(ps, 0, args);
		 printClosing(ps))
	    end
	  | createBuiltin(ps, _)                                  = ()

	fun createInterface(ps, FUNCTION("ItemNew", _)) =
	    ps "\t{\"itemNew\", 2, 1, alice_item_new},\n"
	  | createInterface(ps, FUNCTION("ItemSet", _)) =
	    ps "\t{\"itemSet\", 3, 0, alice_item_set},\n"
	  | createInterface(ps, FUNCTION("PointsNew", _)) =
	    (ps "\t{\"pointsNew\", 1, 1, alice_points_new},\n";
	     ps "\t{\"pointsPut\", 3, 0, alice_points_put},\n")
	  | createInterface(ps, FUNCTION(name, FUNCARGS(rt, args))) =
	    let
		val coreName = ozNameToCoreName name
		val inArgs   = collectInArgs args
		val outArgs  = collectOutArgs(args, rt)
	    in
		(ps "\t{\""; ps (firstLower name); ps "\", "; ps (Int.toString inArgs); ps ", ";
		 ps (Int.toString outArgs); ps ", alice_"; ps coreName; ps "},\n")
	    end
	  | createInterface(ps, _)                                   = ()

	val canvasHeader =
	    ["/* This File is auto-generated. Please do not edit. */\n\n",
	     "#include <mozart.h>\n",
	     "#include <goz_support.h>\n",
	     "#include <gtk/gtk.h>\n",
	     "#include <gtk-canvas.h>\n",
	     "#include <string.h>\n\n",
	     "/* Define Builtin Functions */\n\n"]

	val canvasInterface =
	    ["\n/* Define Interface */\n\n",
	     "static OZ_C_proc_interface oz_interface[] = {\n"]

	val canvasInit =
	    ["\t{0, 0, 0, 0}\n",
	     "};\n\n",
	     "OZ_C_proc_interface *oz_init_module() {\n",
	     "\tgtk_canvas_init();\n",
	     "\treturn oz_interface;\n",
	     "}\n"]

	fun createCanvasBinding cs =
	    let
		val os = TextIO.openOut "GtkCanvas.c"
		val ps = fn s => TextIO.output(os, s)
	    in
		(app ps canvasHeader;
		 app (fn x => createBuiltin(ps, x)) cs;
		 app ps canvasInterface;
		 app (fn x => createInterface(ps, x)) cs;
		 app ps canvasInit;
		 TextIO.closeOut os)
	    end

	val dataHeader =
	    [
	     "/* This file is auto-generated. Please do not edit. */\n\n",
	     "#include <mozart.h>\n",
	     "#include <gtk/gtk.h>\n\n",
	     "/* Define Struct Accessor builtins */\n"
	     ]

	val dataInterface =
	    ["\n/* Define Interface */\n\n",
	     "static OZ_C_proc_interface oz_interface[] = {\n"]

	val dataInit =
	    ["\t{0, 0, 0, 0}\n",
	     "};\n\n",
	     "OZ_C_proc_interface *oz_init_module() {\n",
	     "\treturn oz_interface;\n",
	     "}\n"]

	fun cutUnderscore name =
	    (case String.explode name of
		 #"_"::sr => String.implode sr
	       | _        => name)

	fun createDataConstructor(ps, name) =
	    let
		val biName = cutUnderscore name
	    in
		(ps "\nOZ_BI_define (alice_MemberNew"; ps biName; ps ", 0, 1) {\n";
		 ps "\tOZ_out(0) = OZ_makeForeignPointer(malloc(sizeof(struct ";
		 ps name; ps ")))\n";
		 ps "\treturn OZ_ENTAILED;\n";
		 ps "} OZ_BI_end\n";
		 ps "\nOZ_BI_define (alice_MemberDelete"; ps biName; ps ", 1, 0) {\n";
		 ps "\tOZ_declareForeignType(0, arg, struct "; ps name; ps "*)\n";
		 ps "\tfree(arg);\n";
		 ps "\t return OZ_ENTAILED;\n";
		 ps "} OZ_BI_end\n")
	    end

	fun createDataBuiltins(ps, name, nil)                  = ()
	  | createDataBuiltins(ps, name, MEMBER(sym, typ)::mr) =
	    let
		val biName = ((cutUnderscore name) ^ (firstUpper sym))
		val biType = ("struct " ^ name ^ "*")
	    in
		(ps "\nOZ_BI_define (alice_MemberGet"; ps biName; ps ", 1, 1) {\n";
		 ps "\tOZ_declareForeignType(0, arg, "; ps biType; ps ");\n";
		 ps "\tOZ_out(0) = OZ_makeForeignPointer(arg->"; ps sym; ps ");\n";
		 ps "\treturn OZ_ENTAILED;\n";
		 ps "} OZ_BI_end\n";
		 ps "\nOZ_BI_define (alice_MemberPut"; ps biName; ps ", 2, 0) {\n";
		 ps "\tOZ_declareForeignType(0, arg, struct "; ps name; ps "*);\n";
		 ps "\tOZ_declareForeignType(1, val, "; ps (argToString typ); ps ");\n";
		 ps "\targ->"; ps sym; ps " = val;\n";
		 ps "\treturn OZ_ENTAILED;\n";
		 ps "} OZ_BI_end\n";
		 createDataBuiltins(ps, name, mr))
	    end
	  | createDataBuiltins(ps, name, _::mr)                = createDataBuiltins(ps, name, mr)

	fun createMemberDataInterfaces(ps, name, nil)                = ()
	  | createMemberDataInterfaces(ps, name, MEMBER(sym, _)::mr) =
	    let
		val memName = (name ^ (firstUpper sym))
	    in
		(ps "\t{memberGet"; ps memName;
		 ps ", 1, 1, alice_MemberGet"; ps memName; ps "},\n";
		 ps "\t{memberPut"; ps memName;
		 ps ", 2, 0, alice_MemberPut"; ps memName; ps "},\n";
		 createMemberDataInterfaces(ps, name, mr))
	    end
	  | createMemberDataInterfaces(ps, name, _)                  = ()

	fun createDataInterfaces(ps, name, ms) =
	    let
		val biName = cutUnderscore name
	    in
		(ps "\t{memberNew"; ps biName; ps ", 0, 1, alice_MemberNew"; ps biName; ps "},\n";
		 ps "\t{memberDelete"; ps biName; ps ", 1, 0, alice_MemberDelete";
		 ps biName; ps "},\n";
		 createMemberDataInterfaces(ps, biName, ms))
	    end

	fun createDataBinding ss =
	    let
		fun isValid (STRUCT(name, _)) =
		    (case String.explode name of
			 (#"_")::(#"G")::(#"t")::(#"k")::_ => true
		       | (#"_")::(#"G")::(#"d")::(#"k")::_ => true
		       | (#"t")::v::_                      => Char.isDigit v
		       | _                                 => false)
		  | isValid _                 = false

		val newss = List.filter isValid ss
		val os    = TextIO.openOut "GtkData.c"
		val ps    = fn s => TextIO.output(os, s)

		fun handleBuiltins (STRUCT(name, ms)) =
		    (createDataConstructor(ps, name);
		     createDataBuiltins(ps, name, ms))
		  | handleBuiltins _                  = ()
	    in
		(app ps dataHeader;
		 app handleBuiltins newss;
		 app ps dataInterface;
		 app (fn STRUCT(name, ms) => createDataInterfaces(ps, name, ms) | _ => ()) newss;
		 app ps dataInit;
		 TextIO.closeOut os)
	    end

	fun alicegtk [inFile] =
	    let
		val (gtks, gtkcs, gdks, gdkcs, gcvs, gcvcs, strs) = collect inFile
		val ws                                            = ref (TextIO.openOut wrpGTKFile)
		val is                                            = ref (TextIO.openOut expGTKFile)
		val ss                                            = ref (TextIO.openOut sigGTKFile)
		val pws                                           = fn s => TextIO.output(!ws, s)
		val pis                                           = fn s => TextIO.output(!is, s)
		val sis                                           = fn s => TextIO.output(!ss, s)
	    in
		writeText(pws, wrpGTKPrefix);
		writeText(pis, expGTKPrefix);
		writeText(sis, sigGTKPrefix);
		app (fn x => emitWrapperInterface(pws, pis, sis, "gtk", x)) gtks;
		app (fn x => emitWrapperInterface(pws, pis, sis, "gtk", x)) gtkcs;
		writeText(pis, expGTKEnd); writeText(sis, sigGTKEnd);
		TextIO.closeOut(!ws); TextIO.closeOut(!is); TextIO.closeOut(!ss);
		ws := (TextIO.openOut wrpGDKFile);
		is := (TextIO.openOut expGDKFile);
		ss := (TextIO.openOut sigGDKFile);
		writeText(pws, wrpGDKPrefix);
		writeText(pis, expGDKPrefix);
		writeText(sis, sigGDKPrefix);
		app (fn x => emitWrapperInterface(pws, pis, sis, "gdk", x)) gdks;
		app (fn x => emitWrapperInterface(pws, pis, sis, "gdk", x)) gdkcs;
		writeText(pis, expGDKEnd); writeText(sis, sigGDKEnd);
		TextIO.closeOut(!ws); TextIO.closeOut(!is); TextIO.closeOut(!ss);
		ws := (TextIO.openOut wrpCanvasFile);
		is := (TextIO.openOut expCanvasFile);
		ss := (TextIO.openOut sigCanvasFile);
		writeText(pws, wrpCanvasPrefix);
		writeText(pis, expCanvasPrefix);
		writeText(sis, sigCanvasPrefix);
		natRef := "Native.";
		app (fn x => emitWrapperInterface(pws, pis, sis, "Canvas", x)) gcvs;
		natRef := "Native.";
		app (fn x => emitWrapperInterface(pws, pis, sis, "Canvas", x)) gcvcs;
		writeText(pis, expCanvasEnd); writeText(sis, sigCanvasEnd);
		TextIO.closeOut(!ws); TextIO.closeOut(!is); TextIO.closeOut(!ss);
		createCanvasBinding gcvs;
		OS.Process.success
	    end
	  | alicegtk _ =
	    (TextIO.output (TextIO.stdErr, "Usage: collect <infile>\n");
	     OS.Process.failure)
    end
