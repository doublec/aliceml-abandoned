(*
 * Authors:
 *   Robert Grabowski <grabow@ps.uni-sb.de>
 *
 * Copyright:
 *   Robert Grabowski, 2003
 *
 * Last Change:
 *   $Date$ by $Author$
 *   $Revision$
 *
 *)

(*
  This is the main component of the generator. 
  It parses a C file and generates native, unsafe and enum components.
*)

local
    structure SGDK = MkSpecial (val space = Util.GDK)
    structure NGDK = MkNative(structure TypeManager = TypeManager
			      structure Special = SGDK
			      val space = Util.GDK)
    structure FGDK = MkNativeFields(structure TypeManager = TypeManager
				    structure Special = SGDK
				    val space = Util.GDK)
    structure UGDK = MkUnsafe(structure TypeManager = TypeManager
		 	      structure Special = SGDK
			      val space = Util.GDK)
    structure EGDK = MkEnums(structure TypeManager = TypeManager
		 	     structure Special = SGDK
			     val space = Util.GDK)


    structure SPAN = MkSpecial (val space = Util.PANGO)
    structure EPAN = MkEnums(structure TypeManager = TypeManager
			     structure Special = SPAN
			     val space = Util.PANGO)

    structure SGTK = MkSpecial (val space = Util.GTK)
    structure NGTK = MkNative(structure TypeManager = TypeManager
                              structure Special = SGTK
			      val space = Util.GTK)
    structure FGTK = MkNativeFields(structure TypeManager = TypeManager
				    structure Special = SGTK
				    val space = Util.GTK)
    structure UGTK = MkUnsafe(structure TypeManager = TypeManager
		 	      structure Special = SGTK
			      val space = Util.GTK)
    structure EGTK = MkEnums(structure TypeManager = TypeManager
		 	     structure Special = SGTK
			     val space = Util.GTK)

    structure SGC  = MkSpecial (val space = Util.GNOMECANVAS)
    structure NGC  = MkNative(structure TypeManager = TypeManager
			      structure Special = SGC
			      val space = Util.GNOMECANVAS)
    structure FGC  = MkNativeFields(structure TypeManager = TypeManager
				    structure Special = SGC
				    val space = Util.GNOMECANVAS)
    structure UGC  = MkUnsafe(structure TypeManager = TypeManager
		 	      structure Special = SGC
			      val space = Util.GNOMECANVAS)
    structure EGC  = MkEnums(structure TypeManager = TypeManager
		 	     structure Special = SGC
			     val space = Util.GNOMECANVAS)

in

    fun main dir file =
    let
	val tree = Parser.parse file
    in
	(OS.FileSys.chDir dir ;
	 NGTK.create tree ;
	 FGTK.create tree ;
	 UGTK.create tree ;
	 EGTK.create tree ;
	 EPAN.create tree ;
	 NGDK.create tree ;
	 FGDK.create tree ;
	 UGDK.create tree ;
	 EGDK.create tree ;
	  NGC.create tree ;
	  FGC.create tree ;
	  UGC.create tree ;
	  EGC.create tree )
    end

    fun run _ =
    let
	val args = SMLofNJ.getArgs()
	val (outdir,source) = 
	    case args of
		nil                 => (".", "gtkclean.c")
	      | [outdir']           => (outdir', "gtkclean.c")
	      | outdir'::source'::_ => (outdir', source')

    in
        ( main outdir source;
 	  OS.Process.exit OS.Process.success )
    end
      handle _ => OS.Process.exit OS.Process.failure

    fun compile() = SMLofNJ.exportFn("GtkBinding", run)
end
