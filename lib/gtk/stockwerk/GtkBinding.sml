local
    structure SGDK = MkSpecial (val space = Util.GDK)

    structure UGDK = MkNative(structure TypeManager = TypeManager
			      structure Special = SGDK
			      val space = Util.GDK)
    structure CGDK = MkUnsafe(structure TypeManager = TypeManager
		 	      structure Special = SGDK
			      val space = Util.GDK)
    structure EGDK = MkEnums(structure TypeManager = TypeManager
		 	     structure Special = SGDK
			     val space = Util.GDK)

    structure SGTK = MkSpecial (val space = Util.GTK)

    structure UGTK = MkNative(structure TypeManager = TypeManager
                              structure Special = SGTK
			      val space = Util.GTK)
    structure CGTK = MkUnsafe(structure TypeManager = TypeManager
		 	      structure Special = SGTK
			      val space = Util.GTK)
    structure EGTK = MkEnums(structure TypeManager = TypeManager
		 	     structure Special = SGTK
			     val space = Util.GTK)

    structure SGC  = MkSpecial (val space = Util.GNOMECANVAS)

    structure UGC  = MkNative(structure TypeManager = TypeManager
			      structure Special = SGC
			      val space = Util.GNOMECANVAS)
    structure CGC  = MkUnsafe(structure TypeManager = TypeManager
		 	      structure Special = SGC
			      val space = Util.GNOMECANVAS)
    structure EGC  = MkEnums(structure TypeManager = TypeManager
		 	     structure Special = SGC
			     val space = Util.GNOMECANVAS)


in

    fun main dir =
    let
	val tree = Parser.parse "gtkclean.c"
    in
	(OS.FileSys.chDir dir ;
	 UGTK.create tree ;
	 CGTK.create tree ;
	 EGTK.create tree ;
	 UGDK.create tree ;
	 CGDK.create tree ;
	 EGDK.create tree ;
	  UGC.create tree ;
	  CGC.create tree ;
	  EGC.create tree )
    end

    fun run _ =
    let
	val args = SMLofNJ.getArgs()
	val dir = if null args then "." else hd args
    in
        ( main dir ;
 	  OS.Process.exit OS.Process.success )
    end
      handle _ => OS.Process.exit OS.Process.failure

    fun compile() = SMLofNJ.exportFn("GtkBinding", run)
end
