
local
    val tree = Parser.parse "gtkclean.c"
in
    structure SGTK = MkSpecial (val space = Util.GTK)

    structure UGTK = MkNative(structure TypeManager = TypeManager
                              structure Special = SGTK
			      val space = Util.GTK
			      val tree = tree)
    structure CGTK = MkUnsafe(structure TypeManager = TypeManager
		 	      structure Special = SGTK
			      val space = Util.GTK
			      val tree = tree)
    structure EGTK = MkEnums(structure TypeManager = TypeManager
		 	     structure Special = SGTK
			     val space = Util.GTK
			     val tree = tree)

    structure SGDK = MkSpecial (val space = Util.GDK)

    structure UGDK = MkNative(structure TypeManager = TypeManager
			      structure Special = SGDK
			      val space = Util.GDK
			      val tree = tree)
    structure CGDK = MkUnsafe(structure TypeManager = TypeManager
		 	      structure Special = SGDK
			      val space = Util.GDK
			      val tree = tree)
    structure EGDK = MkEnums(structure TypeManager = TypeManager
		 	     structure Special = SGDK
			     val space = Util.GDK
			     val tree = tree)

    fun main dir = (OS.FileSys.chDir dir ;
		    UGTK.create() ;
		    CGTK.create() ;
		    EGTK.create() ;
		    UGDK.create() ;
		    CGDK.create() ;
		    EGDK.create() )
end
