local
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
	 EGDK.create tree )
    end

    fun depend (file, me, names) =
    let
	val f = TextIO.openAppend file
    in
	( TextIO.output (f, me^": "^(Util.makeTuple " \\\n " "" names)^"\n") ;
	  TextIO.closeOut f )
    end
        handle _ => ()
end
