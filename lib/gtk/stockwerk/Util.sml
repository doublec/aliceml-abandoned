
structure Util :> UTIL =
    struct
	fun firstLower'(X::Xr) = String.implode ((Char.toLower X)::Xr)
	  | firstLower' nil    = ""
	fun firstLower Xs = firstLower'(String.explode Xs)

	fun firstUpper'(X::Xr) = String.implode ((Char.toUpper X)::Xr)
          | firstUpper' nil    = ""
	fun firstUpper Xs = firstUpper'(String.explode Xs)

	fun replaceChar (c,r) s = 
               String.translate 
                  (fn c' => if c = c' then r else Char.toString c' ) 
                  s
	
	fun strUpper s = 
	       String.translate
                  (fn c => if Char.isUpper c 
			       then "_"^(Char.toString c)
                               else (Char.toString (Char.toUpper c)))
                  (firstLower s)

	fun separator s =
	    "-- "^s^"...\n"
(*	    "\n**** "^s^" ***************************************\n\n"*)

	fun indent n = if n = 0 then "" else "    " ^ (indent (n - 1))

        fun makeTuple sep e nil     = e
	  | makeTuple sep e [x]     = x
	  | makeTuple sep e (x::xr) = x ^ sep ^ (makeTuple sep e xr)	

        datatype spaces = GDK | GTK | GNOMECANVAS | PANGO

	val allSpaces = [GDK, GTK, GNOMECANVAS, PANGO]

	fun spaceName GDK         = "Gdk"
	  | spaceName GTK         = "Gtk"
	  | spaceName GNOMECANVAS = "GnomeCanvas"
	  | spaceName PANGO       = "Pango"

	fun spaceFuncPrefix GDK       = "gdk_"
	  | spaceFuncPrefix GTK       = "gtk_"
	  | spaceFuncPrefix GNOMECANVAS = "gnome_canvas_"
	  | spaceFuncPrefix PANGO       = "pango_"

	fun spaceEnumPrefix space = spaceName space
	fun spaceStructPrefix space = "_"^(spaceName space)
	   
	fun checkPrefix'(X::Xr, Y::Yr) = X = Y andalso checkPrefix'(Xr, Yr)
	  | checkPrefix'(nil, _)       = true
	  | checkPrefix' _             = false

	fun checkPrefix Xs Ys =
	    checkPrefix'(String.explode Xs, String.explode Ys)

	fun cutPrefix'(X::Xr, Ys as Y::Yr) =
	    if ((Char.toUpper X)  = (Char.toUpper Y)) then cutPrefix'(Xr, Yr)
	                                              else String.implode Ys
	  | cutPrefix'(nil, Ys)    = String.implode Ys
	  | cutPrefix'(X::Xr, nil) = ""

	fun cutPrefix(Xs, Ys) =
	    cutPrefix'(String.explode Xs, String.explode Ys)


	fun sep #"_" = true
	  | sep _    = false
	fun translateName str =
	    firstLower (String.concat (map firstUpper (String.tokens sep str)))

	fun computeWrapperName (space, str) =
	    translateName (cutPrefix(spaceFuncPrefix space, str))

	fun computeEnumName (space, str) =
	let
	    val n = cutPrefix(strUpper(spaceEnumPrefix space)^"_",str)
	in
	    if Char.isDigit(hd(String.explode n)) then str else n
	end

	fun filters fs xs = foldl (fn (f,e) => List.filter f e) xs fs
	fun funNot f x = not (f x)

	val contains = fn x => List.exists (fn x' => x=x')

	fun removeDuplicates' _ nil     ys = ys
	  | removeDuplicates' f (x::xr) ys = 
	      removeDuplicates' f xr (ys@(if List.exists (fn y=>f(x,y)) ys
	                                      then nil else [x]))
	fun removeDuplicates f xs = removeDuplicates' f xs nil

	type fileInfo = {name: string, intro: string list, outro: string list}
	type fileHandle = fileInfo * TextIO.outstream

	fun outputStrings (h as (_,f)) xs = TextIO.output (f, String.concat xs)

	fun openFile (info : fileInfo) =
	let
	    val f = TextIO.openOut (#name info)
            val h = (info, f) : fileHandle
	    val _ = outputStrings h (#intro info)
	in
            h
	end

	fun closeFile (h as (info, f) : fileHandle) =
	    ( outputStrings h (#outro info) ; TextIO.closeOut f )
	  
    end
