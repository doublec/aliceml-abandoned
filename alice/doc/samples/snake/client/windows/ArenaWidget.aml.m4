(* Alice Snake 2.0 - GUI
*
*  Authors: Benedikt Grundmann / Sebastian Germesin
*
*  $Revision$
*
*  Last updated: $Date$ by $Author$
* 
*
*)

changequote([[,]])

import structure Gtk           from "GtkSupport"
import structure Gdk           from "GtkSupport"
import structure Canvas        from "GtkSupport"
import functor   MkRedBlackMap from "x-alice:/lib/data/MkRedBlackMap"
import val log'                from "../../common/Log"

import signature ARENAWIDGET   from "ARENAWIDGET-sig"
import structure Protocol      from "../../common/Protocol"
import structure Color         from "../../common/Color"

(* Overview:

 Tournament starts:
       val arena = ArenaWidget.initialize

 Level starts:
       initLevel (arena, levelinfo)
 
 every Tick:
       update (arena, difflist)
       changeView (arena, (x, y))

 new Level starts:
       initLevel (arena, levelinfo)

...

*)

fun log'' t (f, v) = log' t ("ArenaWidget." ^ f, v)
    
__overload 'a log: string * 'a -> unit = log''

structure ArenaWidget :> ARENAWIDGET =
struct

    structure P = Protocol

    (* some values *)
    val cellSize      =  20
    val windowWidth   = 400
    val windowHeight  = 300
    val cellSize2     = cellSize     div 2
    val windowHeight2 = windowHeight div 2
    val windowWidth2  = windowWidth  div 2
	

    type arena_field = Gtk.object * P.field
	
    type item_cache  = (int * int * Gtk.object * P.field) list ref

    type arena_widget = 
	{
	 canvas    : Gtk.object,
	 state     : arena_field Array2.array ref,
	 itemCache : item_cache
	 }

    fun colorConv n = Real.round (65535.0 * n)
	
    local
	
	val colormap = Gdk.colormapGetSystem ()
	    
	structure CMap = MkRedBlackMap (Color)
	    
	fun conv c = 
	    let
		val (r, g, b) = Color.rgb c
		val color = Gdk.colorNew (colorConv r, 
					  colorConv g, 
					  colorConv b)
	    in
		Gdk.colormapAllocColor (colormap, color, false, true);
		color
	    end

	fun convArena (r, g, b) = 
	    let
		val color = Gdk.colorNew (colorConv r, 
					  colorConv g, 
					  colorConv b)
	    in
		Gdk.colormapAllocColor (colormap, color, false, true);
		color
	    end
	
	val map  = List.foldl (fn (c, map) =>
			       CMap.insertDisjoint (map, c, conv c)) CMap.empty
	    Color.all
	    
    in
	fun color c  = CMap.lookupExistent (map, c);
	val black    = convArena (0.0, 0.0, 0.0);
	val green    = convArena (0.32, 0.66, 0.17);
	val red      = convArena (1.0, 0.0, 0.0);
	val white    = convArena (1.0, 1.0, 1.0)
        val goodyCol = convArena (0.42, 0.58, 0.24) 	    
    end

    val backgroundColor = green

    (* some procedures to work on item_cache *)
    fun addToItemCache (r, c, object, field, ca) =
	ca := (r, c, object, field) :: !ca

    fun removeFromItemCache (r, c, object, field, ca) = 
	let
	   val _ = log ("remoceFromItemCache", "removing oject from ItemCache")
	    val newList = 
		List.filter (fn (v, w, x, y) => 
			     not(r = v andalso
				 c = w andalso
				 field = y)) (!ca)
	in
	    ca := newList
	end


    fun initialize () = 
        let
	    val canvas = Canvas.new 0

        in
	    {
	        canvas,
	        state     = ref (Array2.array(0, 0, (Gtk.null, P.EMPTY))),
		itemCache = ref nil
	    }
        end
	
    fun toObject ({canvas, ... } : arena_widget) : Gtk.object  = canvas
	
    fun initLevel ({canvas, state, itemCache}, 
		   {dimensions = dim} : P.level_info) =
	let
	    val _ = log ("initLevel", "starts")
	    fun destroyOldItem (obj, _) = if obj <> Gtk.null 
                                             then Gtk.objectDestroy obj
					  else ()
	    val _ = Array2.app Array2.RowMajor destroyOldItem (!state)   
	    val (rows, cols) = dim
	    val _ = Gtk.widgetSetSizeRequest (canvas, windowWidth, 
					      windowHeight)
	    val _ = Canvas.setScrollRegion (canvas, 0.0, 0.0,
					       Real.fromInt(cols * cellSize),
					       Real.fromInt(rows * cellSize))
	    val root = Canvas.root canvas
	    val (x1, y1, x2, y2) = (0.0, 0.0,
				    Real.fromInt (cols * cellSize),
				    Real.fromInt (rows * cellSize))
	    val _ = Canvas.createRect (root, x1, y1, x2, y2, green, green)
	    val arena = Array2.array (rows, cols, (Gtk.null, P.EMPTY))
	in
	    state     := arena;
	    itemCache := nil;
	    log ("initLevel", "ends")
	end
	

    fun insertHead (canvas, x, y, d, snakecol) =
	let
	    val (ex1, ey1, ex2, ey2) = 
		case d of
		    P.UP    => (2 * cellSize div 10,
				3 * cellSize div 10,
				5 * cellSize div 10,
				3 * cellSize div 10)
		  | P.DOWN  => (2 * cellSize div 10,
				4 * cellSize div 10,
				5 * cellSize div 10,
				4 * cellSize div 10)
		  | P.LEFT  => (3 * cellSize div 10,
				2 * cellSize div 10,
				3 * cellSize div 10,
				5 * cellSize div 10)
		  | P.RIGHT => (4 * cellSize div 10,
				2 * cellSize div 10,
				4 * cellSize div 10,
				5 * cellSize div 10)
		    
	    val tonglist = 
		case d of
		    P.UP    => [(4 * cellSize div 10),
				(1 * cellSize div 10),
				(4 * cellSize div 10),
			       (~3 * cellSize div 10)]
		  | P.DOWN  => [(4 * cellSize div 10),
				(8 * cellSize div 10),
				(4 * cellSize div 10),
			       (11 * cellSize div 10)]
		  | P.LEFT  => [(1 * cellSize div 10),
				(4 * cellSize div 10),
			       (~3 * cellSize div 10),
			        (4 * cellSize div 10)]
		  | P.RIGHT => [(8 * cellSize div 10),
				(4 * cellSize div 10),
			       (11 * cellSize div 10),
				(4 * cellSize div 10)]

	    val head = Canvas.createGroup (canvas, real x, real y)

	    val ground = Canvas.createEllipse (head, 0.0, 0.0,
					       real cellSize,
					       real cellSize, snakecol, 
					       snakecol)

	    val tongue = Canvas.createLine (head, tonglist, red, 
					    cellSize div 10)

	    val leftEye = Canvas.createRect (head, real ex1, real ey1,
					     real (ex1 + 2), real (ey1 + 2),
					     black, black)
		
	    val rightEye = Canvas.createRect (head, real ex2, real ey2, 
					      real (ex2 + 2), real (ey2 + 2),
					      black, black)
	in
	    head
	end
	    
    fun insertBody (canvas, x, y, snakecol) =
	let
	    val body = Canvas.createGroup (canvas, real x, real y)

	    val ground = Canvas.createEllipse (body, 0.0, 0.0, 
					       real cellSize,
					       real cellSize,
					       snakecol, snakecol)
	in
	    body
	end

    fun insertClosedGate (canvas, x, y, gatecol) = 
	let
	    val gate = Canvas.createGroup (canvas, real x, real y)
		
	    val ground = Canvas.createRect (gate, 0.0, 0.0, 
					    real cellSize,
					    real cellSize,
					    gatecol, black)
	in
	    gate
	end

    fun insertOpenGate (canvas, x, y, gatecol) = 
	let
	    val gate = Canvas.createGroup (canvas, real x, real y)
		
	    val ground = Canvas.createRect (gate, 0.0, 0.0, 
					    real cellSize,
					    real cellSize,
					    gatecol, black)

	    val opens = Canvas.createRect (gate, 3.0, 3.0,
					   real (cellSize - 3),
					   real (cellSize - 3),
					   black, black)
	in
	    gate
	end

    fun insertWall (canvas, x, y) = 
	let
	    val wall = Canvas.createGroup (canvas, real x, real y)
		
	    val ground = Canvas.createRect (wall, 0.0, 0.0, 
					    real cellSize,
					    real cellSize,
					    red, black)
	in
	    wall
	end

    fun insertGoodie (canvas, x, y, n) =
	(* where n is the amount of a goodie *)
	let
	    val goodie = Canvas.createGroup (canvas, real x, real y)
		
	    val ground = Canvas.createEllipse (goodie, 0.0, 0.0, 
					       real cellSize,
					       real cellSize,
					       goodyCol, black)
	in
	    goodie
	end

    fun createNewFieldObj (r, c, field, arena, canvas) = 
	let
	    val x = cellSize * c
	    val y = cellSize * r
	in
	    (* place the new one *)
	    case field of
		P.EMPTY                => 
		    Array2.update (arena, r, c, (Gtk.null, field))
	      | P.WALL                 => 
		    Array2.update (arena, r, c, 
				   (insertWall (canvas, x, y), field))
	      | P.GOODY n              => 
		    Array2.update (arena, r, c, 
				   (insertGoodie (canvas, x, y, n), field))
	      | P.GATE (col, P.CLOSED) => 
		    Array2.update (arena, r, c, 
				   (insertClosedGate (canvas, x, y, 
						      color col),
				    field))
	      | P.GATE (col, P.OPEN)   => 
		    Array2.update (arena, r, c, 
				   (insertOpenGate (canvas, x, y, 
						    color col), field))
	      | P.SNAKE_HEAD (d, col) => 
		    Array2.update (arena, r, c, 
				   (insertHead (canvas, x, y, d, color col), 
				    field))
	      | P.SNAKE_BODY col      => 
		    Array2.update (arena, r, c, 
				   (insertBody (canvas, x, y, color col), 
				    field))
	end

    fun moveFieldObjTo (obj, oldR, oldC, newR, newC, field, arena) =
	let
	    val deltaY = Real.fromInt (cellSize * (newR - oldR))
	    val deltaX = Real.fromInt (cellSize * (newC - oldC))
	in
	    Canvas.itemMove (obj, deltaX, deltaY);
	    Canvas.itemShow obj;
	    Array2.update (arena, newR, newC, (obj, field))
	end


    fun changeField (object, oldField, r, c, field, cache, arena, canvas) = 
	(if object <> Gtk.null orelse oldField <> P.EMPTY
	     then (addToItemCache (r, c, object, oldField, cache);
		   Canvas.itemHide object)
	 else ();
	 case List.find (fn (_, _, _, f) => f = field) (!cache) of
	     NONE                      => 
		 createNewFieldObj (r, c, field, arena, canvas)
	   | SOME (row, col, obj, fld) => 
		 (removeFromItemCache (row, col, obj, fld, cache);
		  moveFieldObjTo (obj, row, col, r, c, fld, arena)))

    fun insert (a as (cache, arena, canvas)) ((r, c), field) = 
	let
	    val (rows, cols) = Array2.dimensions arena
	    val (obj, f) = Array2.sub (arena, r, c)
	in
	    if f <> field
	    then changeField (obj, f, r, c, field, cache, arena, canvas)
	    else ()
	end

	
    fun arenaWidth (a:arena_widget) =
	let
	    val (_, cols) = Array2.dimensions (! ( #state a))
	in
	    cols * cellSize
	end


    fun arenaHeight (a:arena_widget) =
	let
	    val (rows, _) = Array2.dimensions (! ( #state a))
	in
	    rows * cellSize
	end


    fun changeView (a:arena_widget, (y, x)) =
	let
	    val _ = log ("changeView", "starts")
	    val obj = toObject a
	    val minX = 0
	    val maxX = arenaWidth a
	    val minY = 0
	    val maxY = arenaHeight a
	    val x    = x * cellSize - windowWidth2
	    val y    = y * cellSize - windowHeight2

	in
	    Canvas.scrollTo (obj, x, y);
	    log ("changeView", "ends")
	end

    fun update (a as {canvas, state = ref arena, itemCache}, difflist, pos) =
	let
            val _ = log ("update", "start")
	    val root = Canvas.root canvas
	(*    val (x1, y1, x2, y2) = Canvas.getScrollRegion (0, 0, 0, 0)

	    val _ = if Config.platform = WIN32
			then Canvas.setScrollRegion (x1, y1, x2, y2)
		    else ()*)
	(* TODO: wie war das vorher? *)
	in
	    Canvas.freeze canvas;
	    List.app (insert (itemCache, arena, root)) difflist;
            (case pos of
                NONE    => ()
            |   SOME p  => changeView (a, p));
	    Canvas.thaw canvas;
	    Canvas.updateNow canvas;
	    log ("update", "ends")
	end 

    fun startCountDown (a, w, h) =
        let
            val _ = log ("startCountDown", "start")
	    val obj         = toObject a
            val oldNumber   = ref (Gtk.null)
            val color       = white
           
            fun changeObj new =
                (if !oldNumber <> Gtk.null 
                    then Gtk.objectDestroy (!oldNumber)
                    else ();
                 oldNumber := new)

 
            fun display NONE =
                changeObj (Gtk.null)
              | display (SOME n) =
                let
                    val root    = Canvas.root obj
                    val num     = Int.toString n
		    ifdef([[GTK2]],[[
	            val font   = "Sans 30"
		    ]],[[
		    val font   = "-*-times-bold-*-*-*-30-*-*-*-*-*-*-*"
		    ]])
                    val (x, y) = Canvas.getScrollOffsets (obj, 0, 0)
                    val x      = Real.fromInt (x + w)
                    val y      = Real.fromInt (y + h)
                    val anchor = Gtk.ANCHOR_CENTER
                        
                    val numObj  = Canvas.createText (root, num, font, x, y, 
						     color,anchor)
                in
                    changeObj numObj
                end
        in
            display before log ("startCountDown", "ends")
        end
                
end
