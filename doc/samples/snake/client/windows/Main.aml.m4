(* Alice Snake 2.0 - TextWindow
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

import structure Gtk            from "GtkSupport"
import structure Gdk            from "GtkSupport"

import structure Ctrl           from "x-alice:/lib/utility/Ctrl"
import val log'                 from "../../common/Log"

import structure Pos            from "../../common/Pos"
import structure Color          from "../../common/Color"
import structure Protocol       from "../../common/Protocol"
import structure Highscore      from "../../common/Highscore"

import structure Text           from "Text"
import structure ArenaWidget    from "ArenaWidget"
import structure RadarWidget    from "RadarWidget"
import structure Question       from "Question"


open Ctrl

fun log'' t (f, v) = log' t ("MainWindow." ^ f, v)
    
__overload 'a log: string * 'a -> unit = log''

(* converts a Highscore.highscore into a string 
   differentiated by newlines *)
fun highscoreToString score =
    let
	val _ = log ("highscoreToString", "begins")
	fun pToString p = (if p <= 9 then "    " 
				else if p <= 99 then "   "
				else if p <= 999 then "  " 
				else if p <= 9999 then " "
			  	else "") ^ (Int.toString p)

	fun toString' ({name, color, points,
			player, level}, str) =
	    ("<span foreground='" ^ (Color.toHexStr color) ^ "'>" ^
             "Player: " ^ name ^
             "\nPoints: " ^ (pToString points)  ^ 
             "  Player: " ^ (pToString player)  ^ 
             "  Level : " ^ (pToString level)   ^ "\n\n</span>" ^ str)

	fun toString score = 
	    case Highscore.foldl toString' "" score of
		""	=> "Highscore is empty."
	    |	s 	=> s
    in
        toString score before log ("highscoreToString", "ends")
    end

(* creates a string from a given time with : h : m : s *)
  fun timeToString t =
    let
	val _ = log ("timeToString", "begins")
	val time    = Time.toSeconds t
	val hours   = LargeInt.toInt (time div Int.toLarge 3600)
	val minutes = 
                LargeInt.toInt ((time mod Int.toLarge 3600) div Int.toLarge 60)
	val seconds = 
                LargeInt.toInt ((time mod Int.toLarge 3600) mod Int.toLarge 60)
        
        fun toStr d =
            (if d <= 9 
                then "0"
                else "") ^ Int.toString d 
    in
        toStr hours ^ " : " ^ toStr minutes ^ " : " ^ toStr seconds 
	before log ("timeToString", "ends") 

    end



structure Main :> 
sig

    type mainwindow_type

    type start_client_cb = mainwindow_type -> unit

    type start_server_cb = mainwindow_type -> unit

    type start_single_cb = mainwindow_type -> unit

    type turn_cb         = Protocol.direction -> unit

    type change_view_cb  = Protocol.view_hint -> unit
	
    type quit_cb         = mainwindow_type -> unit

    type give_up_cb      = unit -> unit

    val mkMainWindow :   start_client_cb * start_server_cb * start_single_cb * 
	                                turn_cb * change_view_cb * 
					give_up_cb * quit_cb -> mainwindow_type

    val gameMode :       mainwindow_type -> unit

    val getWindow :      mainwindow_type -> Gtk.object

    val levelStart :     mainwindow_type * Protocol.level_info -> unit

    val levelTick :      mainwindow_type * Highscore.points list option * 
	                               Protocol.diff list * Pos.pos option * 
                                                              Time.time -> unit
 
    val levelCountDown : mainwindow_type * int -> unit

    val gameFinished :   mainwindow_type * Highscore.highscore -> unit

end =
struct


    (* the modes in which the window can exists *)

    type radar_visibility = bool

    datatype mode = START | GAME of radar_visibility



    type mainwindow_type = {object : Gtk.object,
			    arena  : ArenaWidget.arena_widget,
			    radar  : RadarWidget.radar_widget,
			    timeLabel : Gtk.object,
			    mode : mode ref,
			    updatePoints : Gtk.object -> 
			                         {name : string,
						  color : Color.color,
						  points : int,
						  gamePoints : int,
						  lives : int option} list
						                      -> unit,
			    update : Protocol.diff list * Pos.pos option * 
	                             ArenaWidget.arena_widget * 
	 			     RadarWidget.radar_widget -> unit,
			    countdown : (int option -> unit) option ref * 
			                 int * Gtk.object * 
				         ArenaWidget.arena_widget -> unit,
			    displayCountDown : (int option -> unit) option ref,
			    gameMode :  {mode : mode ref, 
					 menuGiveUpItem : Gtk.object,
					 menuMenuItem   : Gtk.object,
					 canvas         : Gtk.object,
					 rightHBox      : Gtk.object } -> unit,
			    menuGiveUpItem : Gtk.object,
			    menuMenuItem : Gtk.object,
			    rightHBox : Gtk.object,
			    pointsLabel : Gtk.object,
			    canvas : Gtk.object}

    type start_client_cb = mainwindow_type -> unit

    type start_server_cb = mainwindow_type -> unit

    type start_single_cb = mainwindow_type -> unit

    type turn_cb         = Protocol.direction -> unit

    type change_view_cb  = Protocol.view_hint -> unit

    type quit_cb         = mainwindow_type -> unit

    type give_up_cb      = unit -> unit


    fun getWindow (w : mainwindow_type) = #object w

    fun gameFinished (p : mainwindow_type, s) = 
	 (Text.mkTextWindow (getWindow p, "Highscore", highscoreToString s);())

    fun levelStart (p : mainwindow_type, levelInf) = 
	(ArenaWidget.initLevel (#arena p, levelInf);
	 RadarWidget.initLevel (#radar p, levelInf);
	 Gtk.labelSetText (#timeLabel p, ""))

    fun levelTick (p : mainwindow_type, points, diffs, pos, time) = 
	(Gtk.labelSetText (#timeLabel p, (timeToString time) ^ "\n");
	 #update p (diffs, pos, #arena p, #radar p);
	 Option.app (#updatePoints p (#pointsLabel p)) points)
	
    fun levelCountDown (p : mainwindow_type, n) = 
	          #countdown p (#displayCountDown p, n, #object p, #arena p)

    fun gameMode (p : mainwindow_type) = 
	             #gameMode p {mode = #mode p,
				  menuGiveUpItem = #menuGiveUpItem p,
				  menuMenuItem = #menuMenuItem p,
				  canvas = #canvas p,
				  rightHBox = #rightHBox p}
				  
		    
    (* builds the mainWindow, starting in START mode *)
    fun mkMainWindow (startClientCB, startServerCB, startSingleCB, 
		      turnCB, changeViewCB, giveUpCB, quitCB) = 
	let
	    val _ = log ("mkMainWindow", "starts")
	    val mainWindow     = Gtk.windowNew Gtk.WINDOW_TOPLEVEL
			
	    (* initialising the canvas widget *)
	    val arena  = ArenaWidget.initialize ()
	    val canvas = ArenaWidget.toObject arena

	    (* shows in which mode the window is *)
	    val mode = ref START

	    (* the menu bar items which sensitivity get 
	     changed some times *)
	    val menuMenuItem   = Gtk.menuItemNewWithLabel "Menu"
	    val menuGiveUpItem = Gtk.menuItemNewWithLabel "Give Up"
		
	    (* the points box items *)
	    val separator1  = Gtk.vseparatorNew ()
	    val pointsLabel = Gtk.labelNew ""
		
	    (* the countdown time label *)
	    val timeLabel = Gtk.labelNew ""
		
	    (* the box where time and points are in *)
	    val labelVBox      = Gtk.vboxNew (false, 2)
	    val rightHBox      = Gtk.hboxNew (false, 2)

            val dialogVBox     = Gtk.vboxNew (false, 5)
	    val dialogHBox     = Gtk.hboxNew (false, 5)
	    val menuBar        = Gtk.menuBarNew ()
	    val menuMenu       = Gtk.menuNew ()
	    val menuLeave      = Gtk.menuNew ()
	    val menuMenuSingle = Gtk.menuItemNewWithLabel "Single-Player"
	    val menuMenuClient = Gtk.menuItemNewWithLabel "Multi-Player Client"
	    val menuMenuServer = Gtk.menuItemNewWithLabel "Multi-Player Server"
	    val menuQuit       = Gtk.menuItemNewWithLabel "Quit"
	    val menuQuitItem   = Gtk.menuItemNewWithLabel "Quit"

            val radar          = RadarWidget.initialize ()
	    val radarWidget    = RadarWidget.toObject radar

	    val displayCountDown    = ref NONE

	    fun update (difflist, pos, arena, radar)  = 
		(ArenaWidget.update (arena, difflist, pos);
		 RadarWidget.update (radar, difflist, pos))
		
	    fun countdown (display, n, win, arena)  =
		let 
		    val _ = log ("countdown", n)
		    val displ = case !display of
			NONE    =>
			    let 
				val (width, height) = 
				    Gtk.windowGetSize win
				val d = 
				    ArenaWidget.startCountDown 
				    (arena, width div 2, height div 2)
			    in
				displayCountDown := SOME d; d
			    end
		      |   SOME d  => d
		in
		    if n = 0 
			then (displ NONE; display := NONE)
		    else displ (SOME n)
		end

	    (* updates the pointsLabel. different points are separated
	     by newlines *)
	    fun updatePoints pointsLabel plist =
		let
		    val _ = log ("updatePoints", "starts")
		    fun pToString p = (if p <= 9 then "    " 
				       else if p <= 99 then "   "
				       else if p <= 999 then "  " 
				       else if p <= 9999 then " "
				       else "") ^ (Int.toString p)
			
		    fun toString' ({name, 
				    color, 
				    points, 
				    gamePoints,
				    lives = NONE}, str) =
			(str ^ "<span foreground='" 
			 ^ (Color.toHexStr color) ^ "'><i>" 
			 ^ name ^ ":\n" 
			 ^ (pToString points) ^ "  +  " 
			 ^ (pToString gamePoints) ^ "\n\n</i></span>")
		      | toString' ({name, 
				    color, 
				    points, 
				    gamePoints,
				    lives = SOME lives}, str) =
			(str ^ "<span foreground='" 
			 ^ (Color.toHexStr color) ^ "'><i>" 
			 ^ name ^ ":\n" 
			 ^ (pToString points) ^ "  +  " 
			 ^ (pToString gamePoints)
			 ^ " | lives: " ^ (Int.toString lives) 
			 ^ "\n\n</i></span>")
		    fun toString () = List.foldl toString' "" plist
		in
		    log("updatePoints", "ends with setting markups") 
		    before
		    Gtk.labelSetMarkup (pointsLabel, toString ())
		end
		
	    fun gameMode {mode,
			  menuGiveUpItem,
			  menuMenuItem,
			  canvas,
			  rightHBox} =
		(mode := GAME(false);
		 Gtk.widgetSetSensitive (menuGiveUpItem, true);
		 Gtk.widgetSetSensitive (menuMenuItem, false);
		 Gtk.widgetShow canvas;
		 Gtk.widgetShow rightHBox;
		 log ("gameMode", "ends"))

	    val mainWindowWidget = {object = mainWindow,
				    radar,
				    arena,
				    mode,
				    displayCountDown,
				    canvas,
				    update,
				    updatePoints,
				    timeLabel,
				    countdown,
				    gameMode,
				    pointsLabel,
				    rightHBox,
				    menuMenuItem,
				    menuGiveUpItem}
				    

	    (* resets the window in START mode *)
	(*    fun reset' () =
		(print "resetting window\n";
                 Gtk.widgetSetSensitive (menuGiveUpItem, false);
		 Gtk.widgetSetSensitive (menuMenuItem, true);
		 Gtk.widgetHide canvas;
		 Gtk.widgetHide rightHBox;
                 Gtk.widgetHide radarWidget;
		 mode := START)*)

(*	    fun reset' () = Gtk.widgetDestroy mainWindow

	    (* resets window and also shows [msg] when needed *)
	    fun reset NONE = reset' ()
	      | reset (SOME (title, msg)) = (Text.mkTextWindow (title, msg);
                                             reset' ())
*)		

	    (* the different behaviour by pressing the quit button *)
	    fun mainQuit () = OS.Process.exit OS.Process.success 

	    fun backToStart () = 
		(case !mode of
		    START  => (log ("backToStart", "in START mode");
			       mainQuit ())
		  | GAME _ =>
			let
			    val _ = log ("backToStart", "in GAME mode")
			    fun cancel () = ()
			    fun no ()     = ()
			    fun yes ()    = giveUpCB ()
                            val answer    = {yes, no, cancel}
			in
			    Question.mkQuestionBox 
			    (mainWindow, "Sure?", 
			     "Do you really want to quit?", answer)
			end)
		     
			
	    (* procedure called by pressing Client - button *)
	    fun startClient () = 
                  (log ("startClient", "has been called");
		   startClientCB mainWindowWidget)
	    (* procedure called by pressing Server - button *)
	    fun startMultiPlayer () = 
		  (log ("startMultiPlayer", "has been called");
		   startServerCB mainWindowWidget)
            (* procedure called by pressing SinglePlayer - button *)
	    fun startSinglePlayer () = 
		  (log ("startSinglePlayer", "has been called");
		   startSingleCB mainWindowWidget)
	    (* procedure called by pressing GiveUp - button *)
	    fun giveUp () = (log ("giveUp", "has been called");
			     giveUpCB ())

	    (* converts canvasEvents into direction or view_hint *)
	    fun key keyval = 
		 (log ("key", "key has been pushed");
		  case !mode of
	              GAME b =>
			  (case Gdk.keyvalName keyval of
			        "Up"      => turnCB Protocol.UP
			    |   "Down"    => turnCB Protocol.DOWN
			    |   "Right"   => turnCB Protocol.RIGHT
			    |   "Left"    => turnCB Protocol.LEFT
			    | ("q" | "Q") => changeViewCB Protocol.PREV
			    | ("w" | "W") => changeViewCB Protocol.NEXT
                            | ("r" | "R") => (if b 
					      then Gtk.widgetHide radarWidget
                                              else Gtk.widgetShow radarWidget;
					      mode := GAME(not b))
			    |   _         => ())
		    |    _   => ())

	    ifdef([[GTK2]],[[
	    (* catches the canvas events *)
	    fun canvasEvent [Gtk.EVENT event] = 
		(case event of
		     Gdk.EVENT_KEY_PRESS {keyval, ...}	=> key keyval
		   |            _                  	=> ())
	      |  canvasEvent       x            = ()
	    ]],[[
	    fun canvasEvent [Gtk.EVENT event] = 
		(case event of
		     Gtk.GDK_KEY_PRESS {keyval, ...}	=> key keyval
		   |            _                  	=> ())
	      |  canvasEvent       x            = ()
            ]])


	in
	    log ("mkMainWindow", "initializing the whole stuff");
	    mode := START;
	    Gtk.windowSetTitle (mainWindow, "Alice Snake");
	    Gtk.windowSetDefaultSize (mainWindow, 800, 500);
	    Gtk.windowSetPosition (mainWindow, Gtk.WIN_POS_CENTER);
	    
	    (* building the menubar *)
	    Gtk.menuAppend (menuMenu, menuMenuSingle);
	    Gtk.menuAppend (menuMenu, menuMenuClient);
	    Gtk.menuAppend (menuMenu, menuMenuServer);
	    Gtk.menuAppend (menuLeave, menuGiveUpItem);
	    Gtk.menuAppend (menuLeave, menuQuit);

	    Gtk.menuItemSetSubmenu (menuMenuItem, menuMenu);
	    Gtk.menuItemSetSubmenu (menuQuitItem, menuLeave);

	    Gtk.menuBarAppend (menuBar, menuMenuItem);
	    Gtk.menuBarAppend (menuBar, menuQuitItem);

	    Gtk.widgetSetSensitive (menuGiveUpItem, false);

	    (* just signalconnecting *)
	    Gtk.signalConnect (mainWindow, "event",
			       ifdef([[GTK2]],[[
			       fn (_,x) => canvasEvent x);
                               ]],[[
                               canvasEvent);
                               ]])
	    Gtk.signalConnect (mainWindow, "delete-event", 
			       fn _ => mainQuit ());
	    Gtk.signalConnect (menuMenuSingle, "activate", 
			       fn _ => startSinglePlayer ());
	    Gtk.signalConnect (menuMenuClient, "activate", 
			       fn _ => startClient ());
	    Gtk.signalConnect (menuMenuServer, "activate", 
			       fn _ => startMultiPlayer ());
	    Gtk.signalConnect (menuQuit, "activate", 
			       fn _ => backToStart ());
	    Gtk.signalConnect (menuGiveUpItem, "activate",
			       fn _ => giveUp ());
	    
	    Gtk.boxPackStart (labelVBox, timeLabel, false, false, 0);
	    Gtk.boxPackStart (labelVBox, pointsLabel, false, false, 0);
	    Gtk.boxPackStart (labelVBox, radarWidget, false, false, 5);
	    
	    Gtk.boxPackStart (rightHBox, separator1, true, true, 0);
	    Gtk.boxPackStart (rightHBox, labelVBox, false, false, 0);

	    Gtk.boxPackStart (dialogHBox, canvas, true, true, 0);
	    Gtk.boxPackStart (dialogHBox, rightHBox, false, false, 0);

	    Gtk.boxPackStart (dialogVBox, menuBar, false, false, 0);
	    Gtk.boxPackStart (dialogVBox, dialogHBox, true, true, 0);

	    Gtk.containerAdd (mainWindow, dialogVBox);

	    Gtk.widgetShowAll mainWindow;

	    Gtk.widgetHide rightHBox;
	    Gtk.widgetHide radarWidget;
	    log ("mkmainWindow", "ends with initializing the stuff");

	    mainWindowWidget
	end
end
