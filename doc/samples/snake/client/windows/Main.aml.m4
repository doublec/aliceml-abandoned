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

import structure Color          from "../../common/Color"
import structure Protocol       from "../../common/Protocol"
import structure Highscore      from "../../common/Highscore"

import structure Error          from "Error"
import structure EnterName      from "EnterName"
import structure ArenaWidget    from "ArenaWidget"
import structure RadarWidget    from "RadarWidget"
import structure Connection     from "Connection"
import structure ServerSettings from "ServerSettings"
import structure Text           from "Text"
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



structure Main =
struct

    (* the modes in which the window can exists *)

    type radar_visibility = bool

    datatype mode = START | GAME of radar_visibility


    (* builds the mainWindow, starting in START mode *)
    fun mkMainWindow ({connect, startServer}, gui) = 
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

	    (* resets the window in START mode *)
	(*    fun reset' () =
		(print "resetting window\n";
                 Gtk.widgetSetSensitive (menuGiveUpItem, false);
		 Gtk.widgetSetSensitive (menuMenuItem, true);
		 Gtk.widgetHide canvas;
		 Gtk.widgetHide rightHBox;
                 Gtk.widgetHide radarWidget;
		 mode := START)*)

	    fun reset' () = (Gtk.widgetDestroy mainWindow;
	                     mkMainWindow ({connect, startServer}, gui))

	    (* resets window and also shows [msg] when needed *)
	    fun reset NONE = reset' ()
	      | reset (SOME (title, msg)) = (Text.mkTextWindow (title, msg);
                                             reset' ())
		
	    val _ = if (Future.isFuture $ Promise.future gui) 
		        then Promise.fulfill (gui, {reset}) else ()

	    (* initializing the procedures for modelGame *)
	    val turn'       = ref (fn _ => ())
	    val changeView' = ref (fn _ => ())
	    val giveUp'     = ref (fn () => ())
	    val disconnect' = ref (fn () => ())
		
	    (* calling the procedure gameMode turns main window in GAME mode,
	     by setting and fullfilling guiGame and updating functionality *)
	    fun gameMode ({disconnect}, {turn, changeView, giveUp}, guiGameP) =
		let
		    val _ = log ("gameMode", "fullfilling guiGame")
		    fun gameFinished h = 
			(reset (SOME ("Highscore", highscoreToString h)))
			
		    fun update (difflist, pos)  = 
                        (ArenaWidget.update (arena, difflist, pos);
	                 RadarWidget.update (radar, difflist, pos))
		
                    val displayCountDown    = ref NONE
                    
                    fun countdown n =
                        let 
                            val _ = log ("countdown", n)
			    val displ = case !displayCountDown of
                                NONE    =>
				let 
				    val (width, height) = 
					Gtk.windowGetSize mainWindow
				    val d = 
					ArenaWidget.startCountDown 
					(arena, width div 2, height div 2)
                                in
                                    displayCountDown := SOME d; d
                                end
                            |   SOME d  => d
                        in
                            if n = 0 
                                then (displ NONE; displayCountDown := NONE)
                            else displ (SOME n)
                        end
		   		    
		    (* updates the pointsLabel. different points are separated
		     by newlines *)
		    fun updatePoints plist =
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
		    
		    fun startLevel levinf = 
			(ArenaWidget.initLevel (arena, levinf);
			 RadarWidget.initLevel (radar, levinf);
			 Gtk.labelSetText (timeLabel, ""))
			
		    fun tick (pts, diffs, headPos, remainingTime) =
			let
			    val _ = log ("tick", "starts")
			    val timeStr = (timeToString remainingTime) ^ "\n"
			in
			    Gtk.labelSetText (timeLabel, timeStr);
                            update (diffs, headPos);
                            log ("tick", "ends") 
			    before Option.app updatePoints pts
			end
	
		in
		    mode := GAME(false);
		    Gtk.widgetSetSensitive (menuGiveUpItem, true);
		    Gtk.widgetSetSensitive (menuMenuItem, false);
		    Gtk.widgetShow canvas;
		    Gtk.widgetShow rightHBox;
		    if (Future.isFuture $ Promise.future guiGameP) 
		    then Promise.fulfill (guiGameP, {startLevel, 
						    countdown, 
						    tick,
						    gameFinished}) else ();
		    (* update procedures *)
		    turn' := turn;
		    changeView' := changeView;
		    giveUp' := giveUp;
		    disconnect' := disconnect;
		    log ("gameMode", "ends")
		end

	    fun giveUp () = (log("giveUp", "has been called");!giveUp' ()) 
	          handle Error.Error msg => reset (SOME ("ERROR!", msg))

	    fun disconnect () = (log("disconnect", "hast been called");
				 !disconnect' ())
	   	  handle Error.Error msg => reset (SOME ("ERROR!", msg))


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
			    fun yes ()    = (disconnect (); reset NONE)
			    val answer    = {yes, no, cancel}
			in
			    Question.mkQuestionBox 
			    ("Sure?", "Do you really want to quit?", answer)
			end)
		     
			
	    (* procedure called by pressing Client - button *)
	    fun startClient () = 
                  (log ("startClient", "has been called");
		   Connection.mkConnectToServer ({connect}, {reset, gameMode}))
 	               handle Error.Error msg => reset (SOME ("Error!", msg))
	    (* procedure called by pressing Server - button *)
	    fun startMultiPlayer () = 
		  (log ("startMultiPlayer", "has been called");
		   ServerSettings.mkServerSettings ({startServer}, 
                                                            {reset, gameMode}))
 	               handle Error.Error msg => reset (SOME ("Error!", msg))
            (* procedure called by pressing SinglePlayer - button *)
	    fun startSinglePlayer () = 
		  (log ("startSinglePlayer", "has been called");
		   EnterName.mkEnterName {startServer, reset, gameMode})
 	               handle Error.Error msg => reset (SOME ("Error!", msg))

	    (* converts canvasEvents into direction or view_hint *)
	    fun key keyval = 
		 (log ("key", "key has been pushed");
		  case !mode of
	              GAME b =>
			  (case Gdk.keyvalName keyval of
			        "Up"      => !turn' Protocol.UP
			    |   "Down"    => !turn' Protocol.DOWN
			    |   "Right"   => !turn' Protocol.RIGHT
			    |   "Left"    => !turn' Protocol.LEFT
			    | ("q" | "Q") => !changeView' Protocol.PREV
			    | ("w" | "W") => !changeView' Protocol.NEXT
                            | ("r" | "R") => (if b 
					      then Gtk.widgetHide radarWidget
                                              else Gtk.widgetShow radarWidget;
					      mode := GAME(not b))
			    |   _         => ())
		    |    _   => ())
			handle Error.Error msg => reset (SOME ("Error!", msg))

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
	    log ("mkmainWindow", "ends with initializing the stuff")
	end
end
