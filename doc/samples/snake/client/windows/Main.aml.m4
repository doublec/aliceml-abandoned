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


(* converts a Highscore.highscore into a string 
   differentiated by newlines *)
fun highscoreToString score =
    let
	fun toString' ({name, color, points,
			player, level}, str) =
	    ("Player: "    ^ name     ^ "  Color: "
	     ^ (Color.toString color) ^ "  Points: "
	     ^ (Int.toString points)  ^ "  Player: "
	     ^ (Int.toString player)  ^ "  Level: "
	     ^ (Int.toString level)   ^ "\n\n" ^ str)

	fun toString score = 
	    case Highscore.foldl toString' "" score of
		""	=> "Highscore is empty."
	    |	s 	=> s
    in
	toString score
    end

(* creates a string from a given time with format: h : m : s *)
  fun timeToString t =
    let
	val time    = Time.toSeconds t
	val hours   = time div 3600
	val minutes = (time mod 3600) div 60
	val seconds = (time mod 3600) mod 60
        
        fun toStr d =
            (if d <= 9 
                then "0"
                else "") ^ Int.toString d 
    in
	toStr hours ^ " : " ^ toStr minutes ^ " : " ^ toStr seconds 
    end



structure Main =
struct

    (* the modes in which the window can exists *)

    type radar_visibility = bool

    datatype mode = START | GAME of radar_visibility
    val mode = ref START


    (* builds the mainWindow, starting in START mode *)
    fun mkMainWindow ({connect, startServer}, gui) = 
	let

	    val mainWindow     = Gtk.windowNew Gtk.WINDOW_TOPLEVEL
			
	    (* initialising the canvas widget *)
	    val arena  = ArenaWidget.initialize ()
	    val canvas = ArenaWidget.toObject arena

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
	    val menuHighscore  = Gtk.menuNew ()
	    val menuLeave      = Gtk.menuNew ()
	    val menuMenuSingle = Gtk.menuItemNewWithLabel "Single-Player"
	    val menuMenuClient = Gtk.menuItemNewWithLabel "Multi-Player Client"
	    val menuMenuServer = Gtk.menuItemNewWithLabel "Multi-Player Server"
	    val menuQuit       = Gtk.menuItemNewWithLabel "Quit"
	    val menuQuitItem   = Gtk.menuItemNewWithLabel "Quit"

            val radar          = RadarWidget.initialize ()
	    val radarWidget    = RadarWidget.toObject radar

	    (* resets the window in START mode *)
	    fun reset' () =
		(Gtk.widgetSetSensitive (menuGiveUpItem, false);
		 Gtk.widgetSetSensitive (menuMenuItem, true);
		 Gtk.widgetHide canvas;
		 Gtk.widgetHide rightHBox;
                 Gtk.widgetHide radarWidget;
		 mode := START)

	    (* resets window and also shows [msg] when needed *)
	    fun reset NONE = reset' ()
	      | reset (SOME (title, msg)) =
		(Text.mkTextWindow (title, msg); reset' ())
		
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
		    fun gameFinished h = 
			(reset (SOME ("Highscore", highscoreToString h)))
			
		    fun update (difflist, pos)  = 
                        (ArenaWidget.update (arena, difflist, pos);
	                 RadarWidget.update (radar, difflist, pos))
		
                    val displayCountDown    = ref NONE
                    
                    fun countdown n =
                        let 
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
			    fun toString' ({name, 
					     color, 
					     points, 
					     gamePoints,
                                             lives = NONE}, str) =
				(str ^ name ^ ":  " 
				 ^ (Int.toString points) ^ "  +  " 
				 ^ (Int.toString gamePoints) ^ "\n\n")
			      | toString' ({name, 
					     color, 
					     points, 
					     gamePoints,
                                             lives = SOME lives}, str) =
				(str ^ name ^ ":  " 
				^ (Int.toString points) ^ "  +  " 
				^ (Int.toString gamePoints)
				^ " | lives: " ^ (Int.toString lives) ^ "\n\n")
			    fun toString () = List.foldl toString' "" plist
			in
			    Gtk.labelSetText (pointsLabel, toString ())
			end
		    
		    fun startLevel levinf = 
			(ArenaWidget.initLevel (arena, levinf);
			 RadarWidget.initLevel (radar, levinf);
			 Gtk.labelSetText (timeLabel, ""))
			
		    fun tick (pts, diffs, headPos, remainingTime) =
			let
			    val timeStr = (timeToString remainingTime) ^ "\n"
			in
			    Gtk.labelSetText (timeLabel, timeStr);
                            update (diffs, headPos);
                            Option.app updatePoints pts
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
		    disconnect' := disconnect
		end

	    fun giveUp () = (!giveUp' ()) 
	          handle Error.Error msg => reset (SOME ("ERROR!", msg))

	    fun disconnect () = (!disconnect' ())
	   	  handle Error.Error msg => reset (SOME ("ERROR!", msg))


	    (* the different behaviour by pressing the quit button *)
	    fun mainQuit () = OS.Process.exit OS.Process.success 

	    fun backToStart () = 
		(case !mode of
		    START  => OS.Process.exit OS.Process.success 
		  | GAME _ =>
			let
			    fun cancel () = ()
			    fun no ()     = ()
			    fun yes ()    = (disconnect (); reset NONE)
			    val answer    = {cancel, no, yes}
			in
			    Question.mkQuestionBox 
			    ("Sure?", "Go back to start?", answer)
			end)
		     
			
	    (* procedure called by pressing Client - button *)
	    fun startClient () = Connection.mkConnectToServer ({connect},
		                                             {reset, gameMode})

	    (* procedure called by pressing Server - button *)
	    fun startMultiPlayer () = 
		          ServerSettings.mkServerSettings ({startServer},
	                                                     {reset, gameMode})

            (* procedure called by pressing SinglePlayer - button *)
	    fun startSinglePlayer () = 
		           EnterName.mkEnterName {startServer, reset, gameMode}
 			
	    (* converts canvasEvents into direction or view_hint *)
	    fun key keyval = 
		 (case !mode of
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
	    Gtk.widgetHide radarWidget
		
	end
end
