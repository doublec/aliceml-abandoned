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
(*
import structure Canvas         from "GtkSupport"
*)
import structure Ctrl           from "x-alice:/lib/utility/Ctrl"

import structure Color          from "../../common/Color"
import structure Protocol       from "../../common/Protocol"
import structure Highscore      from "../../common/Highscore"

import structure Error          from "Error"
import structure EnterName      from "EnterName"
import structure ArenaWidget    from "ArenaWidget"
import structure Connection     from "Connection"
import structure ServerSettings from "ServerSettings"
import structure Text           from "Text"
import structure Question       from "Question"


import val log                  from "../../common/Log"



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

	fun toString () = 
	    Highscore.foldl toString' "" score
    in
	toString ()
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
    datatype mode = START | GAME
    val mode = ref START


    (* builds the mainWindow, starting in START mode *)
    fun mkMainWindow ({connect, startServer}, gui) = 
	let

	    val mainWindow     = Gtk.windowNew Gtk.WINDOW_TOPLEVEL
			
	    (* initialising the canvas widget *)
	    val arena = ArenaWidget.initialize ()
	    val canvas = ArenaWidget.toObject arena

(*	    (* setting the image shown at the beginning *)
	    fun startImage can = 
		let
		    val root = Canvas.root can
		    val image = 
			Gtk.imageNewFromFile ("client/windows/logo.png")
		    val ibox = Gtk.eventBoxNew ()
		    val args = 
			[("widget", Gtk.OBJECT ibox),
			 ("x", Gtk.REAL (Real.fromInt ~320)),
			 ("y", Gtk.REAL (Real.fromInt ~227)),
			 ("width", Gtk.REAL (Real.fromInt 640)),
			 ("height", Gtk.REAL (Real.fromInt 455))]
		    val _ = Gtk.containerAdd (ibox, image)
		    val item = 
			Canvas.itemCreate (root, 
						Canvas.widgetGetType (), 
						args)
		in
		    image
		end
	    val sImage = startImage canvas
*)
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

	    (* one of the main procedures. resets the window
	       in START mode and also the sensitivity of the menubar *)
	    fun reset' () =
		(Gtk.widgetSetSensitive (menuGiveUpItem, false);
		 Gtk.widgetSetSensitive (menuMenuItem, true);
		 Gtk.widgetHide canvas;
		 Gtk.widgetHide rightHBox;
		 mode := START)
		
	    (* resets window and also shows [msg] when needed *)
	    fun reset NONE = reset' ()
	      | reset (SOME (title, msg)) =
		(Text.mkTextWindow (title, msg); reset' ())
		
	    val _ = assert (Future.isFuture $ Promise.future gui) 
		        do Promise.fulfill (gui, {reset})

	    (* initializing the procedures for modelGame *)
	    val turn'       = ref (fn d => ())
	    val changeView' = ref (fn h => ())
	    val giveUp'     = ref (fn () => ())
	    val disconnect' = ref (fn () => ())
		
	    (* calling the procedure gameMode turns main window in GAME mode,
	     by setting and fullfilling guiGame and updating functionality *)
	    fun gameMode ({disconnect}, {turn, changeView, giveUp},
			  guiGameP) =
		let
		    fun gameFinished h = 
			(reset (SOME ("Highscore", highscoreToString h)))
			
		    fun update (difflist, pos)  = 
                        ArenaWidget.update (arena, difflist, pos)
		
                    val displayCountDown    = ref NONE	
                    
                    fun countdown n =
                        let val displ = case !displayCountDown of
                                NONE    =>
				let 
				    val (width, height) = 
					Gtk.windowGetSize mainWindow
				    val d = 
					ArenaWidget.startCountDown 
					(arena, width div 2, height div 2)
                                in
                                    displayCountDown := SOME d;
                                    d
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
			 Gtk.labelSetText (timeLabel, ""))		     
			
		    fun tick (pts, diffs, headPos, remainingTime) =
			let
			    val timeStr = timeToString remainingTime
			in
			    Gtk.labelSetText (timeLabel, timeStr);
                            update (diffs, headPos);
                            Option.app updatePoints pts
			end
	
		in
		    mode := GAME;
		    Gtk.widgetSetSensitive (menuGiveUpItem, true);
		    Gtk.widgetSetSensitive (menuMenuItem, false);
		    Gtk.widgetShow canvas;
		    Gtk.widgetShow rightHBox;
		    assert (Future.isFuture $ Promise.future guiGameP) 
		    do Promise.fulfill (guiGameP, {startLevel, 
						   countdown, 
						   tick,
						   gameFinished});
		    (* update procedures *)
		    turn' := turn;
		    changeView' := changeView;
		    giveUp' := giveUp;
		    disconnect' := disconnect
		end

	    fun giveUp () = (!giveUp' ()) 
	          handle Error.Error msg => reset (SOME ("ERROR!!", msg))

	    fun disconnect () = (!disconnect' ())
	   	  handle Error.Error msg => reset (SOME ("ERROR!!", msg))


	    (* the different behaviour by pressing the quit button *)
	    fun mainQuit () = OS.Process.exit OS.Process.success 

	    fun backToStart () = 
		(case !mode of
		    START => OS.Process.exit OS.Process.success 
		  | GAME  =>
			let
			    fun cancel () = ()
			    fun no () = ()
			    fun yes () = (disconnect (); reset NONE)
			    val answer = {cancel, no, yes}
			in
			    Question.mkQuestionBox 
			    ("Sure?", "Go back to start?", answer)
			end)
		     
			
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


	    (* procedure called by pressing Client - button *)
	    fun startClient () = ((*Canvas.itemHide sImage;*)
		Connection.mkConnectToServer ({connect},
		 {reset, gameMode}))

	    (* procedure called by pressing Server - button *)
	    fun startMultiPlayer () = ((*Canvas.itemHide sImage;*)
                (log ("MainWindow", "startMultiPlayer");
		ServerSettings.mkServerSettings ({startServer},
	        {reset, gameMode})))

            (* procedure called by pressing SinglePlayer - button *)
	    fun startSinglePlayer () = ((*Canvas.itemHide sImage *)
		EnterName.mkEnterName {startServer, reset, gameMode})
 			
	    (* converts canvasEvents into direction or view_hint *)
	    fun key keyval = 
		 (if !mode = GAME
		      then
			  case Gdk.keyvalName keyval of
			        "Up"      => !turn' Protocol.UP
			    |   "Down"    => !turn' Protocol.DOWN
			    |   "Right"   => !turn' Protocol.RIGHT
			    |   "Left"    => !turn' Protocol.LEFT
			    | ("q" | "Q") => !changeView' Protocol.PREV
			    | ("w" | "W") => !changeView' Protocol.NEXT
			    |   _         => ()
		  else ()) 
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
	    
	    Gtk.boxPackStart (rightHBox, separator1, true, true, 0);
	    Gtk.boxPackStart (rightHBox, labelVBox, false, false, 0);

	    Gtk.boxPackStart (dialogHBox, canvas, true, true, 0);
	    Gtk.boxPackStart (dialogHBox, rightHBox, false, false, 0);

	    Gtk.boxPackStart (dialogVBox, menuBar, false, false, 0);
	    Gtk.boxPackStart (dialogVBox, dialogHBox, true, true, 0);

	    Gtk.containerAdd (mainWindow, dialogVBox);

	    Gtk.widgetShowAll mainWindow;

	    Gtk.widgetHide rightHBox
		
	end
end
