%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2000
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

0.2 0.2 0.3 point /black1
0.4 0.4 0.5 point /black2
0.7 0.7 0.5 point /white1
1.0 1.0 0.8 point /white2

%%
%% Surface functions
%%

{ /col1 /col2
  { /v /u /face
    u 12.0 mulf floor 2 modi 1 eqi { col1 } { col2 } if
    0.5 0.5 1.0
  }
} /pieceSurface

white1 white2 pieceSurface apply /whitePieceSurface
black1 black2 pieceSurface apply /blackPieceSurface

%%
%% Board
%%

{ /v /u /face
  3 face lessi
  { % top, bottom: checkered
    0 u 8.0 mulf floor v 8.0 mulf floor addi
  }
  { face 2 modi 0 eqi
    { 0 } % front, left: striped black/white
    { 1 } % back, right: striped white/black
    if u 8.0 mulf floor
  } if 2 modi eqi { black1 } { white2 } if
  0.4 0.6 0.5
} cube -0.5 -1.0 -0.5 translate 8.0 0.3 8.0 scale /board

%%
%% Pawns
%%

{ /surface
  surface sphere 0.0 1.0 0.0 translate
  surface cylinder union
  surface sphere 0.0 2.3 0.0 translate difference
  surface sphere 0.8 uscale 0.0 2.5 0.0 translate union
  0.3 uscale
} /pawn

whitePieceSurface pawn apply /whitePawn
blackPieceSurface pawn apply /blackPawn

%%
%% Towers
%%

{ /surface
  surface cone 0.0 -1.0 0.0 translate 180.0 rotatez 1.0 5.0 1.0 scale
  surface plane 0.0 3.0 0.0 translate intersect
  surface cylinder
  surface cylinder 0.7 uscale
  surface plane 90.0 rotatex
  surface plane -90.0 rotatex 30.0 rotatey intersect /apex
  apex apex 60.0 rotatey union apex 120.0 rotatey union
  apex 180.0 rotatey union apex 240.0 rotatey union apex 300.0 rotatey union
  surface plane 180.0 rotatex intersect union
  0.0 0.5 0.0 translate difference
  0.0 3.0 0.0 translate union
  0.4 uscale
} /tower

whitePieceSurface tower apply /whiteTower
blackPieceSurface tower apply /blackTower

%%
%% The scene
%%

board

whiteTower -0.5 0.0 -3.5 translate union
blackTower 0.5 0.0 0.5 translate union
whitePawn 2.5 0.0 -1.5 translate union
blackPawn 1.5 0.0 -2.5 translate union

30.0 rotatey -20.0 rotatex 0.4 uscale 0.3 0.0 3.0 translate
/scene

0.0 0.0 -1.0 point
1.0 1.0 1.0 point pointlight /l

0.33 0.33 0.33 point [ l ] scene 3 60.0 640 480 "chess.ppm" render
