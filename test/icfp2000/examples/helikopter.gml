% helikopter.gml

0.0  0.0  0.0  point /black
1.0  1.0  1.0  point /white
1.0  0.0  0.0  point /red
1.0  0.4  0.0  point /orange
0.0  0.5  0.0  point /green
0.0  0.0  1.0  point /blue
1.0  0.0  1.0  point /magenta
1.0  1.0  0.0  point /yellow
0.0  1.0  1.0  point /cyan

{ /v/u/f orange 1.0 0.0 1.0 } /bodysurface
{ /v/u/f red 1.0 0.0 1.0 }   /rotorsurface
{ /v/u/f white 1.0 0.0 1.0 } /rotorsurface2
{ /v/u/f green 1.0 0.0 1.0 } /vatsurface

bodysurface sphere	% cockpit

bodysurface cylinder	% extension
0.3 3.2 0.3 scale
-90.0 rotatez
0.0 0.7 0.0 translate
union

rotorsurface cube	% rotor blade
-0.5 -0.5 -0.5 translate
4.0 0.02 0.3 scale
rotorsurface2 cube
-0.5 -0.5 -0.5 translate
0.4 0.02 0.3 scale
/tip
tip 2.06 0.0 0.0 translate union
tip -2.06 0.0 0.0 translate union
/blade

blade blade		% rotor
90.0 rotatey
union
/rotor

rotor			% main rotor
30.0 rotatey
0.0 1.1 0.0 translate
union

rotor			% back rotor
20.0 rotatey
0.3 1.0 0.3 scale
-90.0 rotatex
2.8 0.65 -0.4 translate
union

vatsurface cylinder	% vat
0.1 2.8 0.1 scale
90.0 rotatez
1.4 -1.0 0.0 translate
/vat

vat			% right vat
0.0 0.0 0.8 translate
union

vat			% left vat
0.0 0.0 -0.8 translate
union

-5.0 rotatex
-30.0 rotatey
0.0 0.0 3.5 translate
/scene

-0.5 0.0 -1.0 point
white
pointlight /lig

0.2 0.2	0.2 point	% ambient
[lig]			% lights
scene			% object
1			% depth
90.0			% fov
320 200			% wid ht
"helikopter.ppm"	% output file
render
