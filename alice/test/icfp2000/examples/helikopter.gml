% helikopter.gml

0.0  0.0  0.0  point /black
1.0  1.0  1.0  point /white
1.0  0.0  0.0  point /red
1.0  0.5  0.0  point /orange
%1.0  0.3  0.0  point /brown
0.0  0.7  0.0  point /green
0.0  0.5  0.0  point /darkgreen
0.0  0.0  1.0  point /blue
1.0  0.0  1.0  point /magenta
1.0  1.0  0.0  point /yellow
0.0  1.0  1.0  point /cyan
0.2  0.0  0.3  point /darkpurple

{ /v/u/f orange 0.2 0.8 1.0 } /bodysurface
%{ /v/u/f v 0.25 lessf {brown} {orange} if 0.2 0.8 1.0 } /bodysurface2

{ /v/u/f
  f 4 modi 2 lessi
  {
     u 0.1 lessf
     {white}
     {0.9 u lessf {white} {red} if}
     if
  }
  {white}
  if
  1.0 0.0 1.0
} /rotorsurface

{ /v/u/f darkgreen 1.0 0.0 1.0 } /vatsurface

%{ /v/u/f v 33.0 mulf floor 2 modi 0 eqi {darkgreen} {green} if 1.0 0.0 1.0 } /vatsurface

{ /v/u/f darkpurple 0.1 0.9 1.0 } /windowsurface

bodysurface sphere	% main body

bodysurface cylinder	% extension
0.3 3.2 0.3 scale
-90.0 rotatez
0.0 0.7 0.0 translate
union

bodysurface cylinder	% cockpit
0.5 2.0 0.5 scale
90.0 rotatex
-0.7 0.5 -1.0 translate
difference

windowsurface sphere	% window
0.92 uscale
union

rotorsurface cube	% rotor blade
-0.5 -0.5 -0.5 translate
4.8 0.02 0.3 scale
/blade

blade blade		% rotor
90.0 rotatey
union
/rotor

rotor			% main rotor
70.0 rotatey
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
-34.0 rotatey
0.0 0.0 3.0 translate
/scene

-0.5 0.0 -1.0 point
white
pointlight /lig1

-3.9 4.5 -1.0 point
white
pointlight /lig2

0.2 0.2	0.2 point	% ambient
[lig1 lig2]		% light
scene			% object
1			% depth
90.0			% fov
320 200			% wid ht
"helikopter.ppm"	% output file
render
