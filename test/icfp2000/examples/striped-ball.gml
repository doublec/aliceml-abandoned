% striped-ball.gml
%
% A ball with red and yellow stripes
%

0.0  0.0  0.0  point /black
1.0  1.0  1.0  point /white
1.0  0.0  0.0  point /red
0.0  1.0  0.0  point /green
0.0  0.0  1.0  point /blue
1.0  0.0  1.0  point /magenta
1.0  1.0  0.0  point /yellow
0.0  1.0  1.0  point /cyan

{ /v /u /face
  v 11.0 mulf floor
  2 modi 0 eqi
  {red} {yellow} if
  1.0
  0.0
  1.0
} sphere

30.0 rotatez
40.0 rotatex

0.0 0.0 1.5 translate

/scene

1.0 1.0	1.0 point	% ambient
[]  			% lights
scene			% object
1			% depth
90.0			% fov
320 200			% wid ht
"striped-ball.ppm"	% output file
render
