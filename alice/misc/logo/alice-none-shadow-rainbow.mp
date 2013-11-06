%%%
%%% Author:
%%%   Leif Kornstaedt <kornstae@ps.uni-sb.de>
%%%
%%% Copyright:
%%%   Leif Kornstaedt, 2001
%%%
%%% Last change:
%%%   $Date$ by $Author$
%%%   $Revision$
%%%

input alice

color orange, yellow;
orange = 1/2[red,yellow];
yellow = red+green;

beginfig(1)
  u = 30mm;
  xthick = 8mm;
  ythick = 4mm;
  xseriflen = 3mm;
  yseriflen = ythick;
  xshadowsize = 9mm;
  xextra = 1/4xthick;

  xbase = ythick;
  ybase = ythick;
  xsize = 23/4u-3/2xextra; %% 23 * 7.5mm - 3mm + 8mm = 2096 pixels at 300dpi
  ysize = 1.6u;
  ydot = 1.4u;

  col0 = black;
  col1 = 1/4[3/4red,background];
  col2 = 1/4[3/4orange,background];
  col3 = 1/4[3/4yellow,background];
  col4 = 1/4[3/4green,background];
  col5 = 1/4[3/4blue,background];

  drawalicewithshadow(32);
  draw (xbase,ybase+u/2)--(xbase+xsize,ybase+u/2)
    withpen pensquare scaled (ythick/4) withcolor col0;
endfig;

end;
