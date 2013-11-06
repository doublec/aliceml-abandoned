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

color aliceblue;
aliceblue = (23/255,102/255,231/255);

beginfig(1)
  u = 30mm;
  xthick = 8mm;
  ythick = 4mm;
  xseriflen = 3mm;
  yseriflen = ythick;
  xshadowsize = 4.5mm;
  xextra = 1/4xthick;

  xbase = ythick;
  ybase = ythick;
  xsize = 23/4u-3/2xextra; %% 23 * 7.5mm - 3mm + 8mm = 2096 pixels at 300dpi
  ysize = 1.6u;
  ydot = 1.4u;

  col0 = aliceblue/2;
  col1 = col2 = col3 = col4 = col5 = 1/2[col0,background];

  drawalicewithshadow(32);
  drawbaseline;
  drawstrikethrough;
endfig;

end;
