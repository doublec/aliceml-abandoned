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

color orange, yellow;
orange = 1/2[red,yellow];
yellow = red+green;

def drawcurve suffix $ =
  begingroup
    z$a = z$ shifted (-(xthick-xextra)/2,u-yshadow);
    z$b = z$ shifted (-u+xextra/2,2/3u-yshadow);
    z$c = z$ shifted (-2/3u+xextra/2,-yshadow);
    penpos$a(ythick,-90);
    penpos$b(xthick,0);
    penpos$c(xthick,0);
    penstroke z$a.e{left}..{down}z$b.e..z$c.e;
  endgroup;
enddef;

def drawthea(suffix $)(expr xx,yy) =
  draw (xx,yy+u)--(xx,yy+ythick/2) withpen rectangular;
  z$ = (xx,yy);
  drawcurve $;
enddef;

def drawthel(expr xx,yy) =
  draw (xx-xseriflen,yy+ysize)--(xx,yy+ysize)--(xx,yy+ythick/2)
    withpen rectangular;
enddef;

def drawthei(expr xx,yy) =
  draw (xx-xseriflen,yy+u)--(xx,yy+u)--(xx,yy+ythick/2)
    withpen rectangular;
  draw (xx,yy+ydot) withpen rectangular;
enddef;

def drawthec(suffix $)(expr xx,yy) =
  draw (xx-xthick+3/2xextra,yy+u)--(xx-xthick+3/2xextra,yy+u-yseriflen)
    withpen rectangular;
  z$ = (xx,yy);
  drawcurve $;
enddef;

def drawthee(suffix $)(expr xx,yy) =
  z$ = (xx,yy);
  drawcurve $;
  draw (xx,yy+u)--(xx,yy+u-ythick/2) withpen rectangular;
  z$u = z$ shifted (0,u-ythick);
  z$m = z$ shifted (-u,u/2);
  penpos$u(xthick,0);
  penpos$m(ythick,-90);
  penstroke z$u.e{down}..tension1.0and1.2..{left}z$m.e;
enddef;

def drawalice(suffix $)(expr xx,yy,i,c) =
  drawoptions(withcolor i[col1,c]); drawthea($thea,xx+3/2u,yy);
  drawoptions(withcolor i[col2,c]); drawthel(xx+2u,yy);
  drawoptions(withcolor i[col3,c]); drawthei(xx+5/2u,yy);
  drawoptions(withcolor i[col4,c]); drawthec($thec,xx+4u-xextra,yy);
  drawoptions(withcolor i[col5,c]); drawthee($thee,xx+21/4u-2xextra,yy);
enddef;

def drawalicewithshadow(expr nsteps) =
  begingroup
    save yshadow; numeric yshadow;
    for i = nsteps downto 1:
      yshadow := i * 1mm/nsteps;
      drawalice(alice[i],xbase+i*xshadowsize/nsteps,ybase,
	(i-1)/(nsteps-1),white);
    endfor;
    yshadow := 0;
    drawalice(alice0,xbase,ybase,1.0,col0);
    save baseline; path baseline;
    pickup pensquare scaled ythick;
    baseline = (xbase,ybase)--(xbase+xsize,ybase);
    draw baseline shifted (0,-ythick) withpen rectangular withcolor background;
    draw baseline withcolor col0;
  endgroup
enddef;

beginfig(1)
  numeric u, xthick, ythick, xseriflen, yseriflen, xshadowsize, xextra;
  u = 30mm;
  xthick = 8mm;
  ythick = 4mm;
  xseriflen = 3mm;
  yseriflen = ythick;
  xshadowsize = 9mm;
  xextra = 1/4xthick;

  numeric xbase, ybase, xsize, ysize, ydot;
  xbase = ythick;
  ybase = ythick;
  xsize = 23/4u-3/2xextra; %% 23 * 7.5mm - 3mm + 8mm = 2096 pixels at 300dpi
  ysize = 1.6u;
  ydot = 1.4u;

  color col[];
  col0 = black;
  col1 = 1/4[3/4red,white];
  col2 = 1/4[3/4orange,white];
  col3 = 1/4[3/4yellow,white];
  col4 = 1/4[3/4green,white];
  col5 = 1/4[3/4blue,white];

  pen rectangular;
  rectangular = pensquare xscaled xthick yscaled ythick;

  drawalicewithshadow(32);

  pickup pensquare scaled ythick;
  draw (lft xbase,ybase+u/2)--(rt xbase+xsize,ybase+u/2)--
    (rt xbase+xsize,bot ybase)--(lft xbase,bot ybase)--cycle
    withpen pensquare scaled (ythick/8) withcolor col0;
endfig;

end;
