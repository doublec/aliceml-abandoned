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
    save rectangular; pen rectangular;
    rectangular = pensquare xscaled xthick yscaled ythick;
    save yshadow; numeric yshadow;
    for i = nsteps downto 1:
      yshadow := i * 1mm/nsteps;
      drawalice(alice[i],xbase+i*xshadowsize/nsteps,ybase,
	(i-1)/(nsteps-1),background);
    endfor;
    yshadow := 0;
    drawalice(alice0,xbase,ybase,1.0,col0);
    draw (xbase,ybase-ythick/2)--(xbase+xsize,ybase-ythick/2)
      withpen rectangular withcolor background;
  endgroup
enddef;

def drawbaseline =
  draw (xbase,ybase)--(xbase+xsize,ybase)
    withcolor col0 withpen pensquare scaled ythick;
enddef;

def drawstrikethrough =
  pickup pensquare scaled ythick;
  draw (lft xbase,ybase+u/2)--(rt xbase+xsize,ybase+u/2)--
    (rt xbase+xsize,bot ybase)--(lft xbase,bot ybase)--cycle
    withpen pensquare scaled (ythick/8) withcolor col0;
enddef;

numeric u, xthick, ythick, xseriflen, yseriflen, xshadowsize, xextra;
numeric xbase, ybase, xsize, ysize, ydot;
color col[];
