functor $
import
   Application(exit)
   Property(get put)
   System(show)
define

%\define HaveReturn

\ifndef TOOLS
%\define TOOLS

{Property.put time foo(detailed:true)}

% simulation of real functions
\ifdef Oz_3
\ifdef HaveReturn
Return = FunReturn
\else
{System.printInfo '*** Warning Return/GetReturn not used\n'}
Return    
GetReturn 
\endif
\else
Return    
GetReturn 
\endif

fun {Genlist N}
   case N==0 then nil
   else N|{Genlist N-1}
   end
end


fun {Random N}
   (N*25 + 1345) mod 10000 + (N*713 + 1345) mod 100000
%   (N*25713 + 1345) mod 
end

fun {Randlist N}
   {RandlistAux N 0 nil}
end

fun {RandlistAux N Old L}
   case N==0 then L
   else Aux = {Random Old} in
      {RandlistAux N-1 Aux Aux|L}
   end
end

fun {CheckRand N M}
   Aux = {Random N} in
   case Aux==0 then M
   else {CheckRand Aux M+1} end
end


%{Show {CheckRand 0 0}}



fun {App X Y}
  case X of
     H|R then H|{App R Y}
  else X=nil Y
  end
end

proc {AppFunAux X Y}
  case X of
     H|R then Aux in {AppFunAux R Y} Aux={GetReturn} {Return H|Aux}
  else X=nil {Return Y}
  end
end

fun {AppFun X Y}
   {AppFunAux X Y}
   {GetReturn}
end

proc {Dotimes N P}
   case N==0 then skip
   else {P} {Dotimes N-1 P}
   end
end

fun {GetTime}
   Time = {Property.get time} % was run (run + gc)
in
   Time.total % was run
end

fun {Dobench P}
   TimeBefore = {GetTime}
   {P}
in
   {GetTime} - TimeBefore
end

fun {DobenchN N P}
   case N==0 then nil
   else {Dobench P}|{DobenchN N-1 P}
   end
end

fun {Sum L Res}
   case L of nil then Res
   [] H|R then {Sum R Res+H}
   end
end

fun {Avrg L Len}
   {Sum L 0} div Len
end

fun {SumSquare L Av}
   case L of nil then 0
   [] H|R then (H-Av)*(H-Av)+{SumSquare R Av}
   end
end

fun {StdDev L}
   Len = {Length L}
   Av = {Avrg L Len}
   Var = {Sqrt {Int.toFloat {SumSquare L Av}} / {Int.toFloat (Len-1)}}
in
   Var#(Var*100.0/{Int.toFloat Av})
end

fun {Dobenchavrg N P}
   Aux = {DobenchN N P}
in
   res({Avrg Aux N} Aux)
end

fun {Dobenchavrg2 Name Iter Size P1 P2}
   res(Avrg1 L1) = {Dobenchavrg Iter P1}
   res(Avrg2 L2) = {Dobenchavrg Iter P2}
in
   res(size: Size
       aaname: Name
       avrg: (Avrg2-Avrg1)
       results1: L1
       results2: L2)
end



\endif

%%
%% Benchmarks
%%

fun {Tak X Y Z}
   case Y<X then {Tak {Tak X-1 Y Z} {Tak Y-1 Z X} {Tak Z-1 X Y}}
   else Z 
   end
end

proc {TakAux X Y Z}
   case Y<X then Z1 Z2 Z3 in
      {TakAux X-1 Y Z}
      Z1 = {GetReturn}
      {TakAux Y-1 Z X}
      Z2 = {GetReturn}
      {TakAux Z-1 X Y}
      Z3 = {GetReturn}
      {TakAux Z1 Z2 Z3}
   else {Return Z}
   end
end

fun {TakFun X Y Z}
   {TakAux X Y Z}
   {GetReturn}
end

fun {Cpstak X Y Z}
   {CpstakAux X Y Z fun {$ A} A end}
end

fun {CpstakAux X Y Z K}
   case Y<X then
      {CpstakAux X-1 Y Z
       fun {$ V1}
	  {CpstakAux Y-1 Z X
	   fun {$ V2}	   
	      {CpstakAux Z-1 X Y fun {$ V3} {CpstakAux V1 V2 V3 K} end}
	   end}
       end}
   else
      {K Z}
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fun {Fib N}
   if N < 2 then 1 else {Fib N-2} + {Fib N - 1} end
%    case N
%    of 0 then 1
%    [] 1 then 1
%    [] N then {Fib N-2} + {Fib N-1}
%    end
end

fun {ConcFib N}
   case N
   of 0 then 1
   [] 1 then 1
   [] N then
      N1 = thread {ConcFib (N - 1)} end
      N2 = thread {ConcFib (N - 2)} end
   in
      N1 + N2
   end
end

proc {FibFunAux N}
   case 2<N then Aux1 N1 in
      {FibFunAux N-2}
      N1 = N-1
      Aux1 = {GetReturn}
      {FibFunAux N1}
      {Return Aux1+{GetReturn}}
   else {Return One} end
end

One=1

fun {FibFun N}
   {FibFunAux N}
   {GetReturn}
end

fun {FibF N}
   case 2.0<N then {FibF N-2.0} + {FibF N-1.0} else 1.0 end
end

proc {FibFFunAux N}
   case 2.0<N then Aux1 N1 in
      {FibFFunAux N-2.0}
      N1 = N-1.0
      Aux1 = {GetReturn}
      {FibFFunAux N1}
      {Return Aux1+{GetReturn}}
   else {Return OneF} end
end

OneF=1.0

fun {FibFFun N}
   {FibFFunAux N}
   {GetReturn}
end


fun {FibThread N}
   case 2<N then thread {FibThread N-2} end + thread {FibThread N-1} end else 1 end
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fun {TailRec N}
   case 0<N then {TailRec N - 1} else 0 end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fun {Nrev L}
   case L of
      H|R then {App {Nrev R} H|nil}
   else L=nil nil
   end
end

fun {NrevFun L}
   case L of
      H|R then {AppFun {NrevFun R} H|nil}
   else L=nil nil
   end
end

local
   fun {MyApp Xs Ys}
      case Xs
      of X|Xr then
	 Zs = {MyApp Xr Ys}
      in
	 X|Zs
      [] nil  then Ys
      end
   end
in
   fun {MyRev L}
      case L
      of X|Xr then {MyApp {MyRev Xr} [X]}
      [] nil  then nil
      end
   end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


fun {QuickAux L Cont}
   case L of
      nil then Cont
   [] H|R then {Partition R H nil nil Cont}
   end
end
   
fun {Partition Xs A Less Greater Cont}
   case Xs of
      nil then {QuickAux Less A|{QuickAux Greater Cont}}
   [] H|R then case H<A then {Partition R A H|Less Greater Cont}
	       else {Partition R A Less H|Greater Cont}
	       end
   end
end

fun {Quick L} {QuickAux L nil} end


local
   fun {QuickAux L Cont Cmp}
      case L of nil then Cont
      [] H|R then {Partition R H nil nil Cont Cmp}
      end
   end
   
   fun {Partition Xs A Less Greater Cont Cmp}
      case Xs of
	 nil then {QuickAux Less A|{QuickAux Greater Cont Cmp} Cmp}
      [] H|R then case {Cmp H A} then {Partition R A H|Less Greater Cont Cmp}
		  else {Partition R A Less H|Greater Cont Cmp}
		  end
      end
   end
in
fun {QuickHO L Cmp} {QuickAux L nil Cmp} end
end

local
   fun {QuickAux L Cont Cmp}
      case L of nil then Cont
      [] H|R then {Partition R H nil nil Cont Cmp}
      end
   end
   
   fun {Partition Xs A Less Greater Cont Cmp}
      case Xs of
	 nil then {QuickAux Less A|{QuickAux Greater Cont Cmp} Cmp}
      [] H|R then {Cmp H A} case {GetReturn} then {Partition R A H|Less Greater Cont Cmp}
		  else {Partition R A Less H|Greater Cont Cmp}
		  end
      end
   end
in
fun {QuickHOFun L Cmp} {QuickAux L nil Cmp} end
end


fun {QuickArray Ar}
   {QuickArray1 Ar {Array.low Ar} {Array.high Ar}}
   Ar
end

proc {QuickArray1 Ar From To}
   case From < To then 
      Mid = {PartitionArray Ar From To} in
      {QuickArray1 Ar From Mid-1}
      {QuickArray1 Ar Mid+1 To}
   else skip
   end
end


fun {PartitionArray Ar From To}
   Pivot = {Array.get Ar From}
   Mid = {PartitionArray1 Ar Pivot From+1 From+1 To}
in
   {Array.put Ar From {Array.get Ar Mid}}
   {Array.put Ar Mid Pivot}
   Mid
end

fun {PartitionArray1 Ar Pivot PIndex From To}
   case From =< To then 
      Old = {Array.get Ar From} in
      case Pivot > Old then 
	 {Array.put Ar From {Array.get Ar PIndex}}
	 {Array.put Ar PIndex Old}
	 {PartitionArray1 Ar Pivot PIndex+1 From+1 To}
      else 
	 {PartitionArray1 Ar Pivot PIndex From+1 To}
      end
   else PIndex-1
   end
end

fun {QuickThread Ar}
   {QuickThread1 Ar {Array.low Ar} {Array.high Ar}}
   Ar
end

C = {NewCell 0}
proc {QuickThread1 Ar From To}
   case From < To then 
      Mid = {PartitionArray Ar From To}
      Sync
   in
      thread {Assign C {Access C}+1} {QuickThread1 Ar From Mid-1} Sync=unit end
      {QuickThread1 Ar Mid+1 To}
      {Wait Sync}
   else skip
   end
end



fun {ListToArray L}
   A = {Array.new 1 {Length L} 0}
in
   {ListToArray1 L A 1}
   A
end

proc {ListToArray1 L Ar N}
   case L of nil then skip
   [] H|R then {Array.put Ar N H} {ListToArray1 R Ar N+1}
   end
end

fun {ArrayToList1 A Low High}
   case High < Low then nil
   else
      {Array.get A Low}|{ArrayToList1 A Low+1 High}
   end
end

fun {ArrayToList A}
   {ArrayToList1 A {Array.low A} {Array.high A}}   
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fun {DoFoldL In Z To C}
   case In
   of X|Xr then {DoFoldL Xr {LFT Z C X 1 To} To C}
   [] nil  then Z
   end
end 

fun {LoopForThread C To In}
   case C=<To then {LoopForThread C+1 To {DoFoldL In nil To C}}
   else In
   end
end

fun {Queens N}
   {LoopForThread 1 N [nil]}
end

fun {NoAttak Xs C Y}
   {NoAttak1 Xs C Y 1}
end

fun {NoAttak1 Xs C Y I}
   case Xs of
      nil then true
   [] X|Xr then
      Y\=X andthen {Abs X-Y}\=C-I andthen {NoAttak1 Xr C Y I+1}
   end
end

fun {LFT Ss C Xs Y N}
   case Y>N then Ss
   else case {NoAttak Xs C Y}
        then {LFT {App Xs [Y]}|Ss C Xs Y+1 N}
	     else {LFT Ss C Xs Y+1 N}
	end
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


X_base = ~2.0
Y_base = 1.25
Side = 2.5

Sz = 800
MaxCount = 1024

Delta = Side / {Int.toFloat Sz}


fun {MandelLoop3 Count Z_re Z_im C_re C_im}
   case Count < MaxCount
   then
      Z_re_sq = Z_re * Z_re
      Z_im_sq = Z_im * Z_im
   in
      case (Z_re_sq + Z_im_sq) > 4.0 then Count
      else		     
	 Z_re_im = Z_re * Z_im
      in
	 {MandelLoop3
	  Count+1
	  (Z_re_sq - Z_im_sq) + C_re
	  Z_re_im + Z_re_im + C_im
	  C_re C_im}
      end
   else Count
   end
end

fun {MandelLoop2 J C_im SumIter}
   case J >= Sz then SumIter
   else
      C_re = X_base * (Delta + {Int.toFloat J})
      Count = {MandelLoop3 0 C_re C_im C_re C_im}
   in
      {MandelLoop2 J+1 C_im SumIter+Count}
   end
end

fun {MandelLoop1 I SumIter}
   case I >= Sz then SumIter
   else
      C_im = Y_base - Delta * {Int.toFloat I}
   in
      {MandelLoop1 I+1 {MandelLoop2 0 C_im SumIter}}
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fun {Deriv Exp X}
   case Exp of
      var(U)     then  case U==X then const(1) else const(0) end
   [] const(U)   then  const(0)
   [] plus(U V)  then  plus({Deriv U X} {Deriv V X})
   [] minus(U V) then  minus({Deriv U X} {Deriv V X})
   [] times(U V) then  plus(times({Deriv U X} V) times(U {Deriv V X}))
   [] 'div'(U V) then  'div'(minus(times({Deriv U X} V)
				   times(U {Deriv V X}))
			     exp(V 2))
   [] exp(U N)   then  times(times({Deriv U X} const(N)) exp(U N-1))
   [] uminus(U)  then  uminus({Deriv U X})
   [] log(U)     then  'div'({Deriv U X} U)
   end
end

fun {NthDeriv Exp X N}
   case N==0 then Exp
   else {NthDeriv {Deriv Exp X} X N-1}
   end
end

proc {GoDeriv N}
   {Dotimes N proc {$} {NthDeriv exp('div'(const(1) var(x)) 3) x 6 _} end}
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fun {ThreadCrea N}
   case N==0 then unit
   else  X = thread unit end
   in
      {Wait X}
      {ThreadCrea N-1}
   end
end

proc {Reader N L}
   case N==0 then L=nil
   else H R in
      L = H|R
      {Wait H}
      {Reader N-1 R}
   end
end

proc {NumGen N L}
   case L of nil then skip
   [] H|R then
      H = N
      {NumGen N+1 R}
   end
end

proc {ThreadComm N}
   L Done in
   thread {Reader N L} Done=unit end
   {NumGen 0 L}
   {Wait Done}
   {System.show done}
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


proc {Doit Bench Iter N}
   Fun Res 
in
   case Bench of
      tak    then Fun = proc{$} {Tak 3*N 2*N N _} end
   [] cpstak then Fun = proc{$} {Cpstak 3*N 2*N N _} end
   [] takfun then Fun = proc{$} {TakFun 3*N 2*N N _} end
   [] fib    then Fun = proc{$} {Fib N _} end
   [] concfib then Fun = proc {$} {Wait {ConcFib N}} end
   [] fibfun then Fun = proc{$} {FibFun N _} end
   [] fibf   then Fun = proc{$} {FibF N _} end
   [] fibffun then Fun = proc{$} {FibFFun N _} end
   [] fibthread then Fun = proc{$} {FibThread N _} end
   [] nrev   then Fun = proc{$} {Nrev {Genlist N} _} end
   [] nrevfun then L = {Genlist N} in Fun = proc{$} {MyRev L _} end
   [] quick then L = {Randlist 5000} in
      Fun = proc{$} {Dotimes N proc {$} {Quick L _} end} end
   [] quickho then L = {Randlist 5000} in
      Fun = proc{$} {Dotimes N proc {$}{QuickHO L fun{$ X Y} X<Y end _} end} end
   [] quickhofun then L = {Randlist 5000} in
      Fun = proc{$} {Dotimes N proc {$}{QuickHOFun L proc{$ X Y} {Return X<Y} end _} end} end
   [] quickarray then L = {Randlist 5000} in
      Fun = proc{$} {Dotimes N proc{$} {QuickArray {ListToArray L} _} end} end
   [] quickthread then L = {Randlist 5000} in
      Fun = proc{$} {Dotimes N proc{$} {QuickThread {ListToArray L} _} end} end
   [] queens then Fun = proc{$} {Queens N _} end
   [] deriv  then Fun = proc{$} {GoDeriv N} end
   [] tailrec then Fun = proc{$} {TailRec N _} end
   [] mandel then Fun = proc{$} {MandelLoop1 0 0 _} end
   [] threadcrea then Fun = proc{$} {ThreadCrea N _} end
   [] threadcomm then Fun = proc{$} {ThreadComm N} end
   end
   {Fun}
   Res = {Dobenchavrg Iter Fun}
   {System.show Bench(N Res.1 Res.2)}
end

{Property.put print foo(depth:10 width:100)}

proc {DoitAll Iter}
   {Doit fibfun     Iter 31}
   {Doit fib        Iter 31}
   {Doit fibffun    Iter 31.0}
   {Doit fibf       Iter 31.0}
   {Doit takfun     Iter 8}
   {Doit tak        Iter 8}
   {Doit cpstak     Iter 8}
   {Doit nrevfun    Iter 3000}
   {Doit nrev       Iter 3000}
   {Doit quick      Iter 30}
   {Doit quickho    Iter 30}
   {Doit quickarray Iter 30}
   {Doit queens     Iter 10}
   {Doit mandel     Iter 4711}
end

%% {DoIt Bench N ARg} --> Run Bench with Arg N times
proc {Benches Iter}
   {Doit fib        Iter 31}
%   {Doit concfib    Iter 20}
%   {Doit threadcrea Iter 100000}
   {Doit tak        Iter 8}
%   {Doit cpstak     Iter 8}
   {Doit nrevfun     Iter 3000}
   {Doit quick      Iter 30}
%   {Doit quickho    Iter 30}
%   {Doit quickarray Iter 30}
%   {Doit queens     Iter 10}
   {Doit deriv      Iter 30}
%   {Doit tailrec    Iter 1000000}
end

{Benches 10}
{Application.exit 0}
end
