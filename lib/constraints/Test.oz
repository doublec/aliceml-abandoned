declare

Names = [alice bert chris deb evan]
Prefs = [alice#chris bert#evan chris#deb chris#evan
	 deb#alice deb#evan evan#bert]

proc {Skript Root}
   Pos = {FD.record pos Names 1#{Length Names}}
   Ful = {Map Prefs
	  fun {$ A#B}
	     (Pos.A + 1 =: Pos.B) + (Pos.A - 1 =: Pos.B) =: 1 
	  end}
   Sat = {FD.int 0#{Length Prefs}}
in
   {FD.distinct Pos}
   {FD.sum Ful '=:' Sat}
   {FD.distribute naive Pos}
   Root = sol(pos:Pos ful:Ful sat: Sat)
end

fun {DoSearchAll S}
   case {Space.ask S}
   of failed          then nil
   [] succeeded       then [S]
   [] alternatives(N) then
      C = {Space.clone S}
   in
      {Space.commit S 1}
      {Space.commit C 2#N}
      {Append {DoSearchAll S} {DoSearchAll C}}
   end
end

fun {SearchAll Skript}
   case {DoSearchAll {Space.new Skript}}
   of nil then nil
   [] Ss then
      {Map Ss fun {$ S}
		 {Space.merge S}
	      end}
   end
end

local
   fun {MakeConstrain O}
      proc {$ S BS}
	 OR = {Space.merge {Space.clone BS}}
      in
	 {Space.inject S proc {$ NR}
			    {O OR NR}
			 end}
      end
   end
in
   fun {SearchBestFun P O}
      S         = {Space.new P}
      Constrain = {MakeConstrain O}
      fun {SearchBest S BS}
	 case {Space.ask S}
	 of failed          then BS
	 [] succeeded       then S
	 [] alternatives(N) then
	    C = {Space.clone S}
	 in
	    {Space.commit S 1}
	    {Space.commit C 2#N}
	    case {SearchBest S BS}
	    of NBS then
	       if BS \= NBS then {Constrain C NBS} end
	       {SearchBest C NBS}
	    end
	 end
      end
   in
      case {SearchBest S S}
      of nil then 'NONE'
      [] S   then 'SOME'({Space.merge S})
      end
   end
end
proc {Order A B}
   A.sat =<: B.sat
end

{Inspect {SearchBestFun Skript Order}}
%%{ExploreAll Skript}
{Inspect ExploreBest}

declare X Y Z
{FD.dom 1#10 [X Y Z]}
{Inspect t(X Y Z)}
{FD.sum [X Y] '=:' Z}

declare
proc {Money Root}
   S E N D M O R Y
   Vars
in
   Vars = sol(s:S e:E n:N d:D m:M o:O r:R y:Y)
   {FD.dom 0#9 Vars}
   {FD.distinct Vars}
   M \=: 0
   S \=: 0
   1000 * S + 100 * E + 10 * N + D + 1000 * M + 100 * O + 10 * R + E =: 10000 * M + 1000 * O + 100 * N + 10 * E + Y
   {FD.distribute ff Vars}
   Root = Vars
end
{ExploreAll Money}
