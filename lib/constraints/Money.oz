declare
proc {MoneyOz Root}
   S E N D M O R Y
in
   Root = sol(s:S e:E n:N d:D m:M o:O r:R y:Y)
   {FD.dom 0#9 Root}
   {FD.distinct Root}
   M \=: 0
   S \=: 0
   1000 * S + 100 * E + 10 * N + D + 1000 * M + 100 * O + 10 * R + E =: 10000 * M + 1000 * O + 100 * N + 10 * E + Y
   {FD.distribute ff Root}
end

declare
proc {MoneyAlice Root}
   Vars = {MakeList 8}
in
   {FD.dom 0#9 Vars}
   case Vars
   of [S E N D M O R Y] then
      Send  = {FD.decl}
      More  = {FD.decl}
      Money = {FD.decl}
   in
      {FD.distinct Vars}
      M \=: 0
      S \=: 0
      {FD.sumC [1000 100 10 1] [S E N D] '=:' Send}
      {FD.sumC [1000 100 10 1] [M O R E] '=:' More}
      {FD.sumC [10000 1000 100 10 1] [M O N E Y] '=:' Money}
      {FD.plus Send More Money}
      {FD.distribute ff Vars}
      Root = Vars
   end
end
{ExploreAll MoneyAlice}

