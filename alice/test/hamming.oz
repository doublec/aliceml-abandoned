functor
import
   System(show)
   Application(exit)
define
   fun {Nth X|Xr I}
      case I of 0 then X
      else {Nth Xr I - 1}
      end
   end

   fun lazy {Map F X|Xr}
      {F X}|{Map F Xr}
   end

   fun lazy {Merge Xs=X|Xr Ys=Y|Yr}
      if X == Y then X|{Merge Xr Yr}
      elseif X < Y then X|{Merge Xr Ys}
      else Y|{Merge Xs Yr}
      end
   end

   fun {Loop I}
      case I of 0 then unit
      else Hamming in
	 Hamming = 1|{Merge {Map fun {$ I} 2 * I end Hamming}
		      {Merge {Map fun {$ I} 3 * I end Hamming}
		       {Map fun {$ I} 5 * I end Hamming}}}
	 _ = {Nth Hamming 1500}
	 {Loop I - 1}
      end
   end

   {System.show {Loop 100}}

   {Application.exit 0}
end
