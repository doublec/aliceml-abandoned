functor
import
   System(show)
   Application(exit)
define
   fun {Append Xs Ys}
      case Xs of nil then Ys
      [] X|Xr then X|{Append Xr Ys}
      end
   end

   fun {Rev Xs}
      case Xs of nil then nil
      [] X|Xr then {Append {Rev Xr} [X]}
      end
   end

   fun {MakeList N X}
      case N of 0 then nil
      else X|{MakeList N - 1 X}
      end
   end

   _ = {Rev {MakeList 2000 0}}

   {System.show done}

   {Application.exit 0}
end
