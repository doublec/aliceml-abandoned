functor
import
   System(show)
   Application(exit)
define
   fun {Fib N}
      case N of 0 then 1
      [] 1 then 1
      else {Fib2 N - 1} + {Fib2 N - 2}
      end
   end

   fun {Fib2 N}
      thread
	 {Fib N}
      end
   end

   fun {Loop N}
      case N of 0 then unit
      else
	 _ = {Fib 12}
	 {Loop N - 1}
      end
   end

   {System.show {Loop 50}}

   {Application.exit 0}
end
