functor
export
   Deref
   Construct
   Deconstruct
   PushCallInterpreter
   PushCall
define
   fun {Deref X}
      case X of transient(TransientState) then
	 case {Access TransientState} of ref(Y) then {Deref Y}
	 else X
	 end
      else X
      end
   end

   fun {Construct Args}
      case Args of arg(X) then X
      [] args(...) then {Adjoin Args tuple}
      end
   end

   fun {Deconstruct Args}
      case Args of arg(X) then {Deref X}
      [] args(...) then Args
      end
   end

   PushCallInterpreter =
   pushCallInterpreter(
      run:
	 fun {$ Args pushCall(_ Closure0)|Rest}
	    {PushCall Args Closure0 Rest}
	 end
      handle:
	 fun {$ Debug Exn Frame|Rest}
	    exception(Frame|Debug Exn Rest)
	 end
      toString: fun {$ _} 'PushCall' end)

   fun {PushCall Args Closure0 TaskStack}
      case {Deref Closure0} of Transient=transient(_) then
	 request(Transient Args
		 pushCall(PushCallInterpreter Closure0)|TaskStack)
      elseof Closure then
	 case {Deref Closure.1} of Transient=transient(_) then
	    request(Transient Args
		    pushCall(PushCallInterpreter Closure)|TaskStack)
	 elseof Function then
	    continue(Args {Function.1.pushCall Closure Function TaskStack})
	 end
      end
   end
end
