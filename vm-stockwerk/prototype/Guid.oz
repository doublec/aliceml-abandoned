functor
import
   OS(getPID srand rand)
   OsTime(time)
   Property(get)
export
   new: NewGuid
   hash: HashGuid
   '<': GuidLess
   vm: VMGuid
define
   {OS.srand 0}

   fun {NewGuid}
      tuple({OS.getPID} {OsTime.time} {Property.get 'time.total'} {OS.rand})
   end

   fun {HashGuid tuple(A B C D)}
      A + B + C + D
   end

   local
      fun {GuidLessSub Guid1 Guid2 N}
	 case N of 5 then false
	 else I = Guid1.N J = Guid2.N in
	    if I < J then true
	    elseif I == J then {GuidLessSub Guid1 Guid2 N + 1}
	    else false
	    end
	 end
      end
   in
      fun {GuidLess Guid1 Guid2}
	 {GuidLessSub Guid1 Guid2 1}
      end
   end

   VMGuid = {NewGuid}
end
