functor $
import
   Property(get put)
export
   'UnsafeTimer$' : Interface
define
   {Property.put time foo(detailed:true)}
   Interface = 'UnsafeTimer'(time : fun {$ _}
				       {Property.get time}.total
				    end)
end
