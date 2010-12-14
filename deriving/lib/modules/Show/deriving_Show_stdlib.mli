open Deriving_Show

module Map : sig

  module type OrderedType = sig
    include Map.OrderedType
    module Show_t : sig
      val make: (module Show with type a = t)
    end
  end

  module type S = sig
    include Map.S
    module Show_key : sig
      val make: (module Show with type a = key)
    end
    module Show_t : sig
      val make: (module Show with type a = 'a) -> (module Show with type a = 'a t)
    end
  end

  module MakeShow(O : OrderedType) : sig
    val make:
	(module Show with type a = 'a) ->
	  (module Show with type a = 'a Map.Make(O).t)
  end

  module Make(O : OrderedType) : S with type key = O.t

end

