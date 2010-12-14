open Deriving_Json

module Map : sig

  module type OrderedType = sig
    include Map.OrderedType
    module Json_t : sig
      val make: (module Json with type a = t)
    end
  end

  module type S = sig
    include Map.S
    module Json_key : sig
      val make: (module Json with type a = key)
    end
    module Json_t : sig
      val make: (module Json with type a = 'a) -> (module Json with type a = 'a t)
    end
  end

  module MakeJson(O : OrderedType) : sig
    val make:
	(module Json with type a = 'a) ->
	  (module Json with type a = 'a Map.Make(O).t)
  end

  module Make(O : OrderedType) : S with type key = O.t

end

