(*pp $DERIVING *)

open Deriving_Json

module Map = struct

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

  module Raw_MakeJson(O : OrderedType) = struct

        module Map = Deriving_stdlib.Map.Make(O)

	type key = O.t deriving (Json)
	type 'a t = 'a Map.t =
	  (* Copy/paste the concrete type definition from utils/deriving_stdlib.ml *)
	  | Empty
	  | Node of 'a t * key * 'a * 'a t * int
		deriving (Json)

      end

  module MakeJson(O : OrderedType) = struct

	let make (type tm_a) (m_a : (module Json with type a = tm_a)) =
	  let module M = Raw_MakeJson(O) in
	  (Obj.magic (M.Json_t.make m_a) :
	     (module Json with type a = tm_a Map.Make(O).t))

      end

  module Make(O : OrderedType) : S with type key = O.t = struct
    module M = Map.Make(O)
    include M
    module Json_key = O.Json_t
    module Json_t = MakeJson(O)
  end

end
