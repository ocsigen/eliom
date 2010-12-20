
module Map : sig
  module Make(O: Map.OrderedType) : sig
    type key = O.t
    type 'a t = (* Copy/paste the concrete type definition from stdlib/map.ml *)
      | Empty
      | Node of 'a t * key * 'a * 'a t * int
  end
end
