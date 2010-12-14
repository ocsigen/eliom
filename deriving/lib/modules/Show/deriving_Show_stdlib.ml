(*pp $DERIVING *)

open Deriving_Show

module Map = struct

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

  module MakeShow(O : OrderedType) = struct

	let make (type tm_a) (m_a : (module Show with type a = tm_a)) =
	  (module Defaults(struct
	    module Map = Map.Make(O)
	    type a = tm_a Map.t
	    let format formatter map =
	      Format.pp_open_box formatter 0;
	      Format.pp_print_string formatter "{";
	      Map.iter (fun key value ->
                Format.pp_open_box formatter 0;
                (let module M = (val O.Show_t.make : Show with type a = O.t) in
		 M.format) formatter key;
                Format.pp_print_string formatter " => ";
                (let module M = (val m_a : Show with type a = tm_a) in
		 M.format) formatter value;
                Format.pp_close_box formatter ();
                Format.pp_print_string formatter ";";
                Format.pp_print_space formatter ();
             ) map;
	     Format.pp_print_string formatter "}";
	     Format.pp_close_box formatter ();
	  end) : Show with type a = tm_a Map.Make(O).t)

      end

  module Make(O : OrderedType) : S with type key = O.t = struct
    module M = Map.Make(O)
    include M
    module Show_key = O.Show_t
    module Show_t = MakeShow(O)
  end

end


(*

module Show_set
  (O : Set.OrderedType)
  (K : Show_min with type a = O.t)
  : Show with type a = Set.Make(O).t =
Defaults(
  struct
    module S = Set.Make(O)
    type a = S.t
    let format formatter set =
      Format.pp_open_box formatter 0;
      Format.pp_print_string formatter "{";
      S.iter (fun elt ->
                Format.pp_open_box formatter 0;
                K.format formatter elt;
                Format.pp_close_box formatter ();
             ) set;
      Format.pp_print_string formatter "}";
      Format.pp_close_box formatter ();
  end) *)
