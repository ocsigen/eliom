(** Values sent from the server to the client, compiled on both sides. *)

type case =
  | Case : {name : string; value : 'a; show : 'a -> string} -> case
  (** The server sends [value]; each side prints it with [show]. On the
          client, [value] is not used: it may not be representable there. *)

val cases : case list

(** Floats marshalled as one object: a boxed float or a non-empty float array.
    An empty float array is marshalled as an atom. *)
type float_object = Boxed of float | Float_array of float array

val big_endian : (float_object * case) list
(** Cases whose value holds these floats, which [gen.ml] sends as a big-endian
    server does. *)
