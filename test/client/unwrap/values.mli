(** Values sent from the server to the client, compiled on both sides. *)

type case =
  | Case : {name : string; value : 'a; show : 'a -> string} -> case
  (** The server sends [value]; each side prints it with [show]. On the
          client, [value] is not used: it may not be representable there. *)

val cases : case list
