(* $Id: document.mli,v 1.1 2004/01/25 23:45:03 ohl Exp $ *)

module type T =
  sig
    module X : XHTML.T
    type t
    val empty : string -> t
    val append : string ->
      [ X.heading | X.block | X.LIST.list ] X.elt list -> t -> t
    val add_style_internal : ?title:string -> string list -> t -> t
    val add_style_external : X.uri -> t -> t
    val to_file : ?multi:string -> string -> t -> unit
    val to_files : ?single:string -> string -> t -> unit
  end

module Make (X : XHTML.T) : T with module X = X

