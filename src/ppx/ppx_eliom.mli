open Parsetree

(** Context convenience module. *)
module Context : sig

  type server = [ `Server | `Shared ]
  type client = [ `Client | `Shared ]

  type escape_inject = [
    | `Escaped_value of server
    | `Injection of client
  ]

  type t = [
    | `Server (* [%%server ... ] *)
    | `Client (* [%%client ... ] *)
    | `Shared (* [%%shared  ... ] *)
    | `Fragment of server (* [%client ... ] *)
    | `Escaped_value of server (* [%%server [%client ~%( ... ) ] ] *)
    | `Injection of client (* [%%client ~%( ... ) ] *)
  ]
end


val id_of_string : _ -> string

module Mli : sig

  val is_escaped_ident : string -> bool

  val find_escaped_ident : string Location.loc -> core_type
  val find_injected_ident : string Location.loc -> core_type
  val find_fragment : string Location.loc -> core_type

end

(** Signature of specific code of a preprocessor. *)
module type Pass = sig

  (** How to handle "client", "shared" and "server" sections for top level structure items. *)

  val shared_str: structure_item -> structure_item list
  val server_str: structure_item -> structure_item list
  val client_str: structure_item -> structure_item list

  (** How to handle "client", "shared" and "server" sections for top level signature items. *)

  val shared_sig: signature_item -> signature_item list
  val client_sig: signature_item -> signature_item list
  val server_sig: signature_item -> signature_item list

  (** How to handle "[%client ...]" and "[%shared ...]" expr. *)
  val fragment:
    ?typ:core_type -> context:Context.server ->
    num:Int64.t -> id:string Location.loc ->
    expression -> expression

  (** How to handle escaped "~%ident" inside a fragment. *)
  val escape_inject:
    ?ident:string -> context:Context.escape_inject ->
    id:string Location.loc ->
    expression -> expression

  val prelude : Location.t -> structure
  val postlude : Location.t -> structure

end

module Register (P : Pass) : sig end
