open Parsetree

(** {2 Various helping functions} *)

(** Name of the variable which holds the hash of the file. *)
val id_file_hash : Location.t -> string Location.loc

val eid : string Location.loc -> expression

val position : Location.t -> expression

val format_args : expression list -> expression

val pat_args : pattern list -> pattern

(** These functions try to guess if a given expression will lead to a fragment evaluation
    This is not possible in general, this criteria is only syntactic

    If the expression cannot have fragments, we don't need to use sections.
    Consequently, this function should *never* return false positive.
*)
module Cannot_have_fragment : sig

  val expression : expression -> bool
  val structure_item : structure_item -> bool

end


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

module Mli : sig

  val is_escaped_ident : string -> bool

  val get_injected_ident_info : string -> (string * int)

  val exists : unit -> bool

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
    num:string -> id:string Location.loc ->
    expression -> expression

  (** How to handle escaped "~%ident" inside a fragment. *)
  val escape_inject:
    ?ident:string -> context:Context.escape_inject ->
    id:string Location.loc ->
    expression -> expression

  val prelude : Location.t -> structure
  val postlude : Location.t -> structure

end

module Make (P : Pass) : sig
  val mapper : string list -> Ast_mapper.mapper
end
