open Ppxlib
open Parsetree

(** {2 Various helping functions} *)

val sequence :
  ?loc:Location.t ->
  ?attrs:Parsetree.attribute list ->
  Parsetree.expression list ->
  Parsetree.expression

val str :
  ?loc:Location.t ->
  ?attrs:Parsetree.attribute list  ->
  string ->
  Parsetree.expression
val int :
  ?loc:Location.t ->
  ?attrs:Parsetree.attribute list  ->
  int ->
  Parsetree.expression

(** Name of the variable which holds the hash of the file. *)
val id_file_hash : Location.t -> string Location.loc

val eid : string Location.loc -> expression

val position : Location.t -> expression

val format_args : expression list -> expression

val pat_args : pattern list -> pattern

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
    | `Fragment of server * bool (* [%client ... ] *)
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

module Cmo : sig

  val exists : unit -> bool

  val find_escaped_ident : Location.t -> core_type
  val find_injected_ident : Location.t -> core_type
  val find_fragment : Location.t -> core_type

end

(** Signature of specific code of a preprocessor. *)
module type Pass = sig

  (** How to handle "client", "shared" and "server" sections for top level structure items.

      For shared and server, the boolean argument indicate if this
      declaration can lead to evaluation of a fragment.
  *)

  val shared_str: bool -> structure_item -> structure_item list
  val server_str: bool -> structure_item -> structure_item list
  val client_str: structure_item -> structure_item list

  (** How to handle "client", "shared" and "server" sections for top level signature items. *)

  val shared_sig: signature_item -> signature_item list
  val client_sig: signature_item -> signature_item list
  val server_sig: signature_item -> signature_item list

  (** How to handle "[%client ...]" and "[%shared ...]" expr. *)
  val fragment:
    loc:Location.t -> ?typ:core_type -> context:Context.server ->
    num:string -> id:string Location.loc -> unsafe:bool ->
    expression -> expression

  (** How to handle escaped "~%ident" inside a fragment. *)
  val escape_inject:
    loc:Location.t -> ?ident:string -> context:Context.escape_inject ->
    id:string Location.loc -> unsafe:bool ->
    expression -> expression

  val prelude : Location.t -> structure
  val postlude : Location.t -> structure

end

val driver_args : (Arg.key * Arg.spec * Arg.doc) list

module Make (_ : Pass) : sig
  val mapper : Ppxlib.Ast_traverse.map
end
