open Parsetree


type client_value_context = [ `Server | `Shared ]
type injection_context    = [ `Client | `Shared ]

type escape_inject =
  | Escaped_value of client_value_context
  | Injection of injection_context


val is_escaped_indent_string: string -> bool



(** Signature of specific code of a preprocessor. *)
module type Pass = sig

  (** How to handle "client", "shared" and "server" sections for top level structure items. *)

  val shared_str_item: structure_item -> structure_item list
  val server_str_item: structure_item -> structure_item list
  val client_str_item: structure_item -> structure_item list

  (** How to handle "client", "shared" and "server" sections for top level signature items. *)

  val shared_sig_item: signature_item -> signature_item list
  val client_sig_item: signature_item -> signature_item list
  val server_sig_item: signature_item -> signature_item list

  (** How to handle "[%cval ...]" expr. *)
  val client_value_expr:
    ?typ:core_type -> context:client_value_context ->
    id:Int64.t -> expression -> expression

  (** How to handle escaped "~%ident" inside "[%cval ... ]". *)
  val escape_inject:
    ?ident:string -> context:escape_inject -> expression -> string -> expression

  val implem : structure_item -> structure_item

end
