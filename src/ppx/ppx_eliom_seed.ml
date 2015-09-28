open Parsetree
open Ast_helper

module AM = Ast_mapper

(** Various misc functions *)

let flatmap f l = List.flatten @@ List.map f l

let get_extension = function
  | {pexp_desc= Pexp_extension ({txt},_)} -> txt
  | _ -> invalid_arg "Eliom ppx: Should be an extension."

let in_context cref c f x =
  let old = !cref in
  cref := c ;
  let res = f x in
  cref := old ;
  res

let (%) f g x = f (g x)

let exp_add_attrs e attr =
  {e with pexp_attributes = attr}

module type Helpers = sig

  (** find infered type for escaped expr *)
  val find_fragment_type: Int64.t -> core_type

  (** find infered type for escaped expr *)
  val find_escaped_ident_type: string -> core_type

  (** find infered type for injected ident *)
  val find_injected_ident_type: string -> core_type

  val is_fragment_type : core_type -> core_type option

  val raise_syntax_error : _ Location.loc -> string -> _

  val is_escaped_indent_string: string -> bool
end


(** Identifiers generation. *)
module Name = struct

  let escaped_ident_prefix = "_eliom_escaped_ident_"

  let fragment_ident_prefix = "_eliom_fragment_"

  let injected_ident_fmt : (_,_,_,_) format4 =
    "_eliom_injected_ident_%019d_%d"

  let id_of_string str =
    Printf.sprintf "%019d" (Hashtbl.hash str)

  let escaped_ident_prefix_len = String.length escaped_ident_prefix
  let is_escaped_ident_string id =
    String.length id > escaped_ident_prefix_len &&
    String.sub id 0 escaped_ident_prefix_len = escaped_ident_prefix

  (* Identifiers for the closure representing a fragment. *)
  let fragment_num_base _loc =
    Int64.of_int (Hashtbl.hash !Location.input_name)
  let gen_closure_num_count = ref Int64.zero
  let fragment_num _loc =
    gen_closure_num_count := Int64.succ !gen_closure_num_count;
    Int64.add (fragment_num_base _loc) !gen_closure_num_count
  let fragment_ident id =
    fragment_ident_prefix ^ Int64.to_string id

  (* Globaly unique ident for escaped expression *)
  (* It's used for type inference and as argument name for the
     closure representing the surrounding fragment. *)
  (* Inside a fragment, same ident share the global ident. *)
  let escaped_idents = ref []
  let reset_escaped_ident () = escaped_idents := []
  let escaped_expr, escaped_ident =
    let r = ref 0 in
    let make () =
      incr r; escaped_ident_prefix ^ string_of_int !r
    in
    let for_expr loc = Location.mkloc (make ()) loc in
    let for_id {Location. txt = id ; loc } =
      let txt =
        try List.assoc id !escaped_idents
        with Not_found ->
          let gen_id = make () in
          escaped_idents := (id, gen_id) :: !escaped_idents;
          gen_id
      in {Location. txt ; loc }
    in for_expr, for_id

  (* Escaped expression inside shared sections. *)
  let nested_escaped_idents = ref []
  let reset_nested_escaped_ident () = nested_escaped_idents := []
  let nested_escaped_expr, nested_escaped_ident =
    let r = ref 0 in
    let make () =
      incr r; escaped_ident_prefix ^ string_of_int !r
    in
    let for_expr loc = Location.mkloc (make ()) loc in
    let for_id {Location. txt = id ; loc } =
      let txt =
        try List.assoc id !nested_escaped_idents
        with Not_found ->
          let gen_id = make () in
          nested_escaped_idents := (id, gen_id) :: !nested_escaped_idents;
          gen_id
      in {Location. txt ; loc }
    in for_expr, for_id

  let injected_expr, injected_ident =
    let injected_idents = ref [] in
    let r = ref 0 in
    let gen_ident loc =
      let hash = Hashtbl.hash !Location.input_name in
      incr r;
      let s = Printf.sprintf injected_ident_fmt hash !r in
      {Location. txt = s ; loc }
    in
    let gen_injected_ident loc expr =
      try List.assoc expr !injected_idents
      with Not_found ->
        let gen_id = gen_ident loc in
        injected_idents := (expr, gen_id) :: !injected_idents;
        gen_id
    in
    gen_ident, gen_injected_ident
end

(** Context convenience module. *)
module Context = struct

  type server = [ `Server | `Shared ]
  type client = [ `Client | `Shared ]

  let to_string = function
    | `Client -> "client"
    | `Shared -> "shared"
    | `Server -> "server"

  let of_string = function
    | "server" -> `Server
    | "shared" -> `Shared
    | "client" -> `Client
    | _ -> invalid_arg "Eliom ppx: Not a context"

  type escape_inject = [
    | `Escaped_value of server
    | `Injection of client
  ]

  type t = [
    | `Server (* [%%server ... ] *)
    | `Client (* [%%client ... ] *)
    | `Shared (* [%%shared  ... ] *)
    | `Fragment of server (* [%client ... ] *)
    | `Escaped_value of server (* [%shared ~%( ... ) ] *)
    | `Injection of client (* [%%client ~%( ... ) ] *)
  ]
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
    id:Int64.t ->
    expression -> expression

  (** How to handle escaped "~%ident" inside a fragment. *)
  val escape_inject:
    ?ident:string -> context:Context.escape_inject ->
    string Location.loc -> expression -> expression

  val implem :
    structure_item -> structure_item

end

(**
   Replace shared expression by the equivalent pair.

   [ [%share
       let x = ... %s ... in
       [%client ... %x ... ]
     ] ]
   ≡
   [ let x = ... s ... in
     [%client ... %x ... ]
     ,
     [%client
       let x = ... %s ... in
       ... x ...
     ]
   ]
*)
module Shared = struct

  let server_expr mapper expr =
    match expr with
    | [%expr [%client [%e? _ ]]] -> expr
    | [%expr ~% [%e? injection_expr ]] -> injection_expr
    | _ -> mapper.AM.expr mapper expr
  let server = {AM.default_mapper with expr = server_expr}

  let client_expr context mapper expr =
    match expr with
    | [%expr [%client [%e? fragment_expr ]]] ->
      in_context context `Fragment
        (mapper.AM.expr mapper) fragment_expr
    | [%expr ~% [%e? injection_expr ]] ->
      begin match !context with
        | `Top -> expr
        | `Fragment -> injection_expr
      end
    | _ -> mapper.AM.expr mapper expr
  let client = {AM.default_mapper with expr = client_expr (ref `Top)}

  let expr loc expr =
    let server_expr = server.AM.expr server expr in
    let client_expr = client.AM.expr client expr in
    [%expr ([%e server_expr], [%client [%e client_expr]])] [@metaloc loc]
end


module Register (Pass : Pass) = struct

  (** Environment of discovered injection/escaped values. *)
  let inj_map = Hashtbl.create 17

  let eliom_expr (context : Context.t ref) mapper expr =
    let loc = expr.pexp_loc in
    let attr = expr.pexp_attributes in
    match expr, !context with
    | ([%expr [%client [%e? _ ]]] | [%expr [%shared [%e? _ ]]])
    , `Client ->
      let side = get_extension expr in
      Exp.extension @@ AM.extension_of_error @@
      Location.errorf ~loc
        "The syntax [%%%s ...] is not allowed inside client code."
        side
    | ([%expr [%client [%e? _ ]]] | [%expr [%shared [%e? _ ]]])
    , (`Fragment _ | `Escaped_value _ | `Injection _) ->
      let side = get_extension expr in
      Exp.extension @@ AM.extension_of_error @@
      Location.errorf ~loc
        "The syntax [%%%s ...] can not be nested."
        side

    | [%expr [%shared [%e? side_val ]]], (`Server | `Shared) ->
      let e = Shared.expr loc side_val in
      mapper.AM.expr mapper @@ exp_add_attrs e attr

    | [%expr [%client [%e? side_val ]]], (`Server | `Shared as c) ->
      let (side_val, typ) = match side_val with
        | [%expr ([%e? cval]:[%t? typ]) ] -> (cval, Some typ)
        | _ -> (expr, None)
      in
      let id = Name.fragment_num side_val.pexp_loc in
      in_context context (`Fragment c)
        (Pass.fragment ?typ ~context:c ~id % mapper.AM.expr mapper)
        side_val

    | [%expr ~% [%e? inj ]], _ ->
      begin match !context with
        | `Client | `Shared as c ->
          let ident = Name.injected_ident loc inj in
          let new_context = `Injection c in
          in_context context new_context
            (Pass.escape_inject ~context:new_context ident %
             mapper.AM.expr mapper)
            inj
        | `Fragment c ->
          let ident = match c with
            | `Shared -> Name.nested_escaped_expr loc
            | `Server -> Name.escaped_expr loc
          in
          let new_context = `Escaped_value c in
          in_context context new_context
            (Pass.escape_inject ~context:new_context ident %
             mapper.AM.expr mapper)
            inj
        | `Server ->
          Exp.extension @@ AM.extension_of_error @@
          Location.errorf ~loc
            "The syntax ~%% ... is not allowed inside server code."
        | `Escaped_value _ | `Injection _ ->
          Exp.extension @@ AM.extension_of_error @@
          Location.errorf ~loc
            "The syntax ~%% ... can not be nested."
      end
    | _ -> mapper.AM.expr mapper expr

  let structure_item mapper str =
    let loc = str.pstr_loc in
    match str.pstr_desc with
    | Pstr_extension (({txt=("server"|"shared"|"client") as txt}, _), _) ->
      Str.extension @@ Ast_mapper.extension_of_error @@
      Location.errorf ~loc
        "The %%%%%s extension is only allowed at toplevel."
        txt
    | _ -> mapper.AM.structure_item mapper str

  let eliom_mapper context =
    let context = ref (context :> Context.t) in
    { Ast_mapper.default_mapper
      with
        Ast_mapper.

        expr = eliom_expr context ;

        (* Reject sections not at toplevel. *)
        structure_item = structure_item

    }


  (** Toplevel translation *)
  (** Switch the current context when encountering [%%server] (resp. shared, client)
      annotations. Call the eliom mapper and [Pass.server_str] (resp ..) on each
      structure item.
  *)

  let dispatch (server, shared, client) field context str =
    let f = match context with
      | `Server -> server | `Shared -> shared | `Client -> client
    in
    let m = eliom_mapper context in
    f @@ (field m) m str

  let dispatch_str c mapper =
    dispatch Pass.(server_str, shared_str, client_str)
      (fun x -> x.AM.structure_item) c

  let dispatch_sig c mapper =
    dispatch Pass.(server_sig, shared_sig, client_sig)
      (fun x -> x.AM.signature_item) c

  let toplevel_structure context mapper structs =
    let f pstr =
      let loc = pstr.pstr_loc in
      match pstr.pstr_desc with
      | Pstr_extension (({txt=("shared"|"client"|"server" as txt)}, PStr strs), _) ->
        if strs <> [] then
          [ Str.extension ~loc @@ AM.extension_of_error @@ Location.errorf ~loc
              "The %%%%%s extension doesn't accept arguments." txt ]
        else ( context := Context.of_string txt ; [] )
      | _ ->
        dispatch_str !context mapper pstr
    in
    flatmap f structs

  let toplevel_signature context mapper sigs =
    let f psig =
      let loc = psig.psig_loc in
      match psig.psig_desc with
      | Psig_extension (({txt=("shared"|"client"|"server" as txt)}, PStr strs), _) ->
        if strs <> [] then
          [ Sig.extension ~loc @@ AM.extension_of_error @@ Location.errorf ~loc
              "The %%%%%s extension doesn't accept arguments." txt ]
        else ( context := Context.of_string txt ; [] )
      | _ ->
        dispatch_sig !context mapper psig
    in
    flatmap f sigs

  let toplevel_mapper _args =
    let c = ref `Server in
    {AM.default_mapper
     with
      structure = toplevel_structure c ;
      signature = toplevel_signature c ;
    }

end
