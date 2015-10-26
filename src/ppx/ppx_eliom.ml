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

let id_of_string str =
  Printf.sprintf "%019d" (Hashtbl.hash str)

let file_loc () =
  Location.in_file !Location.input_name

let eid {Location. txt ; loc } =
  Exp.ident ~loc { loc ; txt = Longident.Lident txt }


(** Identifiers generation. *)
module Name = struct

  let escaped_ident_fmt : _ format6 =
    "_eliom_escaped_ident_%Ld"

  let fragment_ident_fmt : _ format6 =
    "_eliom_fragment_%Ld"

  let injected_ident_fmt : _ format6 =
    "_eliom_injected_ident_%019d_%Ld"

  (* Identifiers for the closure representing a fragment. *)
  let fragment_num_base _loc =
    Int64.of_int (Hashtbl.hash !Location.input_name)
  let fragment_num_count = ref Int64.zero
  let fragment_num _loc =
    fragment_num_count := Int64.succ !fragment_num_count;
    Int64.add (fragment_num_base _loc) !fragment_num_count
  let fragment_ident id =
    Printf.sprintf fragment_ident_fmt id

  (* Globaly unique ident for escaped expression *)
  (* It's used for type inference and as argument name for the
     closure representing the surrounding fragment. *)
  (* Inside a fragment, same ident share the global ident. *)
  let escaped_idents = ref []
  let reset_escaped_ident () = escaped_idents := []
  let escaped_expr, escaped_ident =
    let r = ref 0L in
    let make () =
      r := Int64.(add one) !r ;
      Printf.sprintf escaped_ident_fmt !r
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
    let r = ref Int64.zero in
    let make () =
      r := Int64.(add one) !r ;
      Printf.sprintf escaped_ident_fmt !r
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
    let r = ref Int64.zero in
    let gen_ident loc =
      let hash = Hashtbl.hash !Location.input_name in
      r := Int64.(add one) !r ;
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

module Mli = struct

  let type_file = ref ""
  let get_type_file () = match !type_file with
    | "" -> Filename.chop_extension !Location.input_name ^ ".type_mli"
    | f -> f

  let suppress_underscore =
    let c = ref 0 in
    let uid () = incr c ; !c in
    let pfix = Printf.sprintf "__eliom_inferred_type_%d" (uid ()) in
    let typ mapper ty = match ty.ptyp_desc with
      (* | Ptyp_constr  (_, Ast.TyAny _, ty) *)
      (* | Ptyp_constr (_, ty, Ast.TyAny _) -> ty *)
      | Ptyp_var var when var.[0] = '_' ->
        mapper.AM.typ mapper
          {ty with
           ptyp_desc = Ptyp_var (String.sub var 1 (String.length var - 1) ^ pfix)
          }
      | _ -> mapper.AM.typ mapper ty in
    let m = { AM.default_mapper with typ } in
    m.AM.typ m

  let is_injected_ident id =
    try Scanf.sscanf id Name.injected_ident_fmt (fun _ _ -> true)
    with Scanf.Scan_failure _ -> false

  let is_escaped_ident id =
    try Scanf.sscanf id Name.escaped_ident_fmt (fun _ -> true)
    with Scanf.Scan_failure _ -> false

  let is_fragment_ident id =
    try Scanf.sscanf id Name.fragment_ident_fmt (fun _ -> true)
    with Scanf.Scan_failure _ -> false

  let get_fragment_type = function
    | [%type: [%t? typ] Eliom_lib.client_value ]
    | [%type: [%t? typ] Eliom_pervasives.client_value ] ->
      Some typ
    | _ -> None

  let get_binding sig_item = match sig_item.psig_desc with
    | Psig_value {
      pval_name = {txt} ;
      pval_type = [%type: [%t? typ] option ref ] } ->
      if is_injected_ident txt || is_escaped_ident txt then
        Some (txt, suppress_underscore typ)
      else if is_fragment_ident txt then
        match get_fragment_type typ with
        | Some typ -> Some (txt, typ)
        | None -> None
      else
        None
    | _ -> None

  let load_file file =
    try
      let items =
        Pparse.parse_interface ~tool_name:"eliom" Format.err_formatter file
      in
      let h = Hashtbl.create 17 in
      let f item = match get_binding item with
        | Some (s, typ) -> Hashtbl.add h s typ
        | None -> ()
      in
      List.iter f items ;
      h
    with
    | Sys_error s ->
      Location.raise_errorf
        ~loc:(Location.in_file file)
        "Eliom: Error while loading types: %s" s

  let infered_sig = lazy (load_file (get_type_file ()))

  let find err {Location. txt ; loc } =
    try Hashtbl.find (Lazy.force infered_sig) txt with
    | Not_found ->
      Location.raise_errorf ~loc
        "Error: Infered type of %s not found. You need to regenerate %s."
        err (get_type_file ())

  let find_escaped_ident = find "escaped ident"
  let find_injected_ident = find "injected ident"
  let find_fragment = find "client value"

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
    num:Int64.t -> id:string Location.loc ->
    expression -> expression

  (** How to handle escaped "~%ident" inside a fragment. *)
  val escape_inject:
    ?ident:string -> context:Context.escape_inject ->
    id:string Location.loc ->
    expression -> expression

  val prelude : loc -> structure
  val postlude : loc -> structure

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
    [%expr
      Eliom_lib.create_shared_value
        [%e server_expr]
        [%client [%e client_expr]]
    ] [@metaloc loc]
end


module Register (Pass : Pass) = struct

  let eliom_expr (context : Context.t ref) mapper expr =
    let loc = expr.pexp_loc in
    let attr = expr.pexp_attributes in
    match expr, !context with
    | ([%expr [%client [%e? _ ]]] | [%expr [%shared [%e? _ ]]])
    , `Client ->
      let side = get_extension expr in
      Location.raise_errorf ~loc
        "The syntax [%%%s ...] is not allowed inside client code."
        side
    | ([%expr [%client [%e? _ ]]] | [%expr [%shared [%e? _ ]]])
    , (`Fragment _ | `Escaped_value _ | `Injection _) ->
      let side = get_extension expr in
      Location.raise_errorf ~loc
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
      let num = Name.fragment_num side_val.pexp_loc in
      let id = Location.mkloc (Name.fragment_ident num) side_val.pexp_loc in
      in_context context (`Fragment c)
        (Pass.fragment ?typ ~context:c ~num ~id % mapper.AM.expr mapper)
        side_val

    | [%expr ~% [%e? inj ]], _ ->
      let ident = match inj.pexp_desc with
        | Pexp_ident i -> Some (Longident.last i.txt)
        | _ -> None
      in
      begin match !context with
        | `Client | `Shared as c ->
          let id = Name.injected_ident loc inj in
          let new_context = `Injection c in
          in_context context new_context
            (Pass.escape_inject ?ident ~context:new_context ~id %
             mapper.AM.expr mapper)
            inj
        | `Fragment c ->
          let id = match c with
            | `Shared -> Name.nested_escaped_expr loc
            | `Server -> Name.escaped_expr loc
          in
          let new_context = `Escaped_value c in
          in_context context new_context
            (Pass.escape_inject ?ident ~context:new_context ~id %
             mapper.AM.expr mapper)
            inj
        | `Server ->
          Location.raise_errorf ~loc
            "The syntax ~%% ... is not allowed inside server code."
        | `Escaped_value _ | `Injection _ ->
          Location.raise_errorf ~loc
            "The syntax ~%% ... can not be nested."
      end
    | _ -> mapper.AM.expr mapper expr

  let structure_item mapper str =
    let loc = str.pstr_loc in
    match str.pstr_desc with
    | Pstr_extension (({txt=("server"|"shared"|"client")}, _), _) ->
      Location.raise_errorf ~loc
        "Sections are only allowed at toplevel."
    | _ -> mapper.AM.structure_item mapper str

  let signature_item mapper sig_ =
    let loc = sig_.psig_loc in
    match sig_.psig_desc with
    | Psig_extension (({txt=("server"|"shared"|"client")}, _), _) ->
      Location.raise_errorf ~loc
        "Sections are only allowed at toplevel."
    | _ -> mapper.AM.signature_item mapper sig_

  let eliom_mapper context =
    let context = ref (context :> Context.t) in
    { Ast_mapper.default_mapper
      with
        Ast_mapper.

        expr = eliom_expr context ;

        (* Reject sections not at toplevel. *)
        structure_item ;
        signature_item ;
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

  let dispatch_str c _mapper =
    dispatch Pass.(server_str, shared_str, client_str)
      (fun x -> x.AM.structure_item) c

  let dispatch_sig c _mapper =
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
    let loc = file_loc () in
    Pass.prelude loc @ flatmap f structs @ Pass.postlude loc

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

  let mapper _args =
    let c = ref `Server in
    {AM.default_mapper
     with
      structure = toplevel_structure c ;
      signature = toplevel_signature c ;
    }

end
