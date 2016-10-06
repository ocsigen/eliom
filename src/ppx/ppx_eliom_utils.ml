open Parsetree
open Ast_helper

module AM = Ast_mapper
module AC = Ast_convenience

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

let exp_add_attrs attr e =
  {e with pexp_attributes = attr}

let eid {Location. txt ; loc } =
  Exp.ident ~loc { loc ; txt = Longident.Lident txt }

let format_args = function
  | [] -> AC.unit ()
  | [e] -> e
  | l -> Exp.tuple l

let pat_args = function
  | [] -> AC.punit ()
  | [p] -> p
  | l -> Pat.tuple l

(* We use a strong hash (MD5) of the file name.
   We only keep the first 36 bit, which should be well enough: with
   256 files, the likelihood of a collision is about one in two
   millions.
   These bits are encoded using an OCaml-compatible variant of Base
   64, as the hash is used to generate OCaml identifiers. *)
let file_hash loc =
  let s = Digest.string loc.Location.loc_start.pos_fname in
  let e = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_'" in
  let o = Bytes.create 6 in
  let g p = Char.code s.[p] in
  for i = 0 to 5 do
    let p = i * 6 / 8 in
    let d = 10 - (i * 6) mod 8 in
    Bytes.set o i e.[(g p lsl 8 + g (p + 1)) lsr d land 63]
  done;
  Bytes.to_string o

let id_file_hash loc =
  let prefix = "__eliom__compilation_unit_id__" in
  {Location. loc ; txt = prefix ^ file_hash loc}

(** [let __eliom__compilation_unit_id__HASH = "HASH"]
    We hoist the file hash at the beginning of each eliom file.
    This makes the generated javascript code smaller.
*)
let module_hash_declaration loc =
  let id = Pat.var ~loc @@ id_file_hash loc in
  Str.value ~loc Nonrecursive [Vb.mk ~loc id @@ AC.str @@ file_hash loc]

(** The first position in a file, if it exists.
    We avoid {!Location.input_name}, as it's unreliable when reading multiple files.
*)
let file_position str = match str with
  | { pstr_loc } :: _ -> Location.in_file @@ pstr_loc.loc_start.pos_fname
  | [] -> Location.none

let lexing_position ~loc l =
  [%expr
    { Lexing.pos_fname = [%e AC.str l.Lexing.pos_fname];
      Lexing.pos_lnum = [%e AC.int @@ l.Lexing.pos_lnum];
      Lexing.pos_bol = [%e AC.int @@ l.Lexing.pos_bol];
      Lexing.pos_cnum = [%e AC.int @@ l.Lexing.pos_cnum]; }
  ] [@metaloc loc]

let position loc =
  let start = loc.Location.loc_start in
  let stop = loc.Location.loc_start in
  Exp.tuple ~loc [ lexing_position ~loc start ; lexing_position ~loc stop ]

let is_annotation txt l =
  List.exists (fun s -> txt = s || txt = "eliom."^s) l

(** Identifiers generation. *)
module Name = struct

  let escaped_ident_fmt : _ format6 =
    "_eliom_escaped_ident_%Ld"

  let fragment_ident_fmt : _ format6 =
    "_eliom_fragment_%s"

  let injected_ident_fmt : _ format6 =
    "_eliom_injected_ident_%6s%d"

  (* Identifiers for the closure representing a fragment. *)
  let fragment_num_count = ref 0
  let fragment_num _loc =
    incr fragment_num_count;
    Printf.sprintf "%s%d" (file_hash _loc) !fragment_num_count
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
    let for_id loc id =
      let txt =
        try List.assoc id !escaped_idents
        with Not_found ->
          let gen_id = make () in
          escaped_idents := (id, gen_id) :: !escaped_idents;
          gen_id
      in {Location. txt ; loc }
    in for_expr, for_id

  let injected_expr, injected_ident, reset_injected_ident =
    let injected_idents = ref [] in
    let r = ref 0 in
    let gen_ident loc =
      let hash = file_hash loc in
      incr r;
      let s = Printf.sprintf injected_ident_fmt hash !r in
      {Location. txt = s ; loc }
    in
    let gen_injected_ident loc (s:string) =
      try List.assoc s !injected_idents
      with Not_found ->
        let gen_id = gen_ident loc in
        injected_idents := (s, gen_id) :: !injected_idents;
        gen_id
    and reset () = injected_idents := [] in
    gen_ident, gen_injected_ident, reset

end

(* WARNING: if you change this, also change inferred_type_prefix in
   tools/eliomc.ml *)
let inferred_type_prefix = "eliom_inferred_type_"

module Mli = struct

  let type_file = ref None
  let get_type_file () = match !type_file with
    | None -> Filename.chop_extension !Location.input_name ^ ".type_mli"
    | Some f -> f

  let exists () = match !type_file with Some _ -> true | _ -> false

  let suppress_underscore =
    let rename =
      let c = ref 0 in
      fun s -> incr c; Printf.sprintf "an_%s_%d" s !c
    and has_pfix =
      let len = String.length inferred_type_prefix in
      fun s ->
        String.length s >= len &&
        String.sub s 0 len = inferred_type_prefix
    in
    let typ mapper ty = match ty.ptyp_desc with
      (* | Ptyp_constr  (_, Ast.TyAny _, ty) *)
      (* | Ptyp_constr (_, ty, Ast.TyAny _) -> ty *)
      | Ptyp_var var when has_pfix var ->
        mapper.AM.typ mapper
          {ty with
           ptyp_desc = Ptyp_var (rename var)
          }
      | _ -> AM.default_mapper.typ mapper ty in
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

  let get_injected_ident_info id =
    Scanf.sscanf id Name.injected_ident_fmt (fun u n -> (u, n))

  let get_fragment_type = function
    | [%type: [%t? typ] Eliom_client_value.fragment ]
    | [%type: [%t? typ] Eliom_client_value.t ] ->
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
        | Some typ -> Some (txt, suppress_underscore typ)
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

  let inferred_sig = lazy (load_file (get_type_file ()))

  let find err {Location. txt ; loc } =
    try Hashtbl.find (Lazy.force inferred_sig) txt with
    | Not_found ->
      Typ.extension ~loc @@ AM.extension_of_error @@ Location.errorf ~loc
        "Error: Inferred type of %s not found. You need to regenerate %s."
        err (get_type_file ())

  let find_escaped_ident = find "escaped ident"
  let find_injected_ident = find "injected ident"
  let find_fragment = find "client value"

end

(** Context convenience module. *)
module Context = struct

  type server = [ `Server | `Shared ]
  type client = [ `Client | `Shared ]

  let of_string = function
    | "server" | "server.start"
    | "eliom.server" | "eliom.server.start" -> `Server
    | "shared" | "shared.start"
    | "eliom.shared" | "eliom.shared.start" -> `Shared
    | "client" | "client.start"
    | "eliom.client" | "eliom.client.start" -> `Client
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


let match_args = function
  | [ ] -> ()
  | [ "-type" ; type_file ] -> Mli.type_file := Some type_file
  | [ "-notype" ] -> Mli.type_file := None
  | args -> Location.raise_errorf ~loc:Location.(in_file !input_name)
           "Wrong arguments:@ %s" (String.concat " " args)

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

  val prelude : loc -> structure
  val postlude : loc -> structure

end

module Cannot_have_fragment = struct

  let opt_forall p = function
    | None -> true
    | Some x -> p x

  let vb_forall p l =
    let p x = p x.pvb_expr in
    List.for_all p l

  let rec longident = function
    | Longident.Lident _ -> true
    | Longident.Ldot (x,_) -> longident x
    | Longident.Lapply (_,_) -> false

  let rec expression e = match e.pexp_desc with
    | Pexp_ident _
    | Pexp_constant _
    | Pexp_function _
    | Pexp_lazy _
    | Pexp_fun _
      -> true

    | Pexp_newtype (_,e)
    | Pexp_assert e
    | Pexp_field (e,_)
    | Pexp_constraint (e,_)
    | Pexp_coerce (e,_,_)
    | Pexp_poly (e,_)
    | Pexp_try (e,_) -> expression e

    | Pexp_ifthenelse (b,e1,e2) ->
      expression b && expression e1 && opt_forall expression e2
    | Pexp_sequence (e1,e2)
    | Pexp_setfield (e1,_,e2) -> expression e1 && expression e2
    | Pexp_array l
    | Pexp_tuple l -> List.for_all expression l
    | Pexp_record (l,e) ->
      let p x = expression @@ snd x in
      opt_forall expression e && List.for_all p l

    | Pexp_construct (_,e)
    | Pexp_variant (_,e) -> opt_forall expression e
    | Pexp_let (_,l,e) -> vb_forall expression l && expression e
    | Pexp_open (_,x,e) -> longident x.txt && expression e
    | Pexp_letmodule (_,me,e) -> module_expr me && expression e

    (* We could be more precise on those constructs *)
    | Pexp_object _
    | Pexp_while _
    | Pexp_for _
    | Pexp_match _
    | Pexp_pack _
      -> false

    (* We can't say more using syntactic information. *)
    | Pexp_extension _
    | Pexp_send _
    | Pexp_new _
    | Pexp_setinstvar _
    | Pexp_override _
    | Pexp_apply _
    | _
      -> false

  and module_expr x = match x.pmod_desc with
    | Pmod_ident l -> longident l.txt
    | Pmod_functor _ -> true
    | Pmod_unpack e -> expression e
    | Pmod_constraint (e,_) -> module_expr e
    | Pmod_structure l -> List.for_all structure_item l

    | Pmod_apply _
    | _
      -> false

  and module_binding m = module_expr m.pmb_expr

  and structure_item x = match x.pstr_desc with
    | Pstr_type _
    | Pstr_typext _
    | Pstr_exception _
    | Pstr_modtype _
    | Pstr_class _
    | Pstr_class_type _
      -> true

    | Pstr_eval (e,_) -> expression e
    | Pstr_value (_,vb) -> vb_forall expression vb
    | Pstr_primitive _ -> true
    | Pstr_module mb -> module_binding mb
    | Pstr_recmodule mbl -> List.for_all module_binding mbl
    | Pstr_open x -> longident x.popen_lid.txt
    | Pstr_include x -> module_expr x.pincl_mod

    | _ -> false

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
    | _ -> AM.default_mapper.expr mapper expr
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
    | _ -> AM.default_mapper.expr mapper expr
  let client = {AM.default_mapper with expr = client_expr (ref `Top)}

  let expr loc expr =
    let server_expr = server.AM.expr server expr in
    let client_expr = client.AM.expr client expr in
    [%expr
      Eliom_shared.Value.create
        [%e server_expr]
        [%client [%e client_expr]]
    ] [@metaloc loc]
end

module Make (Pass : Pass) = struct

  let eliom_expr (context : Context.t ref) mapper expr =
    let loc = expr.pexp_loc in
    let attr = expr.pexp_attributes in
    match expr, !context with
    | {pexp_desc = Pexp_extension ({txt},_)},
      `Client
      when is_annotation txt ["client"; "shared"] ->
      let side = get_extension expr in
      Exp.extension @@ AM.extension_of_error @@ Location.errorf ~loc
        "The syntax [%%%s ...] is not allowed inside client code."
        side
    | {pexp_desc = Pexp_extension ({txt},_)}
    , (`Fragment _ | `Escaped_value _ | `Injection _)
      when is_annotation txt ["client"; "shared"] ->
      let side = get_extension expr in
      Exp.extension @@ AM.extension_of_error @@ Location.errorf ~loc
        "The syntax [%%%s ...] can not be nested."
        side

    (* [%shared ... ] *)
    | {pexp_desc = Pexp_extension ({txt},PStr [{pstr_desc = Pstr_eval (side_val,attr')}])},
      (`Server | `Shared)
      when is_annotation txt ["shared"] ->
      let e = Shared.expr loc side_val in
      mapper.AM.expr mapper @@ exp_add_attrs (attr@attr') e

    (* [%client ... ] *)
    | {pexp_desc = Pexp_extension ({txt},PStr [{pstr_desc = Pstr_eval (side_val,attr)}])},
      (`Server | `Shared as c)
      when is_annotation txt ["client"] ->
      Name.reset_escaped_ident () ;
      let side_val, typ = match side_val with
        | [%expr ([%e? cval]:[%t? typ]) ] -> (cval, Some typ)
        | _ -> (side_val, None)
      in
      let num = Name.fragment_num side_val.pexp_loc in
      let id = Location.mkloc (Name.fragment_ident num) side_val.pexp_loc in
      in_context context (`Fragment c)
        (Pass.fragment ?typ ~context:c ~num ~id % mapper.AM.expr mapper)
        (exp_add_attrs attr side_val)

    (* ~%( ... ) ] *)
    | [%expr ~% [%e? inj ]], _ ->
      let ident = match inj.pexp_desc with
        | Pexp_ident i -> Some (String.concat "_" @@ Longident.flatten i.txt)
        | _ -> None
      in
      begin match !context with
        | `Client | `Shared as c ->
          let id = match ident with
            | Some id -> Name.injected_ident loc id
            | None -> Name.injected_expr loc
          in
          let new_context = `Injection c in
          in_context context new_context
            (Pass.escape_inject ?ident ~context:new_context ~id %
             mapper.AM.expr mapper)
            inj
        | `Fragment c ->
          let id = match ident with
            | None -> Name.escaped_expr loc
            | Some id -> Name.escaped_ident loc id
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
    | _ -> AM.default_mapper.expr mapper expr

  let structure_item mapper str =
    let loc = str.pstr_loc in
    match str.pstr_desc with
    | Pstr_extension (({txt=("server"|"shared"|"client")}, _), _) ->
      Location.raise_errorf ~loc
        "Sections are only allowed at toplevel."
    | _ -> AM.default_mapper.structure_item mapper str

  let signature_item mapper sig_ =
    let loc = sig_.psig_loc in
    match sig_.psig_desc with
    | Psig_extension (({txt=("server"|"shared"|"client")}, _), _) ->
      Location.raise_errorf ~loc "Sections are only allowed at toplevel."
    | _ -> AM.default_mapper.signature_item mapper sig_

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
      let loc = pstr.pstr_loc
      and maybe_reset_injected_idents = function
        | `Client | `Shared ->
          Name.reset_injected_ident ();
        | _ ->
          ()
      in
      match pstr.pstr_desc with
      | Pstr_extension (({txt}, PStr strs), _)
        when is_annotation txt ["shared.start";
                                "client.start";
                                "server.start"] ->
        if strs <> [] then
          [ Str.extension ~loc @@ AM.extension_of_error @@ Location.errorf ~loc
              "The %%%%%s extension doesn't accept arguments." txt ]
        else (
          maybe_reset_injected_idents !context ;
          context := Context.of_string txt ;
          []
        )
      | Pstr_extension (({txt}, PStr strs), _)
        when is_annotation txt ["shared"; "client" ;"server"] ->
        let c = Context.of_string txt in
        let l = flatmap (dispatch_str c mapper) strs in
        maybe_reset_injected_idents c ; l
      | _ ->
        dispatch_str !context mapper pstr
    in
    let loc = {(file_position structs) with loc_ghost = true} in
    module_hash_declaration loc ::
    Pass.prelude loc @
    flatmap f structs @
    Pass.postlude loc

  let toplevel_signature context mapper sigs =
    let f psig =
      let loc = psig.psig_loc in
      match psig.psig_desc with
      | Psig_extension (({txt}, PStr strs), _)
        when is_annotation txt ["shared.start"; "client.start" ;"server.start"] ->
        if strs <> [] then
          [ Sig.extension ~loc @@ AM.extension_of_error @@ Location.errorf ~loc
              "The %%%%%s extension doesn't accept arguments." txt ]
        else ( context := Context.of_string txt ; [] )
      | _ ->
        dispatch_sig !context mapper psig
    in
    flatmap f sigs

  let mapper args =
    let () = match_args args in
    let c = ref `Server in
    {AM.default_mapper
     with
      structure = toplevel_structure c ;
      signature = toplevel_signature c ;
    }

end
