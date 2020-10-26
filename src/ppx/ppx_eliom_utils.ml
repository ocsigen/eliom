module Parsetree = Ppxlib.Parsetree
module Asttypes = Ppxlib.Asttypes
module Longident = Ppxlib.Longident
module Location = Ppxlib.Location
open Ppxlib.Ast
open Ppxlib.Ast_helper

(** Various misc functions *)

let mkloc txt loc = { txt; loc }
let mkloc_opt ?(loc = !default_loc) x = mkloc x loc

let unit ?loc ?attrs () =
  Exp.construct ?loc ?attrs (mkloc_opt ?loc (Longident.Lident "()")) None
let sequence ?loc ?attrs = function
  | [] -> unit ?loc ?attrs ()
  | hd :: tl ->
     List.fold_left (fun e1 e2 -> Exp.sequence ?loc ?attrs e1 e2) hd tl

let str ?loc ?attrs s = Exp.constant ?loc ?attrs (Const.string s)
let int ?loc ?attrs s = Exp.constant ?loc ?attrs (Const.int s)

let punit ?loc ?attrs () =
  Pat.construct ?loc ?attrs (mkloc_opt ?loc (Longident.Lident "()")) None

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
  | [] -> unit ()
  | [e] -> e
  | l -> Exp.tuple l

let pat_args = function
  | [] -> punit ()
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
  for i = 0 to 4 do
    (* Prevent problematic '_ pattern. This confuses our sed
       invocation in eliomc. Simply replacing the pattern here is
       easier than tightening the sed expression. *)
    if Bytes.get o i = '\'' && Bytes.get o (i + 1) = '_' then Bytes.set o i 'Z'
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
  Str.value ~loc Nonrecursive [Vb.mk ~loc id @@ str @@ file_hash loc]

(** The first position in a file, if it exists.
    We avoid {!Location.input_name}, as it's unreliable when reading multiple files.
*)
let file_position str = match str with
  | { pstr_loc } :: _ -> Location.in_file @@ pstr_loc.loc_start.pos_fname
  | [] -> Location.none

let lexing_position ~loc l =
  [%expr
    { Lexing.pos_fname = [%e str l.Lexing.pos_fname];
      Lexing.pos_lnum = [%e int @@ l.Lexing.pos_lnum];
      Lexing.pos_bol = [%e int @@ l.Lexing.pos_bol];
      Lexing.pos_cnum = [%e int @@ l.Lexing.pos_cnum]; }
  ]

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
    let for_expr loc = mkloc (make ()) loc in
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
   tools/eliomc.ml and ocamlbuild/ocamlbuild_eliom.ml *)
let inferred_type_prefix = "eliom_inferred_type_"

module Mli = struct

  let type_file = ref None
  let get_type_file () = match !type_file with
    | None ->
      Filename.chop_extension !Ocaml_common.Location.input_name ^ ".type_mli"
    | Some f ->
      f

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
    (object
      inherit Ppxlib.Ast_traverse.map as super
      method! core_type ty =
        match ty.ptyp_desc with
      (* | Ptyp_constr  (_, Ast.TyAny _, ty) *)
      (* | Ptyp_constr (_, ty, Ast.TyAny _) -> ty *)
        | Ptyp_var var when has_pfix var ->
          super#core_type
            {ty with
             ptyp_desc = Ptyp_var (rename var)
            }
        | _ -> super#core_type ty
    end)#core_type

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
      let ch = open_in file in
      let items =
        Ppxlib.Parse.interface (Lexing.from_channel ch)
      in
      close_in ch;
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
      Typ.extension ~loc @@ Location.Error.to_extension @@
      Location.Error.make ~loc ~sub:[]
        (Printf.sprintf
           "Error: Inferred type of %s not found. You need to regenerate %s."
           err (get_type_file ()))

  let find_escaped_ident = find "escaped ident"
  let find_injected_ident = find "injected ident"
  let find_fragment = find "client value"

end

module Cmo = struct

  let file = ref None

  let exists () = !file <> None

  let record_events events evl =
    let open Instruct in
    List.iter
      (fun ev ->
        match ev with
        | {ev_loc = {loc_start = {Lexing.pos_fname; pos_cnum};
                     loc_end = {Lexing.pos_cnum = pos_cnum'}};
           ev_kind = Event_after ty} ->
           if pos_cnum' = pos_cnum + 1 then
             Hashtbl.add events (pos_fname, pos_cnum) ty
        | _ -> ())
      evl

  let get_file () = match !file with Some f -> f | None -> assert false

  let load () =
    let file = get_file () in
    match open_in file with
    | exception Sys_error s ->
      Location.raise_errorf
        ~loc:(Location.in_file file)
        "Eliom: Error while loading types: %s" s
    | ic ->
       let open Cmo_format in
       let buffer =
         really_input_string ic (String.length Config.cmo_magic_number) in
       if buffer <> Config.cmo_magic_number then
         Location.raise_errorf
           ~loc:(Location.in_file file)
           "Eliom: Error while loading types: not an object file";
       let cu_pos = input_binary_int ic in
       seek_in ic cu_pos;
       let cu = (input_value ic : compilation_unit) in
       if cu.cu_debug = 0 then
         Location.raise_errorf
           ~loc:(Location.in_file file)
           "Eliom: Error while loading types: no debugging information";
       seek_in ic cu.cu_debug;
       let evl = (input_value ic : Instruct.debug_event list) in
       let events = Hashtbl.create 100 in
       record_events events evl;
       close_in ic;
       events

  let events = lazy (load ())

  let label_of_string s =
    if s = "" then
      Asttypes.Nolabel
    else if s.[0] = '?' then
      Asttypes.Optional (String.sub s 1 (String.length s - 1))
    else
      Asttypes.Labelled s

  let rec ident_of_out_ident id =
    let open Outcometree in
    let open Longident in
    match id with
    | Oide_apply (id, id') ->
       Lapply (ident_of_out_ident id, ident_of_out_ident id')
    | Oide_dot (id, nm) ->
       Ldot (ident_of_out_ident id, nm)
    | Oide_ident {printed_name = nm} ->
       Lident nm

  let counter = ref 0

  let type_of_out_type ty =
    let open Outcometree in
    let open Parsetree in
    let map = Hashtbl.create 1 in
    let var x =
      try Hashtbl.find map x with Not_found ->
        let x' = Printf.sprintf "%s%s_%d" inferred_type_prefix x !counter in
        incr counter;
        Hashtbl.add map x x';
        x'
    in
    let rec type_of_out_type ty =
      match ty with
      | Otyp_var (_, s) ->
         Typ.var (var s)
      | Otyp_arrow (lab, ty1, ty2) ->
         Typ.arrow (label_of_string lab)
           (type_of_out_type ty1) (type_of_out_type ty2)
      | Otyp_tuple tyl ->
         Typ.tuple (List.map type_of_out_type tyl)
      | Otyp_constr (id, tyl) ->
         Typ.constr (mkloc (ident_of_out_ident id) Location.none)
           (List.map type_of_out_type tyl)
      | Otyp_object (fields, rest) ->
         let fields =
           List.map
             (fun (label, ty) ->
               {pof_desc = Otag (mkloc label Location.none,
                                 type_of_out_type ty);
                pof_loc = Location.none;
                pof_attributes = []})
             fields
         in
         Typ.object_ (fields) (if rest = None then Closed else Open)
      | Otyp_class (_, id, tyl) ->
         Typ.class_ (mkloc (ident_of_out_ident id) Location.none)
           (List.map type_of_out_type tyl)
      | Otyp_alias (ty, s) ->
         Typ.alias (type_of_out_type ty) (var s)
      | Otyp_variant (_, Ovar_typ ty, closed, tags) ->
         Typ.variant [Rf.mk (Rinherit (type_of_out_type ty))]
           (if closed then Closed else Open) tags
      | Otyp_variant (_, Ovar_fields lst, closed, tags) ->
         let row_fields =
           List.map
             (fun (label, const, tyl) ->
               Rf.mk (Rtag (mkloc label Location.none,
                            const,
                            List.map type_of_out_type tyl)))
             lst
         in
         Typ.variant row_fields (if closed then Closed else Open) tags
      | Otyp_poly (sl, ty) ->
         Typ.poly (List.map (fun v -> mkloc (var v) Location.none) sl)
           (type_of_out_type ty)
      | Otyp_abstract | Otyp_open | Otyp_sum _ | Otyp_manifest _
        | Otyp_record _ | Otyp_module _ | Otyp_attribute _ | Otyp_stuff _ ->
         assert false
    in
    type_of_out_type ty

  let typ ty =
    let ty = Printtyp.tree_of_type_scheme ty in
    type_of_out_type ty

  let find err loc =
    let {Lexing.pos_fname; pos_cnum} = loc.Location.loc_start in
    try
      typ (Hashtbl.find (Lazy.force events) (pos_fname, pos_cnum))
    with Not_found ->
      Typ.extension ~loc @@ Location.Error.to_extension @@
      Location.Error.make ~loc ~sub:[]
        (Printf.sprintf
           "Error: Inferred type of %s not found. You need to regenerate %s."
           err (get_file ()))

  let find_escaped_ident = find "escaped ident"
  let find_injected_ident = find "injected ident"
  let find_fragment loc =
    match Mli.get_fragment_type (find "client value" loc) with
    | Some ty -> ty
    | None -> assert false

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
    | `Fragment of server * bool (* [%client ... ] *)
    | `Escaped_value of server (* [%shared ~%( ... ) ] *)
    | `Injection of client (* [%%client ~%( ... ) ] *)
  ]
end


let driver_args = [
  "-type", Arg.String (fun type_file -> Mli.type_file := Some type_file),
    "FILE Load inferred types from FILE.";
  "-notype", Arg.Unit (fun () -> Mli.type_file := None),
    " Unset explicitly set path from which to load inferred types.";
  "-server-cmo", Arg.String (fun file -> Cmo.file := Some file),
    "FILE Load inferred types from server cmo file FILE."
  ]

let () =
  List.iter (fun (key, spec, doc) -> Ppxlib.Driver.add_arg key spec ~doc)
    driver_args

(** Signature of specific code of a preprocessor. *)
module type Pass = sig

  (** How to handle "client", "shared" and "server" sections for top level structure items. *)

  val shared_str: bool -> structure_item -> structure_item list
  val server_str: bool -> structure_item -> structure_item list
  val client_str: structure_item -> structure_item list

  (** How to handle "client", "shared" and "server" sections for top level signature items. *)

  val shared_sig: signature_item -> signature_item list
  val client_sig: signature_item -> signature_item list
  val server_sig: signature_item -> signature_item list

  (** How to handle "[\%client ...]" and "[\%shared ...]" expr. *)
  val fragment:
    loc:Location.t -> ?typ:core_type -> context:Context.server ->
    num:string -> id:string Location.loc -> unsafe:bool ->
    expression -> expression

  (** How to handle escaped "~%ident" inside a fragment. *)
  val escape_inject:
    loc:Location.t -> ?ident:string -> context:Context.escape_inject ->
    id:string Location.loc -> unsafe:bool ->
    expression -> expression

  val prelude : loc -> structure
  val postlude : loc -> structure

end

(** These functions try to guess if a given expression will lead to a fragment evaluation
    This is not possible in general, this criteria is only syntactic

    If the expression cannot have fragments, we don't need to use sections.
    Consequently, this function should *never* return false positive.
*)
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
    | Pexp_open (i,e) -> module_expr i.popen_expr && expression e
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
    | Pstr_open x -> module_expr x.popen_expr
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
  let server =
    (object
       inherit Ppxlib.Ast_traverse.map as super
       method! expression expr =
         match expr with
         | [%expr [%client [%e? _ ]]] -> expr
         | [%expr [%client.unsafe [%e? _ ]]] -> expr
         | [%expr ~% [%e? injection_expr ]] -> injection_expr
         | _ -> super#expression expr
     end) #expression

  let client expr =
    let context = (ref `Top) in
    (object (self)
       inherit Ppxlib.Ast_traverse.map as super
       method! expression expr =
         match expr with
         | [%expr [%client [%e? fragment_expr ]]]
           | [%expr [%client.unsafe [%e? fragment_expr ]]] ->
            in_context context `Fragment
              self#expression fragment_expr
         | [%expr ~% [%e? injection_expr ]] ->
            begin match !context with
            | `Top -> expr
            | `Fragment -> injection_expr
            end
         | _ -> super#expression expr
     end)#expression expr

  let expr loc ~unsafe expr =
    let server_expr = server expr in
    let client_expr = client expr in
    if unsafe then
      [%expr
          Eliom_shared.Value.create
          [%e server_expr]
          [%client.unsafe [%e client_expr]]
      ]
    else
      [%expr
          Eliom_shared.Value.create
          [%e server_expr]
          [%client [%e client_expr]]
      ]
end

module Make (Pass : Pass) = struct
  let eliom_mapper context =
    let context = ref (context :> Context.t) in
    (object (self)
      inherit Ppxlib.Ast_traverse.map as super
      method! expression expr =
        let loc = expr.pexp_loc in
        let attr = expr.pexp_attributes in
        match expr, !context with
        | {pexp_desc = Pexp_extension ({txt},_)},
          `Client
             when is_annotation txt ["client"; "shared";
                                     "client.unsafe"; "shared.unsafe"] ->
           let side = get_extension expr in
           Exp.extension @@ Location.Error.to_extension @@
             Location.Error.make ~loc ~sub:[]
               (Printf.sprintf
                  "The syntax [%%%s ...] is not allowed inside client code."
                  side)
        | {pexp_desc = Pexp_extension ({txt},_)}
        , (`Fragment _ | `Escaped_value _ | `Injection _)
             when is_annotation txt ["client"; "shared";
                                     "client.unsafe"; "shared.unsafe"] ->
           let side = get_extension expr in
           Exp.extension @@ Location.Error.to_extension @@
             Location.Error.make ~loc ~sub:[]
               (Printf.sprintf
                  "The syntax [%%%s ...] can not be nested."
                  side)

        (* [%shared ... ] *)
        | {pexp_desc =
             Pexp_extension ({txt},
                             PStr [{pstr_desc = Pstr_eval (side_val,attr')}])},
          (`Server | `Shared)
             when is_annotation txt ["shared"; "shared.unsafe"] ->
           let unsafe = is_annotation txt ["shared.unsafe"] in
           let e = Shared.expr loc ~unsafe side_val in
           self#expression @@ exp_add_attrs (attr@attr') e

        (* [%client ... ] *)
        | {pexp_desc =
             Pexp_extension ({txt},
                             PStr [{pstr_desc = Pstr_eval (side_val,attr)}])},
          (`Server | `Shared as c)
             when is_annotation txt ["client"; "client.unsafe"] ->
           Name.reset_escaped_ident () ;
           let side_val, typ = match side_val with
             | [%expr ([%e? cval]:[%t? typ]) ] -> (cval, Some typ)
             | _ -> (side_val, None)
           in
           let num = Name.fragment_num side_val.pexp_loc in
           let id = mkloc (Name.fragment_ident num) side_val.pexp_loc in
           let unsafe = is_annotation txt ["client.unsafe"] in
           in_context context (`Fragment (c, unsafe))
             (Pass.fragment ~loc ?typ ~context:c ~num ~id ~unsafe
              % self#expression)
             (exp_add_attrs attr side_val)

        (* ~%( ... ) ] *)
        | [%expr ~% [%e? inj ]], _ ->
           let ident = match inj.pexp_desc with
             | Pexp_ident i ->
                Some (String.concat "_" @@ Longident.flatten_exn i.txt)
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
                (Pass.escape_inject ~loc ?ident ~context:new_context ~id
                   ~unsafe:false %
                   self#expression)
                inj
           | `Fragment (c, unsafe) ->
              let id = match ident with
                | None -> Name.escaped_expr loc
                | Some id -> Name.escaped_ident loc id
              in
              let new_context = `Escaped_value c in
              in_context context new_context
                (Pass.escape_inject
                   ~loc ?ident ~context:new_context ~id ~unsafe %
                   self#expression)
                inj
           | `Server ->
              Location.raise_errorf ~loc
                "The syntax ~%% ... is not allowed inside server code."
           | `Escaped_value _ | `Injection _ ->
              Location.raise_errorf ~loc
                "The syntax ~%% ... can not be nested."
           end
        | _ -> super#expression expr

  method! structure_item str =
    let loc = str.pstr_loc in
    match str.pstr_desc with
    | Pstr_extension (({txt=("server"|"shared"|"client")}, _), _) ->
      Location.raise_errorf ~loc
        "Sections are only allowed at toplevel."
    | _ -> super#structure_item str

  method! signature_item sig_ =
    let loc = sig_.psig_loc in
    match sig_.psig_desc with
    | Psig_extension (({txt=("server"|"shared"|"client")}, _), _) ->
      Location.raise_errorf ~loc "Sections are only allowed at toplevel."
    | _ -> super#signature_item sig_
     end)

  (** Toplevel translation *)
  (** Switch the current context when encountering [%%server] (resp. shared, client)
      annotations. Call the eliom mapper and [Pass.server_str] (resp ..) on each
      structure item.
  *)

  let dispatch_str context stri =
    (* We must do this before any transformation on the structure. *)
    let no_fragment = Cannot_have_fragment.structure_item stri in
    let f = match context with
      | `Server -> Pass.server_str no_fragment
      | `Shared -> Pass.shared_str no_fragment
      | `Client -> Pass.client_str
    in
    let m = eliom_mapper context in
    f @@ m#structure_item stri

  let dispatch_sig context sigi =
    let f = match context with
      | `Server -> Pass.server_sig
      | `Shared -> Pass.shared_sig
      | `Client -> Pass.client_sig
    in
    let m = eliom_mapper context in
    f @@ m#signature_item sigi

  let toplevel_structure context structs =
    let rec f pstr =
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
          [ Str.extension ~loc @@ Location.Error.to_extension @@
              Location.Error.make ~loc ~sub:[]
                (Printf.sprintf
                   "The %%%%%s extension doesn't accept arguments." txt) ]
        else (
          maybe_reset_injected_idents !context ;
          context := Context.of_string txt ;
          []
        )
      | Pstr_extension (({txt}, PStr strs), _)
        when is_annotation txt ["shared"; "client" ;"server"] ->
        let c = Context.of_string txt in
        let l = flatmap (dispatch_str c) strs in
        maybe_reset_injected_idents c ; l
      | Pstr_include {pincl_mod = {pmod_desc = Pmod_structure l;
                                   pmod_attributes = []};
                      pincl_attributes = []} ->
        flatmap f l
      | _ ->
        dispatch_str !context pstr
    in
    let loc = {(file_position structs) with loc_ghost = true} in
    module_hash_declaration loc ::
    Pass.prelude loc @
    flatmap f structs @
    Pass.postlude loc

  let toplevel_signature context sigs =
    let f psig =
      let loc = psig.psig_loc in
      match psig.psig_desc with
      | Psig_extension (({txt}, PStr strs), _)
        when is_annotation txt ["shared.start"; "client.start" ;"server.start"] ->
        if strs <> [] then
          [ Sig.extension ~loc @@ Location.Error.to_extension @@
              Location.Error.make ~loc ~sub:[]
                (Printf.sprintf
                   "The %%%%%s extension doesn't accept arguments." txt) ]
        else ( context := Context.of_string txt ; [] )
      | Psig_extension (({txt}, PSig sigs), _)
        when is_annotation txt ["shared"; "client" ;"server"] ->
        let c = Context.of_string txt in
        flatmap (dispatch_sig c) sigs
      | _ ->
        dispatch_sig !context psig
    in
    flatmap f sigs

  let mapper =
    let c = ref `Server in
    object
      inherit Ppxlib.Ast_traverse.map
      method! structure s = toplevel_structure c s
      method! signature s = toplevel_signature c s
    end

end
