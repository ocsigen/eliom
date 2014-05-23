
module Make(Syntax : Camlp4.Sig.Camlp4Syntax) = struct

  let sig_inc = ref []

  let _ =
    Camlp4.Options.add "-sig-inc" (Arg.String (fun s ->
        sig_inc := s::!sig_inc)) "descr"

  include Syntax

  let rec ctyp_unapply al =
    function
    | Ast.TyApp (_, f, a) -> ctyp_unapply (a :: al) f
    | f -> (f, al)

  class subst_var env = object (self)
    inherit Ast.map as super
    method ctyp ty = match ty with
      | <:ctyp@_loc< '$lid:a$ >> ->
        begin
          try List.assoc a env
          with Not_found -> ty end
      | ty -> super#ctyp ty
  end

  class subst_type env = object (self)
    inherit Ast.map as super
    method sig_item si = match si with
      | Ast.SgTyp (_loc, (Ast.TyDcl (_, lid, _, Ast.TyNil _, _)))
        when List.mem_assoc lid env -> <:sig_item< >>
      | si -> super#sig_item si
    method ctyp ty = match ty with
      | <:ctyp@_loc< $lid:lid$ >> when List.mem_assoc lid env ->
        let _, _, ty = List.assoc lid env in
        ty
      | Ast.TyApp (_loc, _, _) -> begin
          let (id, args) = ctyp_unapply [] ty in
          match id with
          | <:ctyp< $lid:lid$ >> when List.mem_assoc lid env ->
            let args = List.map self#ctyp args in
            let _loc, vars, ty = List.assoc lid env in
            let env =
              try List.combine vars args
              with _ -> Loc.raise _loc (Failure "Invalid type arity") in
            (new subst_var env)#ctyp ty
          | _ -> super#ctyp ty
        end
      | ty -> super#ctyp ty
  end

  let create_env wc =
    let varname = function
      | <:ctyp@_loc< '$lid:a$ >> -> a
      | _ -> assert false
    in
    let map_type wc = match wc with
      | <:with_constr< type $typ:ty1$ := $typ:ty2$ >> -> begin
          let id, vars = ctyp_unapply [] ty1 in
          match id with
          | <:ctyp< $lid:lid$ >> ->
            (lid, (Ast.loc_of_ctyp ty1,List.map varname vars, ty2))
          | _ -> assert false
        end
      | _ ->
        Loc.raise
          (Ast.loc_of_with_constr wc)
          (Failure "Unhandled substitution")
    in
    List.map map_type (Ast.list_of_with_constr wc [])

  let subst_type wc mt =
    let _loc = Ast.Loc.ghost in
    let env = create_env wc in
    (new subst_type env)#module_type mt

  let rec find_in_paths f paths = match paths with
    | [] -> raise Not_found
    | x::xs ->
      let f' = Filename.concat x f in
      (* Printf.eprintf "look in %s \n" x; *)
      if Sys.file_exists f'
      then f'
      else find_in_paths f xs

  let load_file sourcedir f =
    let f = try find_in_paths f (List.rev !sig_inc) with
      | Not_found ->
        Printf.eprintf "Error: File not found test (%s)\n" f;
        Printf.eprintf "in \n %s\n" (String.concat ",\n " (List.rev_map (fun s -> Filename.concat s f) !sig_inc));
        exit 1 in
    (* Printf.eprintf "Loading file %S\n" f; *)
    let ic = open_in f in
    (* Format.eprintf "Pa_include %s\n" f; *)
    try
      let (items, stopped) =
        Gram.parse interf
          (Loc.mk  (f ^ " " ^ string_of_int (Random.int 1000000 )))
          (Stream.of_channel ic) in
      assert (stopped = None);
      close_in ic;
      items
    with
    | Not_found ->
      Printf.eprintf "Error: File not found (%s)\n" f;
      close_in ic;
      exit 1
    | e ->
      Printf.eprintf "%s\n" (Camlp4.ErrorHandler.to_string e);
      close_in ic;
      exit 1

        (* Extending syntax *)
        EXTEND Gram
        GLOBAL: module_type sig_item;

      sig_item: BEFORE "top"
          [ [ "include"; mt = module_type ->
              begin match mt with
                | <:module_type< sig $x$ end>> ->
                  (* Hack: insert SgNil with a correct location, in order to
                           preserve comments locations *)
                  Ast.(SgSem(_loc, SgNil _loc, x))
                | mt -> <:sig_item@here< include $mt$ >>
              end
            ] ];

      module_type: LEVEL "with"
          [ [ mt = SELF; "subst"; wc = with_constr ->
              <:module_type@here< $subst_type wc mt$ >> ] ];

      module_type: LEVEL "simple"
          [ [ mli = a_STRING ->
              let sourcedir = Filename.dirname (Loc.file_name _loc) in
              (<:module_type@here< sig $list:load_file sourcedir mli$ end >>)] ];

      END

end

module Id : Camlp4.Sig.Id = struct
  let name = "Include .mli as module signature."
  let version = "alpha"
end

module M = Camlp4.Register.OCamlSyntaxExtension(Id)(Make)
