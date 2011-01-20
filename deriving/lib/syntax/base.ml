(*pp $CAMLP4OF *)

(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

open Utils
open Type
open Camlp4.PreCast

open Context

exception Underivable of string
exception NoSuchClass of string

let fatal_error loc msg =
  Syntax.print_warning loc msg;
  exit 1

let display_errors loc f p =
  try
    f p
  with
    Underivable msg | Failure msg -> fatal_error loc msg

(* Environment for code generation *)

let insert_params ctxt ?(prefix="m") params =
  { ctxt with names = List.fold_right
      (fun param map ->
	ExprMap.add param (Printf.sprintf "%s_%s" prefix (param_name param)) map)
      params ctxt.names;
    tnames = List.fold_right
      (fun param map ->
	ExprMap.add param (Printf.sprintf "t%s_%s" prefix (param_name param)) map)
      params ctxt.tnames
 }

(* *)

let outside_ctxt = {
  wrapper_id = "DummyId";
  params = [];
  inside = false;
  names = ExprMap.empty;
  tnames = ExprMap.empty;
  suffix = None;
  decls = NameMap.empty;
  generate = (fun _ -> fun _ -> assert false);
}

(* Static context for code generator. *)

module InContext(L: Loc)(C : DeriverParams) =
struct

  include L
  include C
  module Untranslate = Untranslate(L)

  let seq l r = <:expr< $l$ ; $r$ >>
  let rec seq_list = function
    | [] -> <:expr< () >>
    | [e] -> e
    | e::es -> seq e (seq_list es)

  let record_pattern ?(prefix="") (fields : Type.field list) : Ast.patt =
    <:patt<{$list:
              (List.map (fun (label,_,_) -> <:patt< $lid:label$ = $lid:prefix ^ label$ >>)
                      fields) $}>>

  let record_expr  (fields : (string * Ast.expr) list) : Ast.expr =
    (* let fs = *)
      (* List.fold_left *)
        (* (fun l r -> <:rec_binding< $l$ ; $r$ >>) *)
	(* <:rec_binding< >> *)
      (* (List.map (fun (label, exp) -> <:rec_binding< $lid:label$ = $exp$ >>) *)
         (* fields) in *)
    <:expr< { $list:List.map (fun (label, exp) -> <:rec_binding< $lid:label$ = $exp$ >>)
		fields$ } >>

  let record_expression ?(prefix="") (fields : Type.field list) : Ast.expr =
      (* let es = List.fold_left *)
        (* (fun l r -> <:rec_binding< $l$ ; $r$ >>) *)
	  (* <:rec_binding< >> *)
        (* (List.map (fun (label,_,_) -> <:rec_binding< $lid:label$ = $lid:prefix ^ label$ >>) *)
           (* fields) in *)
      <:expr< { $list:List.map (fun (label,_,_) -> <:rec_binding< $lid:label$ = $lid:prefix ^ label$ >>)
           fields$ } >>

  let mproject mexpr (name:string) =
    match mexpr with
      | <:module_expr< $id:m$ >> -> <:expr< $id:m$.$lid:name$ >>
      | _ -> <:expr< let module M = $mexpr$ in M.$lid:name$ >>

  let mProject mexpr name =
    match mexpr with
      | <:module_expr< $uid:m$ >> -> <:module_expr< $uid:m$.$uid:name$ >>
      | _ -> <:module_expr< struct module M = $mexpr$ include M.$uid:name$ end >>

  let expr_list : Ast.expr list -> Ast.expr =
      (fun exprs ->
         List.fold_right
           (fun car cdr -> <:expr< $car$ :: $cdr$ >>)
           exprs
         <:expr< [] >>)

  let patt_list : Ast.patt list -> Ast.patt =
      (fun patts ->
         List.fold_right
           (fun car cdr -> <:patt< $car$ :: $cdr$ >>)
           patts
         <:patt< [] >>)

  let tuple_expr : Ast.expr list -> Ast.expr = function
    | [] -> <:expr< () >>
    | [x] -> x
    | x::xs -> Ast.ExTup (_loc, List.fold_left (fun e t -> Ast.ExCom (_loc, e,t)) x xs)

  let tuple ?(prefix="v") n : string list * Ast.patt * Ast.expr =
    let v n = Printf.sprintf "%s%d" prefix n in
      match n with
        | 0 -> [], <:patt< () >>, <:expr< () >>
        | 1 -> [v 0], <:patt< $lid:v 0$ >>, <:expr< $lid:v 0$ >>
        | n ->
            let patts, exprs =
              (* At time of writing I haven't managed to write anything
                 using quotations that generates an n-tuple *)
              List.fold_left
                (fun (p, e) (patt, expr) -> Ast.PaCom (_loc, p, patt), Ast.ExCom (_loc, e, expr))
                (<:patt< >>, <:expr< >>)
                (List.map (fun n -> <:patt< $lid:v n$ >>, <:expr< $lid:v n $ >>)
                   (List.range 0 n))
            in
              List.map v (List.range 0 n), Ast.PaTup (_loc, patts), Ast.ExTup (_loc, exprs)

  let instantiate_modargs, instantiate_modargs_repr =
    let lookup ctxt var =
      try
	`Constr ([ExprMap.find (`Param var) ctxt.tnames], [])
      with Not_found ->
	failwith ("Unbound type parameter '" ^ fst var)
    in
    let rec o ctxt = object
      inherit transform as super
      method expr = function
	| `Param p -> lookup ctxt p
	| e        -> super#expr e
      method poly_expr (params, expr) =
	let ctxt = {ctxt with
		    tnames = List.fold_left
		      (fun tnames ((name,_) as p) -> ExprMap.add (`Param p) name tnames)
                      ctxt.tnames params } in
	(params, (o ctxt)#expr expr)
    end in
    (fun ctxt -> (o ctxt)#expr),
    (fun ctxt -> (o ctxt)#repr)

  let cast_pattern ctxt ?(param="x") t =
    let t = Untranslate.expr (instantiate_modargs ctxt t) in
      (<:patt< $lid:param$ >>,
       <:expr<
         let module M =
             struct
               type t = ($t$)
               let test = function #t -> true | _ -> false
             end in M.test $lid:param$ >>,
       <:expr<
         (let module M =
              struct
                type t = ($t$)
                let cast = function #t as t -> t | _ -> assert false
              end in M.cast $lid:param$ )>>)

  let rec modname_from_qname qname =
    match qname with
      | [] -> invalid_arg "modname_from_qname"
      | [t] -> <:ident< $uid:classname ^ "_"^ t$ >>
      | t::ts -> <:ident< $uid:t$.$modname_from_qname ts $ >>

  let apply_functor (f : Ast.module_expr) (args : Ast.expr list) : Ast.expr =
    List.fold_left (fun f p -> <:expr< $f$ $p$ >>) (mproject f "make") args

  let atype_expr ctxt expr =
    Untranslate.expr (instantiate_modargs ctxt expr)

  let atype ctxt (name, params, rhs, _, _) =
    match rhs with
    | `Fresh _ | `Variant _ | `Nothing ->
	let params =
	  List.map
	    (fun p -> `Param (ExprMap.find (`Param p) ctxt.names, None))
	    params in
	Untranslate.expr (`Constr ([name], params))
    | `Expr e -> atype_expr ctxt e

  let wrap_default m = match default_module with
  | None -> m
  | Some (name,_) ->
      <:module_expr< $uid:runtimename$.$uid:name$($m$) >>

  let make_include_rec_inst ctxt ty =
    let name = String.uncapitalize (ExprMap.find ty ctxt.names) in
    let params =
      List.map
	(fun p -> <:expr< $lid:ExprMap.find p ctxt.names$ >>)
	ctxt.params in
    let mexpr = apply_functor <:module_expr< $uid:ctxt.wrapper_id$ >> params in
    match ctxt.suffix with
    | None -> <:expr< Lazy.force ($mexpr$.$uid:ctxt.wrapper_id$.$lid:name$) >>
    | Some (r, c, _) ->
	<:expr< let module M =
	               (val Lazy.force $mexpr$.$uid:ctxt.wrapper_id$.$lid:name$
	                 : $uid:r$.$uid:c$ with type a = $atype_expr ctxt ty$)
	        in M.$lid:String.uncapitalize classname$ >>

  let unpack ?(classname = classname) ?(runtimename = runtimename) ctxt e ty =
    match e with
    | <:expr< (module $m$) >> -> m
    | _ ->
	<:module_expr< (val $e$ : $uid:runtimename$.$uid:classname$
	                          with type a = $atype_expr ctxt ty$) >>

  let pack ?(classname = classname) ?(runtimename = runtimename) ctxt m ty =
    match m with
    | <:module_expr< (val $e$) >> -> e
    | _ ->
	<:expr< (module $m$ : $uid:runtimename$.$uid:classname$
                              with type a = $atype_expr ctxt ty$) >>

  let rdepends =
    List.map
      (fun d -> (module (val d : DeriverDepends)(L) : RawDeriverDepends))
      depends

  let unpack_depends ctxt expr ty =
    List.fold_left (fun e d ->
      let module M = (val d : RawDeriverDepends) in
      <:expr< let module $uid:M.classname$ =
	(val $lid:String.uncapitalize M.classname$
	  : $uid:M.runtimename$.$uid:M.classname$
	with type a = $atype_expr ctxt ty$) in $e$ >>)
      expr rdepends

  let pack_depends ctxt ty =
    List.map (fun d ->
      let module M = (val d : RawDeriverDepends) in
      let ctxt = { ctxt with suffix = Some (runtimename, classname, ty) } in
      <:str_item<
        let $lid:String.uncapitalize M.classname$ =
	  $pack
	    ~runtimename:M.runtimename
	    ~classname:M.classname ctxt (M.generate_expr ctxt ty) ty$ >>)
      rdepends

  class virtual generator =
  object (self)

    method wrap ctxt ty items =
      let mexpr =
	<:module_expr< struct
	  type a = ($atype_expr ctxt ty$)
	  $list:pack_depends ctxt ty$
	  $list:items$
	end >> in
      wrap_default mexpr

    method mapply ctxt (funct : Ast.module_expr) args =
      apply_functor funct (List.map (fun ty -> pack ctxt (self#expr ctxt ty) ty) args)

    method virtual variant:
	context -> decl -> expr NameMap.t -> variant -> Ast.str_item list
    method virtual sum:
	?eq:expr -> context -> decl -> expr NameMap.t -> summand list -> Ast.str_item list
    method virtual record:
	?eq:expr -> context -> decl -> expr NameMap.t -> field list -> Ast.str_item list
    method virtual tuple: context -> expr list -> Ast.str_item list
    method virtual alpha: context -> Type.param -> Ast.str_item list

    method param ctxt p =
      match ctxt.suffix with
      | None ->
	  unpack ctxt <:expr< $lid:ExprMap.find (`Param p) ctxt.names$ >> (`Param p)
      | Some (r,c, _) ->
	  let mexpr = <:module_expr<
	    (val $lid:ExprMap.find (`Param p) ctxt.names$
                   : $uid:r$.$uid:c$ with type a = $atype_expr ctxt (`Param p)$) >>
	  in
	  unpack ctxt (mproject mexpr (String.uncapitalize classname)) (`Param p)

    method object_   _ o =
      raise (Underivable (classname ^ " cannot be derived for object types"))
    method class_    _ c =
      raise (Underivable (classname ^ " cannot be derived for class types"))
    method label     _ l =
      raise (Underivable (classname ^ " cannot be derived for label types"))
    method function_ _ f =
      raise (Underivable (classname ^ " cannot be derived for function types"))

    method constr ctxt (qname, params) =
      let ty = `Constr (qname, params) in
      let m = try
        (* Find module in context of the current type declaration... *)
	let name = ExprMap.find ty ctxt.names in
        if ctxt.inside then (* inside the recursive module definition *)
	  match ctxt.suffix with
	  | None -> <:expr< Lazy.force $lid:String.uncapitalize name$ >>
	  | Some (r, c, (`Constr ([name], params) as ty')) when ty' = ty ->
	      let e = apply_functor
		  <:module_expr< $uid:Printf.sprintf "%s_%s" classname name$ >>
	        (List.map (fun p -> pack ctxt (self#expr ctxt p) p)params) in
	      <:expr< $e$ >>
	  | Some (r, c, _) ->
	      let e = <:expr< Lazy.force $lid:String.uncapitalize name$ >> in
	      let me = unpack ~classname:c ~runtimename:r ctxt e ty in
	      mproject me (String.uncapitalize classname)
	else (* outside the recursive module definition *)
	  make_include_rec_inst ctxt ty
      with Not_found ->
	(* ... or apply previously defined deriver ((pre)defined or not) *)
        let qname =
	  try [runtimename ; List.assoc qname predefs]
	  with Not_found -> qname in
	let id = <:module_expr< $id:modname_from_qname qname$ >> in
        self#mapply ctxt id params in
      unpack ctxt m ty

    method expr ctxt ty = match ty with
      | `Param p    ->                   (self#param      ctxt p)
      | `Object o   -> self#wrap ctxt ty (self#object_    ctxt o)
      | `Class c    -> self#wrap ctxt ty (self#class_     ctxt c)
      | `Label l    -> self#wrap ctxt ty (self#label      ctxt l)
      | `Function f -> self#wrap ctxt ty (self#function_  ctxt f)
      | `Constr c   ->                   (self#constr     ctxt c)
      | `Tuple t    -> self#wrap ctxt ty (self#tuple      ctxt t)

    method rhs ctxt (tname, params, rhs, constraints, _ as decl) subst =
      let ty =
	substitute_expr subst
	  (`Constr([tname], List.map (fun p -> `Param p) params)) in
      match substitute_rhs subst rhs with
      | `Fresh (_, _, (`Private : [`Private|`Public])) when not allow_private ->
          raise (Underivable ("The class " ^ classname ^
			      " cannot be derived for private types"))
      | `Fresh (eq, Sum summands, _) ->
          self#wrap ctxt ty (self#sum ?eq ctxt decl subst summands)
      | `Fresh (eq, Record fields, _) ->
	  self#wrap ctxt ty (self#record ?eq ctxt decl subst fields)
      | `Expr e -> self#expr ctxt e
      | `Variant v ->
	  self#wrap ctxt ty (self#variant ctxt decl subst v)
      | `Nothing -> <:module_expr< >>

    method call_poly_expr ctxt (params, ty : Type.poly_expr) name =
      let params = List.map (fun p -> `Param p) params in
      let ctxt = insert_params ctxt ~prefix:"pv" params in
      let expr = self#call_expr ctxt ty name in
      List.fold_right (fun (`Param a as p) expr ->
	let ty = "tpv_"^param_name p in
	let m = wrap_default <:module_expr< struct
	    type a = $lid:ty$
	    $list:pack_depends ctxt p$
	    $list:self#alpha ctxt a$
	  end >> in
	<:expr< fun (type $lid:ty$) ->
	  let $lid:ExprMap.find p ctxt.names$ =
	    (module $mexp:m$ : $uid:runtimename$.$uid:classname$ with type a = $atype_expr ctxt p$)in
	  $expr$ >>) params expr

    method call_expr ctxt ty name = mproject (self#expr ctxt ty) name

  end

  (** *)

  let extract_recursive_calls decls : ESet.t list =
    let names = List.map (fun (name,_,_,_,_) -> name) decls in
    let obj = (object (self)
      inherit [ESet.t] fold as default
      method crush sets = List.fold_left ESet.union ESet.empty sets
      method expr e = match e with
      | `Constr ([name], args) as e when List.mem name names ->
	  ESet.add (name, args) (default#expr e)
      | e -> default#expr e
    end) in
    List.map (fun (_,_,rhs,_,_) -> obj#rhs rhs) decls

  (* Find all recursive instance needed by a set of type declarations.
     Throw an exception if the set is known to be infinite
     (a.k.a. non-regural types). *)
  let close_decls (decls: Type.decl list) : (Type.decl * ESet.t) list =

    let check_regular_instance name (name', args') =
      name <> name' ||
      List.for_all
	(function (`Constr _ | `Tuple _ | `Function _) as e -> not (contains_tvars e)
	  | _ -> true) args' in

    let expand (tys : (Type.decl * ESet.t) list) name ty_set (name', args') =
      let ((_, params',_,_,_), ty_set') = List.find (fun ((n,_,_,_,_),_) -> n = name') tys in
      let subst = NameMap.fromList (List.map2 (fun (p, _) a -> p, a) params' args') in
      ESet.fold
	(fun (name'', args'') acc ->
	  let new_ty = name'', List.map (substitute_expr subst) args'' in
	  if not (check_regular_instance name new_ty) then
            failwith ("The following types contain non-regular recursion:\n   "
                      ^String.concat ", " (List.map (fun ((n,_,_,_,_),_)->n) tys)
                      ^"\nderiving does not support non-regular types");
	  if ESet.mem new_ty ty_set then acc else ESet.add new_ty acc)
	ty_set' ESet.empty in

    let expands (tys : (Type.decl * ESet.t) list) =
      List.map
	(fun ((name,_,_,_,_),ty_set) ->
	  ESet.fold
	    (fun ty acc -> ESet.union (expand tys name ty_set ty) acc)
	    ty_set ESet.empty)
	tys in

    let aggregate_new_tys (tys : (Type.decl * ESet.t) list) new_tys =
      List.map2 (fun (d,set) new_set -> d, ESet.union set new_set) tys new_tys in

    let rec loop_close_decls (tys : (Type.decl * ESet.t) list) new_tys =
      if List.for_all (fun l -> l = ESet.empty) new_tys then tys
      else
	let tys = aggregate_new_tys tys new_tys in
	let new_tys = expands tys in
	loop_close_decls tys new_tys
    in
    loop_close_decls
      (List.map (fun d -> d, ESet.empty) decls)
      (extract_recursive_calls decls)

  let rename_params (name, params, rhs, constraints, deriving as decl) =
    (* Replace type variables [params] with 'a 'b 'c ... *)
    (* TODO rename polymorphic variable... *)
    if deriving then decl else
    let map = (List.mapn (fun (o, v) n -> o, (typevar_of_int n, v)) params) in
    let subst = NameMap.fromList (List.map (fun (o, n) -> o, `Param n) map) in
    ((name, List.map snd map, substitute_rhs subst rhs,
      List.map (substitute_constraint subst) constraints, false))

  let add_instances functors (((name,params,_,_,_) as decl), insts) =
    (* Determine types variables involved in recursion. *)
    (* Example, type variable 'b is not involved in recursion :
          type 'a t1 = ('a, int) t2
          and ('a, 'b) t2 = A of 'a t1 | B of 'b deriving (Show) *)
    let freevars = ESet.fold
	(fun (name, args) acc ->
	  ExprSet.union (Type.free_tvars (`Constr ([name], args))) acc)
	insts ExprSet.empty in
    if (not (ExprSet.for_all
	       (fun var -> List.exists (fun p -> var = `Param p) params) freevars)) then
      failwith ("Recursive polymorphic call is not allowed.");
    (* Then regroups with instances that shares effective parameters. *)
    let rec loop functors =
      match functors with
      | [] -> [insts, freevars, [decl]]
      | (insts', vars, decls) :: functors when ExprSet.equal freevars vars ->
	  (ESet.union insts insts', vars, decl :: decls) :: functors
      | e :: functors -> e :: loop functors
    in
    loop functors

  (* Recursives modules *)

  let make_functor ctxt params body =
    List.fold_right
      (fun p rhs ->
	let arg = ExprMap.find p ctxt.names
	and targ = ExprMap.find p ctxt.tnames in
	<:expr< fun (type $lid:targ$) -> fun ($lid:arg$: (module $uid:runtimename$.$uid:classname$ with type a = $lid:targ$)) -> $rhs$ >>)
      params body

  let is_default_instance decls name args =
    try
      let (_,params,_,_,_) = List.find (fun (n,_,_,_,_) -> n = name) decls in
      List.for_all2 (fun a p -> a = `Param p) args params
    with Not_found -> false

  let append_recursive_instance ctxt (name, eparams) mrec =
    let ty = `Constr ([name], eparams) in
    let mname = ExprMap.find ty ctxt.names in
    let (_,params,_,_,_ as decl) = NameMap.find name ctxt.decls in
    let subst = NameMap.fromList (List.zip (List.map fst params) eparams) in
    let mexpr = ctxt.generate ctxt decl subst in
    (String.uncapitalize mname,
     <:ctyp< (module $uid:runtimename$.$uid:classname$
                     with type a = $Untranslate.expr ty$) Lazy.t >>,
     <:expr< lazy (module $mexpr$ : $uid:runtimename$.$uid:classname$
                                    with type a = $atype_expr ctxt ty$) >>) :: mrec

  let make_rec_insts ctxt insts =
    if ESet.cardinal insts = 0 then <:str_item< >>
    else
      (* insert effective type parameter in recursive modules context *)
      let ctxt = insert_params ctxt ctxt.params in
      (* build recursives modules *)
      let rec_mods = ESet.fold (append_recursive_instance ctxt) insts [] in
      (* bind them together *)
      let bindings = List.map (fun (n,_,e) -> <:binding< $lid:n$ = $e$ >>) rec_mods in
      let rec_bindings =
	List.map (fun (n,_,_) -> <:rec_binding< $lid:n$ = $lid:n$ >>) rec_mods in
      let body = <:expr< let rec $list:bindings$ in { $list:rec_bindings$ } >> in
      (* wrap functor around them *)
      let field (n, e, _) = Ast.TyCol(_loc, Ast.TyId(_loc, Ast.IdLid(_loc, n)), e) in
      let tdecl =
	Ast.TyDcl (_loc, "t_"^ctxt.wrapper_id, List.map Untranslate.expr ctxt.params,
		   <:ctyp< { $Untranslate.unlist Untranslate.semi rec_mods field$ } >>,
		   []) in
      <:str_item<
	  module $uid:ctxt.wrapper_id$ = struct
	    type $tdecl$
	    let make = $make_functor ctxt ctxt.params body$
	  end >>

  (* Instance module *)

  let make_instance ctxt (name,params,_,_,_ as decl) =
    let params = List.map (fun p -> `Param p) params in
    let ctxt = insert_params ctxt params in
    let ctxt = { ctxt with inside = false } in
    let ty = `Constr ([name], params) in
    (* let atype = atype_expr ctxt ty in *)
    let mname, mexpr = try
      (* Module is recursive, just apply the functor. *)
      ExprMap.find ty ctxt.names,
      make_include_rec_inst ctxt ty
    with Not_found ->
      (* Module isn't recursive... *)
      Printf.sprintf "%s_%s" classname name,
      pack ctxt (ctxt.generate ctxt decl NameMap.empty) ty in
    <:str_item< module $uid:mname$ = struct
      let make = $make_functor ctxt params mexpr$
    end>>

  (* Global function *)

  let sort_freevars (fv: ExprSet.t) : expr list =
    List.sort compare (ExprSet.fold (fun v acc -> v :: acc) fv [])

  let generate generate decls =
    let decls = List.map rename_params decls in
    let decls_instances = close_decls decls in
    let sets = List.fold_left add_instances [] decls_instances in
    let sets =
      List.map (fun (insts, fv, decls) -> insts, sort_freevars fv, decls) sets in
    let generate_set (insts, params, inst_decls) =
      (* Build common context for recursives modules and public instances. *)
      let wrapper_id = Printf.sprintf "%s_Functor_%s" classname (random_id 32) in
      let names = ESet.fold
          (fun (name, args) map ->
	    let suffix =
	      if is_default_instance inst_decls name args
	      then ""
	      else "_" ^ random_id 32 in
	    ExprMap.add (`Constr ([name], args))
	      (Printf.sprintf "%s_%s%s" classname name suffix) map)
          insts ExprMap.empty in
      let decls =
	NameMap.fromList (List.map (fun (n,_,_,_,_ as decl) -> n, decl) decls) in
      let ctxt = { wrapper_id; params; names; tnames = ExprMap.empty; generate; decls;
		   inside = true; suffix = None } in
      (* Generate recursive modules... *)
      let mrec_insts = make_rec_insts ctxt insts in
      (* ... and generate "public" instance *)
      let minsts = List.map (make_instance ctxt) inst_decls in
      <:str_item< $mrec_insts$ $list:minsts$ >>
    in
    <:str_item< $list:List.map generate_set sets$ >>

  let make_sig_functor ctxt params body =
    List.fold_right
      (fun p rhs ->
	let arg = ExprMap.find p ctxt.names in
        <:ctyp<
	 (module $uid:runtimename$.$uid:classname$ with type a = '$lid:arg$) -> $rhs$ >>)
      params body

  let default_generate_sig context (name, params, _, _, _) =
    let params =
      List.map
	(fun p -> `Param (ExprMap.find (`Param p) context.names, None))
	params in
    let atype = Untranslate.expr (`Constr ([name], params)) in
    <:ctyp< (module $uid:runtimename$.$uid:classname$
                    with type a = $atype$) >>

  (* let default_generate_sig_expr context decl = *)
    (* <:ctyp< (module $uid:runtimename$.$uid:classname$ *)
                    (* with type a = $atype_expr context decl$) >> *)

  let generate_sig ctxt gen_sig (name,params,_,_,generated as decl) =
    if name = "a" then
      raise (Underivable ("deriving: types called `a' are not allowed.\n"
			  ^ "Please change the name of your type and try again."));
    if generated then <:sig_item< >>
    else
      let params = List.map (fun p -> `Param p) params in
      let ctxt = insert_params ctxt params in
      let body =
	make_sig_functor ctxt params (gen_sig ctxt decl) in
      <:sig_item<
        module $uid:Printf.sprintf "%s_%s" classname name$ : sig
	  val make: $body$
	end >>

   let generate_sigs gen_sig decls =
     <:sig_item< $list:List.map (generate_sig outside_ctxt gen_sig) decls$ >>

end

let derivers = Hashtbl.create 15
let hashtbl_add (context, _ as deriver) =
  let module Params = (val context : DeriverParams) in
  Hashtbl.add derivers Params.classname deriver
let register_hook = ref [hashtbl_add]
let add_register_hook f = register_hook := f :: !register_hook
let register context deriver =
  List.iter (fun f -> f (context, deriver)) !register_hook
let find classname =
  try Hashtbl.find derivers classname
  with Not_found -> raise (NoSuchClass classname)
let is_registered classname = Hashtbl.mem  derivers classname

let derive_str _loc (decls : Type.decl list) (context, deriver) : Ast.str_item =
  let module Loc = struct let _loc = _loc end in
  let module Params = (val context : DeriverParams) in
  let module DeriverHelpers = InContext(Loc)(Params) in
  let module Deriver = (val deriver : MakeDeriver)(DeriverHelpers) in
  DeriverHelpers.generate Deriver.generate decls

let derive_sig _loc (decls : Type.decl list) (context, deriver) : Ast.sig_item =
  let module Loc = struct let _loc = _loc end in
  let module Params = (val context : DeriverParams) in
  let module DeriverHelpers = InContext(Loc)(Params) in
  let module Deriver = (val deriver : MakeDeriver)(DeriverHelpers) in
  (* TODO define generate_sig in each modules ? (needed for Functor) *)
  DeriverHelpers.generate_sigs DeriverHelpers.default_generate_sig decls

let instantiate _loc ty deriver =
  let module Loc = struct let _loc = _loc end in
  let module U = Type.Untranslate(Loc) in
  let binding = Ast.TyDcl (_loc, "inline", [], ty, []) in
  let decls = display_errors _loc Type.Translate.decls binding in
  if List.exists Type.contains_tvars_decl decls then
    fatal_error _loc
      ("deriving: type variables cannot be used in `method' instantiations");
  let tdecls = List.map U.decl decls in
  let m = derive_str _loc decls deriver in
  let module Params = (val fst deriver : DeriverParams) in
  <:expr<
    let module M = struct
      type $list:tdecls$
      $m$
      include $uid:Params.classname ^ "_inline"$
    end in
    M.make
  >>
