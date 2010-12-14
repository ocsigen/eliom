(*pp $CAMLP4OF *)

(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

open Camlp4.PreCast
open Pa_deriving_common

open Context
open Base
open Type
open Utils

module Params = struct
  let classname = "Typeable"
  let runtimename = "Deriving_Typeable"
  let predefs = [
    ["int"], "int";
    ["bool"], "bool";
    ["unit"], "unit";
    ["char"], "char";
    (* ["int32"], "int32"; *)
    (* ["Int32";"t"], "int32"; *)
    (* ["int64"], "int64"; *)
    (* ["Int64";"t"], "int64"; *)
    (* ["nativeint"], "nativeint"; *)
    ["float"], "float";
    (* ["num"], "num"; *)
    ["string"], "string";
    ["list"], "list";
    ["ref"], "ref";
    ["option"], "option";
  ]
  let default_module = Some ("Defaults", "min")
  let allow_private = true
  let depends = []
end

module Make(H : DeriverHelpers) = struct

  open H

  let nameMap = ref StringMap.empty

  let mkName tname =
    try
      StringMap.find tname !nameMap
    with StringMap.NotFound _ ->
      let file_name, sl, _, _, _, _, _, _ = Loc.to_tuple _loc in
      let name =
	Printf.sprintf "%s_%d_%f_%s" file_name sl (Unix.gettimeofday ()) tname in
      nameMap := StringMap.add tname name !nameMap;
      name

  let wrap type_rep = [ <:str_item< let type_rep = lazy $type_rep$ >> ]

  let gen call_expr ?eq ctxt ((tname,params,_,_,_) as decl : Type.decl) subst _ =
    let paramList =
      List.fold_right
        (fun ty cdr -> <:expr< $call_expr ctxt ty "type_rep"$::$cdr$ >>)
        (List.map (fun (a, _ as p) -> try NameMap.find a subst with StringMap.NotFound  _ -> `Param p) params)
      <:expr< [] >>
    in
    wrap <:expr< $uid:runtimename$.TypeRep.mkFresh $str:mkName tname$ $paramList$ >>

  let instance = object(self)

    inherit generator

    method tuple ctxt tys =
      let params =
        expr_list (List.map (fun ty -> self#call_expr ctxt ty "type_rep") tys) in
      wrap <:expr<$uid:runtimename$.TypeRep.mkTuple $params$ >>

    method sum = gen self#call_expr
    method record = gen self#call_expr

    method variant ctxt decl subst (_,tags) =
      let tags, extends =
	List.fold_left
          (fun (tags, extends) -> function
            | Tag (l,[])  -> <:expr< ($str:l$, None) :: $tags$ >>, extends
            | Tag (l,tys) ->
		<:expr< ($str:l$, Some $self#call_expr ctxt (`Tuple tys) "type_rep"$) :: $tags$ >>,
		extends
            | Extends t ->
		tags,
		<:expr< $self#call_expr ctxt t "type_rep"$::$extends$ >>)
        (<:expr< [] >>, <:expr< [] >>) tags in
    wrap <:expr< $uid:runtimename$.TypeRep.mkPolyv $tags$ $extends$ >>

    method alpha ctxt ty =
      wrap <:expr< $uid:runtimename$.TypeRep.mkFresh $str:mkName (fst ty)$ [] >>

  end

  let generate = instance#rhs

end

let depend = (module (functor (L: Loc) -> struct
  let classname = Params.classname
  let runtimename = Params.runtimename
  let generate_expr ctxt expr =
    let module Deriver = Make(InContext(L)(Params)) in
    match expr with
    | `Param p -> Deriver.instance#wrap ctxt expr (Deriver.instance#alpha ctxt p)
    | expr -> Deriver.instance#expr ctxt expr
end) : DeriverDepends)

let _ = Base.register (module Params: DeriverParams) (module Make : MakeDeriver)
