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
  let classname = "Eq"
  let runtimename = "Deriving_Eq"

  let predefs = [
    ["unit"], "unit";
    ["bool"], "bool";
    ["char"], "char";
    ["int"], "int";
    ["int32"], "int32";
    ["Int32";"t"], "int32";
    ["int64"], "int64";
    ["Int64";"t"], "int64";
    ["nativeint"], "nativeint";
    ["float"], "float";
    (* ["num"], "num"; *)
    ["list"], "list";
    ["option"], "option";
    (* ["Map";"t"], "map_s_t"; *)
    ["string"], "string";
    ["ref"], "ref";
    ["array"], "array";
  ]
  let default_module = None
  let allow_private = true
  let depends = []
end

module Make(H : DeriverHelpers) = struct

  open H

  let lprefix = "l" and rprefix = "r"

  let wildcard_failure = <:match_case< _ -> false >>

  let wrap eq =
    [ <:str_item< let eq l r = match l, r with $list:eq$ >>]

  let instance = object (self)

    inherit generator

    method tuple ctxt tys =
      match tys with
      | [ty] ->
          wrap [ <:match_case< (l, r) -> $self#call_expr ctxt ty "eq"$ l r >> ]
      | tys ->
          let _, (lpatt, rpatt), e =
            List.fold_right
              (fun ty (n, (lpatt, rpatt), e) ->
                let lid = Printf.sprintf "l%d" n
		and rid = Printf.sprintf "r%d" n in
                (n+1,
                 (Ast.PaCom (_loc,<:patt< $lid:lid$ >>, lpatt),
                  Ast.PaCom (_loc,<:patt< $lid:rid$ >>, rpatt)),
                 <:expr< $self#call_expr ctxt ty "eq"$ $lid:lid$ $lid:rid$ && $e$ >>))
              tys
              (0, (<:patt< >>, <:patt< >>), <:expr< true >>)
          in
          wrap [ <:match_case< $Ast.PaTup(_loc, Ast.PaCom(_loc, Ast.PaTup(_loc, lpatt),Ast.PaTup(_loc, rpatt)))$ -> $e$ >> ]


    method case ctxt : Type.summand -> Ast.match_case =
      fun (name,args) ->
	match args with
        | [] -> <:match_case< ($uid:name$, $uid:name$) -> true >>
        | _ ->
            let nargs = List.length args in
            let _, lpatt, lexpr = tuple ~prefix:"l" nargs
            and _, rpatt, rexpr = tuple ~prefix:"r" nargs in
            <:match_case<
            ($uid:name$ ($lpatt$), $uid:name$ ($rpatt$)) ->
              $self#call_expr ctxt (`Tuple args) "eq"$ $lexpr$ $rexpr$ >>

    method sum ?eq ctxt decl subst summands =
      let wildcard =
	match summands with
	| [_] -> []
	| _ -> [ wildcard_failure ] in
      wrap (List.map (self#case ctxt) summands @ wildcard)

    method field ctxt : Type.field -> Ast.expr = function
      | (name, ty, `Immutable) -> <:expr<
          $self#call_poly_expr ctxt ty "eq"$
	  $lid:lprefix ^ name$ $lid:rprefix ^ name$ >>
      | (_, _, `Mutable) -> assert false

    method record ?eq ctxt decl subst fields =
      if List.exists (function (_,_,`Mutable) -> true | _ -> false) fields then
	wrap [ <:match_case< (x, y) -> x == y >> ]
      else
	let lpatt = record_pattern ~prefix:"l" fields
	and rpatt = record_pattern ~prefix:"r" fields
	and expr =
	  List.fold_right
            (fun f e -> <:expr< $self#field ctxt f$ && $e$ >>)
            fields
            <:expr< true >>
	in wrap [ <:match_case< (($lpatt$), ($rpatt$)) -> $expr$ >> ]

    method polycase ctxt : Type.tagspec -> Ast.match_case = function
      | Tag (name, []) -> <:match_case< `$name$, `$name$ -> true >>
      | Tag (name, tys) -> <:match_case<
          `$name$ l, `$name$ r ->
            $self#call_expr ctxt (`Tuple tys) "eq"$ l r >>
      | Extends ty ->
          let lpatt, lguard, lcast = cast_pattern ctxt ~param:"l" ty in
          let rpatt, rguard, rcast = cast_pattern ctxt ~param:"r" ty in
          <:match_case<
             (($lpatt$), ($rpatt$)) when $lguard$ && $rguard$ ->
                $self#call_expr ctxt ty "eq"$ $lcast$ $rcast$ >>

    method variant ctxt decl subst (spec, tags) =
      wrap (List.map (self#polycase ctxt) tags @ [wildcard_failure])

    method alpha ctxt ty = wrap [ <:match_case< _ -> assert false >> ]

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
