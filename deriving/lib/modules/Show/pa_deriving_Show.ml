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
  let classname = "Show"
  let runtimename = "Deriving_Show"
  let predefs = [
    ["int"      ], "int";
    ["bool"     ], "bool";
    ["unit"     ], "unit";
    ["char"     ], "char";
    ["int32"    ], "int32";
    ["Int32";"t"], "int32";
    ["int64"    ], "int64";
    ["Int64";"t"], "int64";
    ["nativeint"], "nativeint";
    ["float"    ], "float";
    (* ["num"], "num"; *)
    ["string"   ], "string";
    ["list"     ], "list";
    ["ref"      ], "ref";
    ["option"   ], "option";
    ["array"    ], "array";
    (* ["Map";"t"  ], "map"; *)
    (* ["Set";"t"  ], "set"; *)
    (* ["open_flag"], "open_flag"; *)
    (* ["fpclass"  ], "fpclass"; *)
  ]
  let default_module = Some ("Defaults", "min")
  let allow_private = true
  let depends = []
end

module Make(H : DeriverHelpers) : Deriver = struct

  open H

  let in_a_box box e =
    <:expr<
      Format.$lid:box$ formatter 0;
      $e$;
      Format.pp_close_box formatter () >>

  let in_hovbox = in_a_box "pp_open_hovbox" and in_box = in_a_box "pp_open_box"

  let wrap formatter =
    [ <:str_item< let format formatter = function $list:formatter$ >> ]

  let instance = object (self)

    inherit generator as super

    method nargs ctxt (exprs : (name * Type.expr) list) : Ast.expr =
      match exprs with
        | [id,t] ->
              <:expr< $self#call_expr ctxt t "format"$ formatter $lid:id$ >>
        | exprs ->
            let fmt =
              "@[<hov 1>("^ String.concat ",@;" (List.map (fun _ -> "%a") exprs) ^")@]" in
              List.fold_left
                (fun f (id, t) ->
                   <:expr< $f$ $self#call_expr ctxt t "format"$ $lid:id$ >>)
                <:expr< Format.fprintf formatter $str:fmt$ >>
                exprs

    method tuple ctxt args =
      let n = List.length args in
      let tvars, tpatt, _ = tuple n in
      wrap
	[ <:match_case< $tpatt$ ->
	    $self#nargs ctxt (List.map2 (fun v t -> v, t) tvars args)$ >>]

    method case ctxt : Type.summand -> Ast.match_case =
      fun (name, args) ->
        match args with
          | [] ->
	      <:match_case< $uid:name$ -> Format.pp_print_string formatter $str:name$ >>
          | _ ->
              let vars, patt, exp = tuple (List.length args) in
                <:match_case<
                  $uid:name$ $patt$ ->
                  $in_hovbox <:expr<
                    Format.pp_print_string formatter $str:name$;
                    Format.pp_print_break formatter 1 2;
                    $self#nargs ctxt (List.zip vars args)$ >> $ >>

    method field ctxt (name, ty, _) : Ast.expr =
      <:expr<
        Format.pp_print_string formatter $str:name ^ " ="$;
        $self#call_poly_expr ctxt ty "format"$ formatter $lid:name$
      >>

    method sum ?eq ctxt decl subst summands =
      wrap (List.map (self#case ctxt) summands)

    method record ?eq ctxt decl subst fields =
      let formatter = <:match_case<
	$record_pattern fields$ -> $in_hovbox
	    <:expr<
          Format.pp_print_char formatter '{';
          $List.fold_left1
            (fun l r -> <:expr< $l$; Format.pp_print_string formatter "; "; $r$ >>)
            (List.map (self#field ctxt) fields)$;
          Format.pp_print_char formatter '}'; >>$ >> in
      wrap [formatter]

    method polycase ctxt : Type.tagspec -> Ast.match_case = function
      | Tag (name, []) ->
          <:match_case< `$uid:name$ ->
                        Format.pp_print_string formatter $str:"`" ^ name ^" "$ >>
      | Tag (name, e) ->
          <:match_case< `$uid:name$ x ->
                         $in_hovbox <:expr<
                            Format.pp_print_string formatter $str:"`" ^ name ^" "$;
                            $self#call_expr ctxt (`Tuple e) "format"$ formatter x >>$ >>
      | Extends t ->
          let patt, guard, cast = cast_pattern ctxt t in
            <:match_case<
              $patt$ when $guard$ ->
              $in_hovbox <:expr< $self#call_expr ctxt t "format"$ formatter $cast$ >>$ >>

    method variant ctxt decl subst (_,tags) =
      let formatters =
	List.map (self#polycase ctxt) tags @ [ <:match_case< _ -> assert false >> ] in
      wrap formatters

    method alpha ctxt ty = [ <:str_item< let format _ _ = assert false >> ]


  end

  let generate = instance#rhs

end

let _ = Base.register (module Params: DeriverParams) (module Make : MakeDeriver)
